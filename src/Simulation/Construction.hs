{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Simulation.Construction ( handleFactionConstruction, queueCostReq, planetConstructionSpeed
                               , overallConstructionSpeed )
    where

import Import
import qualified Queries (planetConstructionQueue)
import Resources ( RawResources(..), RawResource(..), ResourceCost, ConstructionSpeed
                 , ResourcesAvailable )
import Common (safeHead)
import Construction ( Constructable(..), constructionLeft, ConstructionSpeedCoeff(..)
                    , OverallConstructionSpeed(..), speedLimitedByOverallSpeed, resourceScaledBySpeed
                    , constructionWillFinish, speedLimitedByWorkLeft )
import CustomTypes ( StarDate )
import MenuHelpers (getScore)
import Buildings (BuildingInfo(..), BLevel(..), building)
import News.Import (buildingConstructionFinishedNews)
import Data.Maybe (fromJust)


-- | Process through all construction queues of a faction and update them
--   New buildings will be constructed when applicable, resources are spent
--   and overall resource consumption is taken into account when selecting the speed
--   of the construction
handleFactionConstruction :: (BaseBackend backend ~ SqlBackend,
                              BackendCompatible SqlBackend backend,
                              PersistQueryWrite backend,
                              PersistStoreWrite backend, PersistQueryRead backend, PersistUniqueRead backend,
                              MonadIO m) =>
                              StarDate -> Entity Faction -> ReaderT backend m ()
handleFactionConstruction date factionE = do
    let faction = entityVal factionE
    planets <- selectList [ PlanetOwnerId ==. Just (entityKey factionE)] []
    queues <- mapM (Queries.planetConstructionQueue . entityKey) planets
    let totalCost = mconcat $ map (queueCostReq . toPlainObjects) queues
    let availableResources = getScore $ Just faction
    let consSpeed = overallConstructionSpeed totalCost availableResources
    _ <- updateWhere [ FactionId ==. entityKey factionE ]
                     [ FactionBiologicals -=. resourceScaledBySpeed (ccdBiologicalCost totalCost) (overallSpeedBiological consSpeed)
                     , FactionMechanicals -=. resourceScaledBySpeed (ccdMechanicalCost totalCost) (overallSpeedMechanical consSpeed)
                     , FactionChemicals -=. resourceScaledBySpeed (ccdChemicalCost totalCost) (overallSpeedChemical consSpeed)
                     ]
    mapM_ (doPlanetConstruction (entityKey factionE) date consSpeed . planetAndFirstConstruction) queues


-- | Select construction from queue with the smallest construction index
planetAndFirstConstruction :: (Maybe (Entity Planet), [Entity BuildingConstruction]) -> (Maybe (Entity Planet), Maybe (Entity BuildingConstruction))
planetAndFirstConstruction (planet, queue@(_:_)) =
    (planet, safeHead sortedQ)
    where
        sortedQ = sortBy sorter queue
        sorter a b = compare ((cIndex . entityVal) a) ((cIndex . entityVal) b)

planetAndFirstConstruction (planet, []) =
    (planet, Nothing)


-- | Perform construction on a planet at given speed
doPlanetConstruction :: (PersistQueryRead backend, PersistQueryWrite backend,
                        MonadIO m, BaseBackend backend ~ SqlBackend) =>
                        FactionId -> StarDate -> OverallConstructionSpeed -> (Maybe (Entity Planet), Maybe (Entity BuildingConstruction))
                        -> ReaderT backend m ()
doPlanetConstruction fId date speed (Just planetE, Just bConsE) = do
    let bCons = entityVal bConsE
    let realSpeed = speedLimitedByOverallSpeed speed $ planetConstructionSpeed $ entityVal planetE
    let modelBuilding = building (buildingConstructionType bCons) (BLevel $ buildingConstructionLevel bCons)
    let cToDo = speedLimitedByWorkLeft realSpeed bCons (buildingInfoCost modelBuilding)
    _ <- if constructionWillFinish realSpeed (cProgress bCons) (buildingInfoCost modelBuilding)
         then finishConstruction fId date bConsE
         else workOnConstruction cToDo bConsE
    return ()
doPlanetConstruction _ _ _ _ =
    return ()


-- | Finish construction by removing it from construction queue, updating indecies and
--   placing the newly constructed building on planet. Respective reports and news entries
---  are created for the faction.
finishConstruction :: (PersistQueryRead backend, PersistQueryWrite backend,
                       MonadIO m, BaseBackend backend ~ SqlBackend) =>
                       FactionId -> StarDate -> Entity BuildingConstruction -> ReaderT backend m ()
finishConstruction fId date bConsE = do
    let bCons = entityVal bConsE
    let bConsId = entityKey bConsE
    let planetId = buildingConstructionPlanetId bCons
    _ <- delete bConsId
    _ <- updateWhere [ BuildingConstructionPlanetId ==. planetId
                     , BuildingConstructionId !=. bConsId ] [ BuildingConstructionIndex -=. 1 ]
    _ <- updateWhere [ ShipConstructionPlanetId ==. Just planetId ] [ ShipConstructionIndex -=. 1 ]
    let newBuilding = Building { buildingPlanetId = planetId
                               , buildingType = buildingConstructionType bCons
                               , buildingLevel = buildingConstructionLevel bCons
                               , buildingDamage = 0.0
                               }
    bId <- insert newBuilding
    let report = BuildingReport { buildingReportBuildingId = bId
                                , buildingReportPlanetId = planetId
                                , buildingReportType = Just $ buildingConstructionType bCons
                                , buildingReportLevel = Just $ buildingConstructionLevel bCons
                                , buildingReportDamage = Just 0.0
                                , buildingReportFactionId = fId
                                , buildingReportDate = date
                                }
    _ <- insert report
    -- TODO: rather messy piece, clean up this
    planet <- get planetId
    let starSystemId = fmap planetStarSystemId planet :: Maybe StarSystemId
    starSystem <- mapM get starSystemId
    let news = buildingConstructionFinishedNews (Entity planetId $ fromJust planet) (Entity (fromJust starSystemId) (fromJust $ fromJust starSystem) )
                                                (Entity bId newBuilding) date fId
    _ <- insert news
    return ()


-- | Update construction in queue by increasing its progress fields by given speed
workOnConstruction :: (PersistQueryWrite backend,
                      BaseBackend backend ~ SqlBackend, MonadIO m) =>
                      RawResources ConstructionSpeed -> Entity BuildingConstruction -> ReaderT backend m ()
workOnConstruction speed bConsE = do
    let bConsId = entityKey bConsE
    _ <- update bConsId [ BuildingConstructionProgressBiologicals +=. unRawResource (ccdBiologicalCost speed)
                        , BuildingConstructionProgressMechanicals +=. unRawResource (ccdMechanicalCost speed)
                        , BuildingConstructionProgressChemicals +=. unRawResource (ccdChemicalCost speed) ]
    return ()


-- | Turn entities into plain objects
toPlainObjects :: (Maybe (Entity Planet), [Entity BuildingConstruction]) -> (Maybe Planet, [BuildingConstruction])
toPlainObjects (planet, constructions) =
    (entityVal <$> planet, map entityVal constructions)


-- | Total requirement of cost for a construction queue for a turn
--   Take into account speed the planet can construct buildings
queueCostReq :: (Maybe Planet, [BuildingConstruction]) -> RawResources ResourceCost
queueCostReq (Just planet, construction:_) =
    RawResources (min (ccdMechanicalCost planetSpeed) (ccdMechanicalCost constLeft))
                 (min (ccdBiologicalCost planetSpeed) (ccdBiologicalCost constLeft))
                 (min (ccdChemicalCost planetSpeed) (ccdChemicalCost constLeft))
    where
        modelBuilding = building (buildingConstructionType construction)
                                 (BLevel $ buildingConstructionLevel construction)
        planetSpeed = planetConstructionSpeed planet
        constLeft = constructionLeft (buildingInfoCost modelBuilding)
                                     (cProgress construction)
queueCostReq (_, _) = mempty


-- | Speed that a planet can build constructions
planetConstructionSpeed :: Planet -> RawResources ConstructionSpeed
planetConstructionSpeed _ =
    RawResources (RawResource 50) (RawResource 50) (RawResource 50)


-- | Overall speed coefficient with given total cost and total resources
--   Used to scale all construction of a faction, so they don't end up using
--   more resources than they have
overallConstructionSpeed :: RawResources ResourceCost -> RawResources ResourcesAvailable -> OverallConstructionSpeed
overallConstructionSpeed cost res =
    OverallConstructionSpeed
        { overallSpeedBiological = bioSpeed
        , overallSpeedMechanical = mechSpeed
        , overallSpeedChemical = chemSpeed
        }
    where
        bioSpeed = speedPerResource (ccdBiologicalCost cost) (ccdBiologicalCost res)
        mechSpeed = speedPerResource (ccdMechanicalCost cost) (ccdMechanicalCost res)
        chemSpeed = speedPerResource (ccdChemicalCost cost) (ccdChemicalCost res)


-- | Speed that consumes at most available amount of resources or finishes the construction
speedPerResource :: RawResource t -> RawResource t -> ConstructionSpeedCoeff t
speedPerResource cost res =
    if res >= cost
    then NormalConstructionSpeed
    else LimitedConstructionSpeed $ fromIntegral (unRawResource res) / fromIntegral (unRawResource cost)
