{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Simulation.Construction ( handleFactionConstruction, queueCostReq, planetConstructionSpeed )
    where

import Import
import qualified Queries (planetConstructionQueue)
import CustomTypes (TotalCost(..), RawResource(..), TotalResources(..), subTotalCost, Biological, Mechanical, Chemical)
import Common (safeHead)
import Construction (ConstructionSpeed(..), Constructable(..))
import MenuHelpers (getScore)
import Buildings (BuildingInfo(..), BLevel(..), building)


handleFactionConstruction :: (BaseBackend backend ~ SqlBackend,
                              BackendCompatible SqlBackend backend,
                              PersistQueryWrite backend,
                              PersistStoreWrite backend, PersistQueryRead backend, PersistUniqueRead backend,
                              MonadIO m) =>
                             Time -> Entity Faction -> ReaderT backend m ()
handleFactionConstruction date factionE = do
    let faction = entityVal factionE
    planets <- selectList [ PlanetOwnerId ==. Just (entityKey factionE)] []
    queues <- mapM (Queries.planetConstructionQueue . entityKey) planets
    let totalCost = mconcat $ map (queueCostReq . toPlainObjects) queues
    let availableResources = getScore $ Just faction
    let consSpeed = overallConstructionSpeed totalCost availableResources
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
                        Key Faction -> Time -> OverallConstructionSpeed -> (Maybe (Entity Planet), Maybe (Entity BuildingConstruction)) 
                            -> ReaderT backend m ()
doPlanetConstruction fId date speed (Just planetE, Just bConsE) = do
    let bCons = entityVal bConsE
    let realSpeed = applyOverallSpeed speed $ planetConstructionSpeed $ entityVal planetE
    let modelBuilding = building (buildingConstructionType bCons) (BLevel $ buildingConstructionLevel bCons)
    let cToDo = limitConstructionSpeed realSpeed bCons (buildingInfoCost modelBuilding)
    _ <- if constructionWillFinish realSpeed (cProgress bCons) (buildingInfoCost modelBuilding)
         then finishConstruction fId date bConsE
         else workOnConstruction cToDo bConsE
    return ()
doPlanetConstruction _ _ _ _ = 
    return ()

-- | Limit construction speed to amount that there's work left to do
limitConstructionSpeed :: ConstructionSpeed -> BuildingConstruction -> TotalCost -> ConstructionSpeed
limitConstructionSpeed cSpeed bConst cost =
    let
        bioSpeed =  if (RawResource $ buildingConstructionProgressBiologicals bConst) + constructionSpeedBiologicalCost cSpeed > ccdBiologicalCost cost
                    then ccdBiologicalCost cost - (RawResource $ buildingConstructionProgressBiologicals bConst)
                    else (RawResource $ buildingConstructionProgressBiologicals bConst) + constructionSpeedBiologicalCost cSpeed
        mechSpeed = if (RawResource $ buildingConstructionProgressMechanicals bConst) + constructionSpeedMechanicalCost cSpeed > ccdMechanicalCost cost
                    then ccdMechanicalCost cost - (RawResource $ buildingConstructionProgressMechanicals bConst)
                    else (RawResource $ buildingConstructionProgressMechanicals bConst) + constructionSpeedMechanicalCost cSpeed
        chemSpeed = if (RawResource $ buildingConstructionProgressChemicals bConst) + constructionSpeedChemicalCost cSpeed > ccdChemicalCost cost
                    then ccdChemicalCost cost - (RawResource $ buildingConstructionProgressChemicals bConst)
                    else (RawResource $ buildingConstructionProgressChemicals bConst) + constructionSpeedChemicalCost cSpeed
    in
        ConstructionSpeed { 
              constructionSpeedMechanicalCost = mechSpeed
            , constructionSpeedBiologicalCost = bioSpeed
            , constructionSpeedChemicalCost = chemSpeed
            }

-- | Will construction finish based on speed, progress so far and required construction
constructionWillFinish :: ConstructionSpeed -> TotalCost -> TotalCost -> Bool
constructionWillFinish speed progress total = 
    constructionSpeedMechanicalCost speed >= ccdMechanicalCost total - ccdMechanicalCost progress 
    && constructionSpeedBiologicalCost speed >= ccdBiologicalCost total - ccdBiologicalCost progress
    && constructionSpeedChemicalCost speed >= ccdChemicalCost total - ccdChemicalCost progress

finishConstruction :: (PersistQueryRead backend, PersistQueryWrite backend,
                       MonadIO m, BaseBackend backend ~ SqlBackend) =>
                       Key Faction -> Time -> Entity BuildingConstruction -> ReaderT backend m ()
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
                                , buildingReportDate = timeCurrentTime date
                                }
    _ <- insert report
    -- TODO: news entry
    return ()

workOnConstruction :: (PersistQueryWrite backend, 
                      BaseBackend backend ~ SqlBackend, MonadIO m) => 
                      ConstructionSpeed -> Entity BuildingConstruction -> ReaderT backend m ()
workOnConstruction speed bConsE = do
    let bConsId = entityKey bConsE
    _ <- update bConsId [ BuildingConstructionProgressBiologicals +=. unRawResource (constructionSpeedBiologicalCost speed)
                        , BuildingConstructionProgressMechanicals +=. unRawResource (constructionSpeedMechanicalCost speed)
                        , BuildingConstructionProgressChemicals +=. unRawResource (constructionSpeedChemicalCost speed) ]
    return ()

applyOverallSpeed :: OverallConstructionSpeed -> ConstructionSpeed -> ConstructionSpeed
applyOverallSpeed coeffSpeed speed =
    -- TODO: implement
    speed

-- | Turn entities into plain objects
toPlainObjects :: (Maybe (Entity Planet), [Entity BuildingConstruction]) -> (Maybe Planet, [BuildingConstruction])
toPlainObjects (planet, constructions) =
    (entityVal <$> planet, map entityVal constructions)

-- | Total requirement of cost for a construction queue for a turn
--   Take into account speed the planet can construct buildings
queueCostReq :: (Maybe Planet, [BuildingConstruction]) -> TotalCost
queueCostReq (Just planet, construction:_) = 
    TotalCost (min (constructionSpeedMechanicalCost planetSpeed) (ccdMechanicalCost constLeft)) 
              (min (constructionSpeedBiologicalCost planetSpeed) (ccdBiologicalCost constLeft))
              (min (constructionSpeedChemicalCost planetSpeed) (ccdChemicalCost constLeft))
    where
        modelBuilding = building (buildingConstructionType construction) 
                                 (BLevel $ buildingConstructionLevel construction)
        planetSpeed = planetConstructionSpeed planet
        constLeft = subTotalCost (buildingInfoCost modelBuilding)
                                 (cProgress construction)
queueCostReq (_, _) = mempty

-- | Speed that a planet can build constructions
planetConstructionSpeed :: Planet -> ConstructionSpeed
planetConstructionSpeed _ =
    ConstructionSpeed (RawResource 50) (RawResource 50) (RawResource 50)

-- | Overall speed coefficient with given total cost and total resources
--   Used to scale all construction of a faction, so they don't end up using
--   more resources than they have
overallConstructionSpeed :: TotalCost -> TotalResources -> OverallConstructionSpeed
overallConstructionSpeed cost res =
    OverallConstructionSpeed
        { overallSpeedBiological = bioSpeed
        , overallSpeedMechanical = mechSpeed
        , overallSpeedChemical = chemSpeed
        }
    where
        bioSpeed = speedPerResource (ccdBiologicalCost cost) (totalResourcesBiological res)
        mechSpeed = speedPerResource (ccdMechanicalCost cost) (totalResourcesMechanical res)
        chemSpeed = speedPerResource (ccdChemicalCost cost) (totalResourcesChemical res)

-- | Speed that consumes at most available amount of resources or finishes the construction
speedPerResource :: RawResource t -> RawResource t -> ConstructionSpeedCoeff t
speedPerResource cost res =
    if res >= cost
    then NormalConstructionSpeed
    else LimitedConstructionSpeed $ fromIntegral (unRawResource res) / fromIntegral (unRawResource cost)

data ConstructionSpeedCoeff t = NormalConstructionSpeed
    | LimitedConstructionSpeed Double
    deriving (Show, Read, Eq)

instance Ord (ConstructionSpeedCoeff t) where
    (<=) (LimitedConstructionSpeed a) NormalConstructionSpeed =
        a <= 1.0
    (<=) (LimitedConstructionSpeed a) (LimitedConstructionSpeed b) =
        a <= b
    (<=) NormalConstructionSpeed NormalConstructionSpeed = True
    (<=) NormalConstructionSpeed (LimitedConstructionSpeed a) =
        a >= 1.0

data OverallConstructionSpeed = OverallConstructionSpeed
    { overallSpeedBiological :: ConstructionSpeedCoeff Biological
    , overallSpeedMechanical :: ConstructionSpeedCoeff Mechanical
    , overallSpeedChemical :: ConstructionSpeedCoeff Chemical
    }
    deriving (Show, Read, Eq)
