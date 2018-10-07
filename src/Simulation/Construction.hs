{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Simulation.Construction ( handleFactionConstruction, queueCostReq, planetConstructionSpeed )
    where

import Import
import qualified Queries (planetConstructionQueue)
import CustomTypes (TotalCost(..), Cost(..), TotalResources(..), subTotalCost)
import Common (safeHead)
import Construction (ConstructionSpeed(..), Constructable(..))
import MenuHelpers (getScore)
import qualified Prelude as P (minimum)
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
    queues <- mapM Queries.planetConstructionQueue $ map entityKey planets
    let totalCost = mconcat $ map (queueCostReq . toPlainObjects) queues
    let availableResources = getScore $ Just faction
    let consSpeed = overallConstructionSpeed totalCost availableResources
    -- TODO: select building with smallest index (should be 0)
    _ <- mapM (doPlanetConstruction consSpeed) $ map (\(planet, queue) -> (planet, safeHead queue)) queues
    return ()

-- | Perform construction on a planet at given speed
doPlanetConstruction :: (PersistQueryRead backend, PersistQueryWrite backend,
                        MonadIO m, BaseBackend backend ~ SqlBackend) =>
                        OverallConstructionSpeed -> (Maybe (Entity Planet), Maybe (Entity BuildingConstruction)) -> ReaderT backend m ()
doPlanetConstruction speed ((Just planetE), (Just bConsE)) = do
    -- will construction be finished
    --   no? -> update construction left
    --   yes? -> remove construction from queue, updating indexes
    --           place new building on planet
    --           make news entry about finished construction
    let planet = entityVal planetE
    let bCons = entityVal bConsE
    let realSpeed = applyOverallSpeed speed $ planetConstructionSpeed planet
    let modelBuilding = building (buildingConstructionType bCons) (BLevel $ buildingConstructionLevel bCons)
    _ <- if constructionWillFinish realSpeed (cProgress bCons) (buildingInfoCost modelBuilding)
         then finishConstruction bConsE
         else workOnConstruction realSpeed bConsE
    return ()
doPlanetConstruction _ _ = do
    return ()

-- | Will construction finish based on speed, progress so far and required construction
constructionWillFinish :: ConstructionSpeed -> TotalCost -> TotalCost -> Bool
constructionWillFinish speed progress total = 
    -- TODO: implement
    False

finishConstruction :: (PersistQueryRead backend, PersistQueryWrite backend,
                       MonadIO m, BaseBackend backend ~ SqlBackend) =>
                       Entity BuildingConstruction -> ReaderT backend m ()
finishConstruction bConsE = do
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
    -- TODO: news entry
    return ()

workOnConstruction :: (PersistQueryWrite backend, 
                      BaseBackend backend ~ SqlBackend, MonadIO m) => 
                      ConstructionSpeed -> Entity BuildingConstruction -> ReaderT backend m ()
workOnConstruction speed bConsE = do
    let bCons = entityVal bConsE
    let bConsId = entityKey bConsE
    _ <- update bConsId [ BuildingConstructionProgressBiologicals +=. (unCost $ constructionSpeedBiologicalCost speed)
                        , BuildingConstructionProgressMechanicals +=. (unCost $ constructionSpeedMechanicalCost speed)
                        , BuildingConstructionProgressChemicals +=. (unCost $ constructionSpeedChemicalCost speed) ]
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
queueCostReq ((Just planet), (construction:_)) = 
    TotalCost (min (constructionSpeedMechanicalCost planetSpeed) (ccdMechanicalCost constLeft)) 
              (min (constructionSpeedBiologicalCost planetSpeed) (ccdBiologicalCost constLeft))
              (min (constructionSpeedChemicalCost planetSpeed) (ccdChemicalCost constLeft))
    where
        modelBuilding = building (buildingConstructionType construction) 
                                 (BLevel $ buildingConstructionLevel construction)
        planetSpeed = planetConstructionSpeed $ planet
        constLeft = subTotalCost (buildingInfoCost modelBuilding)
                                 (cProgress construction)
queueCostReq (_, _) = mempty

-- | Speed that a planet can build constructions
planetConstructionSpeed :: Planet -> ConstructionSpeed
planetConstructionSpeed _ =
    ConstructionSpeed (Cost 50) (Cost 50) (Cost 50)

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

speedPerResource :: Cost -> Cost -> ConstructionSpeedCoeff
speedPerResource cost res =
    if res >= cost
    then NormalConstructionSpeed
    else LimitedConstructionSpeed $ (fromIntegral $ unCost res) / (fromIntegral $ unCost cost)

data ConstructionSpeedCoeff = NormalConstructionSpeed
    | LimitedConstructionSpeed Double
    deriving (Show, Read, Eq)

instance Ord ConstructionSpeedCoeff where
    (<=) (LimitedConstructionSpeed a) NormalConstructionSpeed =
        a <= 1.0
    (<=) (LimitedConstructionSpeed a) (LimitedConstructionSpeed b) =
        a <= b
    (<=) NormalConstructionSpeed NormalConstructionSpeed = True
    (<=) NormalConstructionSpeed (LimitedConstructionSpeed a) =
        a >= 1.0

data OverallConstructionSpeed = OverallConstructionSpeed
    { overallSpeedBiological :: ConstructionSpeedCoeff
    , overallSpeedMechanical :: ConstructionSpeedCoeff
    , overallSpeedChemical :: ConstructionSpeedCoeff
    }
    deriving (Show, Read, Eq)
