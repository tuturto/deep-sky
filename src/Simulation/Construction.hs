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
import CustomTypes (TotalCost(..), Cost(..), TotalResources(..))
import Common (safeHead)
import Construction (ConstructionSpeed(..), Constructable(..))
import MenuHelpers (getScore)
import qualified Prelude as P (minimum)


handleFactionConstruction :: (BaseBackend backend ~ SqlBackend,
                              BackendCompatible SqlBackend backend,
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
    return ()

-- | Turn entities into plain objects
toPlainObjects :: (Maybe (Entity Planet), [Entity BuildingConstruction]) -> (Maybe Planet, [BuildingConstruction])
toPlainObjects (planet, constructions) =
    (entityVal <$> planet, map entityVal constructions)

-- | Total requirement of cost for a construction queue for a turn
--   Take into account speed the planet can construct buildings
queueCostReq :: (Maybe Planet, [BuildingConstruction]) -> TotalCost
queueCostReq ((Just planet), bConstructions) = 
    TotalCost (min (constructionSpeedMechanicalCost planetSpeed) (ccdMechanicalCost constLeft)) 
              (min (constructionSpeedBiologicalCost planetSpeed) (ccdBiologicalCost constLeft))
              (min (constructionSpeedChemicalCost planetSpeed) (ccdChemicalCost constLeft))
    where
        planetSpeed = planetConstructionSpeed $ planet
        constLeft = case cCost <$> safeHead bConstructions of
                        Just x -> x
                        Nothing -> mempty
queueCostReq (Nothing, _) = mempty

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
