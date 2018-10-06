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
import CustomTypes (TotalCost(..), Cost(..))
import Common (safeHead)
import Construction (ConstructionSpeed(..), Constructable(..))
import MenuHelpers (getScore)


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
    -- TODO: real type
    let availableResources = getScore $ Just faction
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
