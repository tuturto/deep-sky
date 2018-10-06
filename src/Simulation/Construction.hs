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

module Simulation.Construction ( handleFactionConstruction )
    where

import Import
import qualified Database.Esqueleto as E
import qualified Queries (planetConstructionQueue)
import CustomTypes (TotalCost(..), Cost(..))
import Common (maybeGet, safeHead)
import Construction


handleFactionConstruction :: (BaseBackend backend ~ SqlBackend,
                              BackendCompatible SqlBackend backend,
                              PersistStoreWrite backend, PersistQueryRead backend, PersistUniqueRead backend,
                              MonadIO m) =>
                             Time -> Entity Faction -> ReaderT backend m ()
handleFactionConstruction date faction = do
    planets <- selectList [ PlanetOwnerId ==. Just (entityKey faction)] []
    queues <- mapM Queries.planetConstructionQueue $ map entityKey planets
    let totalCost = mconcat $ map queueCostReq queues
    return ()

-- | Total requirement of cost for a construction queue for a turn
--   Take into account speed the planet can construct buildings
queueCostReq :: (Maybe (Entity Planet), [Entity BuildingConstruction]) -> TotalCost
queueCostReq (planet, bConstructions) =    
    TotalCost (Cost 0) (Cost 0) (Cost 0)
    where
        maxSpeed = planetConstructionSpeed . entityVal <$> planet
        constLeft = cCost . entityVal <$> safeHead bConstructions

-- | Speed that a planet can build constructions
planetConstructionSpeed :: Planet -> TotalCost
planetConstructionSpeed planet =
    TotalCost (Cost 50) (Cost 50) (Cost 50)
