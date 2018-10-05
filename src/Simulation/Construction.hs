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
import Queries (loadPlanetConstructionQueue)
import CustomTypes (TotalCost(..), Cost(..))


handleFactionConstruction :: (BaseBackend backend ~ SqlBackend,
                              BackendCompatible SqlBackend backend,
                              PersistStoreWrite backend, PersistQueryRead backend, PersistUniqueRead backend,
                              MonadIO m) =>
                             Time -> Entity Faction -> ReaderT backend m ()
handleFactionConstruction date faction = do
    planets <- selectList [ PlanetOwnerId ==. Just (entityKey faction)] []
    queues <- mapM loadPlanetConstructionQueue $ map entityKey planets
    let totalCost = mconcat $ map queueCost queues
    return ()

queueCost (planet, bConstructions) =
    TotalCost (Cost 0) (Cost 0) (Cost 0)
