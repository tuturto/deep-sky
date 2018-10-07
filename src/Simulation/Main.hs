{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Simulation.Main (processTurn)
    where

import Import
import Simulation.Time
import Simulation.Food (handleFactionFood)
import Simulation.Observations (handleFactionObservations)
import Simulation.Construction (handleFactionConstruction)

-- | simulate a single step
processTurn :: (BaseBackend backend ~ SqlBackend,
    BackendCompatible SqlBackend backend, PersistUniqueRead backend,
    PersistQueryWrite backend,
    PersistQueryRead backend, PersistStoreWrite backend, MonadIO m) =>
    ReaderT backend m (Time)
processTurn = do
    newTime <- advanceTime
    factions <- selectList [] [ Asc FactionId ]
    _ <- mapM (handleFaction newTime) factions
    return (newTime)

-- | process single faction, handling all of it's needs, orders and such
handleFaction :: (BaseBackend backend ~ SqlBackend,
    BackendCompatible SqlBackend backend, PersistUniqueRead backend,
    PersistQueryWrite backend, PersistStoreWrite backend, 
    MonadIO m, PersistQueryRead backend) =>
    Time -> Entity Faction -> ReaderT backend m ()
handleFaction date faction = do
    _ <- handleFactionFood faction
    _ <- handleFactionObservations date faction
    _ <- handleFactionConstruction date faction
    return ()
