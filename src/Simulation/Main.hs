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

module Simulation.Main where

import Import
import Simulation.Time
import Simulation.Food (handleFactionFood)
import Simulation.Observations (handleFactionObservations)

-- | simulate a single step
processTurn :: (BaseBackend backend ~ SqlBackend,
    PersistQueryRead backend, PersistStoreWrite backend, MonadIO m) =>
    ReaderT backend m (Time)
processTurn = do
    newTime <- advanceTime
    factions <- selectList [] [ Asc FactionId ]
    _ <- mapM handleFaction factions
    return (newTime)

-- | process single faction, handling all of it's needs, orders and such
handleFaction :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, MonadIO m, PersistQueryRead backend) =>
    Entity Faction -> ReaderT backend m ()
handleFaction faction = do
    _ <- handleFactionFood faction
    _ <- handleFactionObservations faction
    return ()
