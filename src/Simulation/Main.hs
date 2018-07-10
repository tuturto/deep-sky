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
import System.Random

-- | simulate a single step
processTurn :: (BaseBackend backend ~ SqlBackend,
    PersistQueryRead backend, PersistStoreWrite backend, MonadIO m) =>
    ReaderT backend m (Time)
processTurn = do
    newTime <- advanceTime
    rng <- liftIO $ getStdGen
    factions <- selectList [] [ Asc FactionId ]
    _ <- mapM handleFaction factions
    return (newTime)

-- | process single faction, handling all of it's needs, orders and such
handleFaction :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, MonadIO m, PersistQueryRead backend) =>
    Entity Faction -> ReaderT backend m ()
handleFaction faction = do
    planets <- selectList [ PlanetOwnerId ==. Just (entityKey faction)] []
    let reqBio = foodRequirement $ map entityVal planets
    _ <- update (entityKey faction) [ FactionBiologicals -=. reqBio ]
    return ()

-- | amount of food a planet requires to sustain itself
foodRequirement :: [Planet] -> Int
foodRequirement planets = length planets