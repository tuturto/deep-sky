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
    bios <- mapM getFoodRequirement $ map entityKey planets
    let reqBio = foldr (+) 0 bios
    _ <- update (entityKey faction) [ FactionBiologicals -=. reqBio ]
    return ()

-- | calculate amount of food a given planet requires
getFoodRequirement :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistQueryRead backend) =>
    Key Planet -> ReaderT backend m Int
getFoodRequirement pid = do
    pop <- selectList [ PlanetPopulationPlanetId ==. pid ] []
    let res = pFoodRequirement $ map entityVal pop
    return res

-- | calculate amount of food given population requires
pFoodRequirement :: [PlanetPopulation] -> Int
pFoodRequirement population = 
    totalPopulation * 2
        where totalPopulation = foldr (\a b -> planetPopulationPopulation a + b) 0 population
