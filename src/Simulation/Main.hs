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
import CustomTypes
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
    lReqBio <- mapM getFoodRequirement $ map entityKey planets
    let reqBio = foldr (+) 0 lReqBio
    lProdBio <- mapM getFoodProduction $ map entityKey planets
    let prodBio = foldr (+) 0 lProdBio
    let deltaBio = prodBio - reqBio
    _ <- update (entityKey faction) [ FactionBiologicals +=. deltaBio ]
    return ()

-- | calculate amount of food a given planet requires
getFoodRequirement :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistQueryRead backend) =>
    Key Planet -> ReaderT backend m Int
getFoodRequirement pid = do
    pop <- selectList [ PlanetPopulationPlanetId ==. pid ] []
    let res = foodRequirement $ map entityVal pop
    return res

-- | calculate amount of food a given planet produces
getFoodProduction :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistQueryRead backend) =>
    Key Planet -> ReaderT backend m Int
getFoodProduction pid = do
    buildings <- selectList [ BuildingPlanetId ==. pid ] []
    let res = foodProduction $ map entityVal buildings
    return res

-- | calculate amount of food given population requires
foodRequirement :: [PlanetPopulation] -> Int
foodRequirement population = 
    totalPopulation * 2
        where totalPopulation = foldr (\a b -> planetPopulationPopulation a + b) 0 population

-- | calculate amount of food produced by group of buildings
foodProduction :: [Building] -> Int
foodProduction buildings =
    foldr (+) 0 productions 
        where productions = map prod buildings
              prod x = case (buildingType x) of
                            Farm -> 5
                            _    -> 0
