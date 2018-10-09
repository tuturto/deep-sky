{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Simulation.Food ( handleFactionFood, getFoodRequirement, getFoodProduction, foodRequirement
                       , foodProduction )
    where

import Import
import CustomTypes

-- | handle production and consumption of food for given faction
handleFactionFood :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Entity Faction -> ReaderT backend m ()
handleFactionFood faction = do
    planets <- selectList [ PlanetOwnerId ==. Just (entityKey faction)] []
    lReqBio <- mapM (getFoodRequirement . entityKey) planets
    let reqBio = foldl' (+) 0 lReqBio
    lProdBio <- mapM (getFoodProduction . entityKey) planets
    let prodBio = foldl' (+) 0 lProdBio
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
    foldl' (+) 0 productions 
        where productions = map prod buildings
              prod x = case buildingType x of
                            Farm -> 5
                            _    -> 0
