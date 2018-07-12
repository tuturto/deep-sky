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
import System.Random
import MenuHelpers

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
    _ <- handleFactionFood faction
    _ <- handleFactionObservations faction
    return ()

-- | Generate reports for all kinds of things faction can currently observe
handleFactionObservations :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend,
    MonadIO m) =>
    Entity Faction -> ReaderT backend m ()
handleFactionObservations faction = do
    -- observations by forces on planet
    -- observations of space by planetary sensor arrays (SensorStation)
    -- observations of by space forces
    factionPlanets <- selectList [ PlanetOwnerId ==. Just (entityKey faction)] []    
    populations <- selectList [ PlanetPopulationPlanetId <-. map entityKey factionPlanets 
                              , PlanetPopulationPopulation >. 0 ] []
    let populatedPlanets = filter isPopulated factionPlanets
                            where isPopulated planet = elem (entityKey planet)
                                    (map (planetPopulationPlanetId . entityVal) populations)
    _ <- mapM (doPlanetObservation faction) populatedPlanets
    return ()

-- | Let forces on populated planet observe their surroundings and write reports
doPlanetObservation :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Entity Faction -> Entity Planet -> ReaderT backend m ()
doPlanetObservation faction planet = do
    time <- starDate
    let p = entityVal planet
    let observation = PlanetReport (entityKey planet) (Nothing) (planetStarSystemId p)
                                   (Just $ planetName p) (Just $ planetPosition p) (Just $ planetGravity p)
                                   (entityKey faction) (timeCurrentTime time)
    _ <- insert observation
    pops <- selectList [ PlanetPopulationPlanetId ==. (entityKey planet)] []
    let popObservations = map (\pop -> PlanetPopulationReport (entityKey planet) (Just $ planetPopulationRaceId $ entityVal pop)
                                                              (Just $ planetPopulationPopulation $ entityVal pop)
                                                              (entityKey faction) (timeCurrentTime time)) pops
    _ <- mapM insert popObservations
    buildings <- selectList [ BuildingPlanetId ==. (entityKey planet)] []
    let bObservations = map (\b -> BuildingReport (entityKey b) (entityKey planet) (Just $ buildingType (entityVal b))
                                                  (Just $ buildingLevel (entityVal b)) (Just $ buildingConstruction (entityVal b))
                                                  (Just $ buildingDamage (entityVal b))
                                                  (entityKey faction) (timeCurrentTime time)) buildings
    _ <- mapM insert bObservations
    return ()
