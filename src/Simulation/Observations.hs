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

module Simulation.Observations where

import Import
import Simulation.Time
import Simulation.Food (handleFactionFood)
import System.Random
import MenuHelpers
import CustomTypes
import Report

-- | Generate reports for all kinds of things faction can currently observe
handleFactionObservations :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend,
    MonadIO m) =>
    Entity Faction -> ReaderT backend m ()
handleFactionObservations faction = do
    -- observations by on faction's planets
    -- observations of space by planetary sensor arrays (SensorStation and such)
    -- observations by space forces
    -- observations by ground forces
    factionPlanets <- selectList [ PlanetOwnerId ==. Just (entityKey faction)] []    
    populations <- selectList [ PlanetPopulationPlanetId <-. map entityKey factionPlanets 
                              , PlanetPopulationPopulation >. 0 ] []
    let populatedPlanets = filter isPopulated factionPlanets
                            where isPopulated planet = elem (entityKey planet)
                                    (map (planetPopulationPlanetId . entityVal) populations)
    _ <- mapM (doPlanetObservation faction) populatedPlanets
    _ <- mapM (doSensorStationObservation faction) populatedPlanets
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

-- | Let sensor stations on planet observe surrounding space
doSensorStationObservation :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Entity Faction -> Entity Planet -> ReaderT backend m ()
doSensorStationObservation faction planet = do
    time <- starDate
    let p = entityVal planet
    buildings <- selectList [ BuildingPlanetId ==. (entityKey planet)
                            , BuildingType ==. SensorStation
                            , BuildingConstruction ==. 1.0 
                            , BuildingDamage <. 0.5 ] []
    stars <- selectList [ StarStarSystemId ==. planetStarSystemId p ] []
    starReports <- createStarReports (planetStarSystemId p) $ entityKey faction
    planets <- selectList [ PlanetStarSystemId ==. planetStarSystemId p 
                          , PlanetId !=. entityKey planet ] []
    planetReports <- createPlanetReports (planetStarSystemId p) $ entityKey faction
    starLanes <- selectList ([ StarLaneStarSystem1 ==. planetStarSystemId p ]
                         ||. [ StarLaneStarSystem2 ==. planetStarSystemId p ]) []
    starLaneReports <- createStarLaneReports (planetStarSystemId p) $ entityKey faction
    return ()
