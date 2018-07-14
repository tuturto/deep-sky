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
import Common

-- | Generate reports for all kinds of things faction can currently observe
--   Currently these include forces on faction's own planets (population that is),
--   sensor stations on planets, forces on space (space ships and satellites) and
--   ground forces on other faction's planets
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
    _ <- mapM (doSensorStationObservations faction) populatedPlanets
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
doSensorStationObservations :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Entity Faction -> Entity Planet -> ReaderT backend m ()
doSensorStationObservations faction planet = do
    time <- starDate
    let p = entityVal planet
    buildings <- selectList [ BuildingPlanetId ==. (entityKey planet)
                            , BuildingType ==. SensorStation
                            , BuildingConstruction ==. 1.0 
                            , BuildingDamage <. 0.5 ] []
    starSystem <- get $ planetStarSystemId p
    starSystemReport <- createSystemReport (planetStarSystemId p) $ entityKey faction -- where does this really belong?
    stars <- selectList [ StarStarSystemId ==. planetStarSystemId p ] []
    starReports <- createStarReports (planetStarSystemId p) $ entityKey faction
    let starGroups = groupStarReports stars starReports
    planets <- selectList [ PlanetStarSystemId ==. planetStarSystemId p 
                          , PlanetId !=. entityKey planet ] []
    planetReports <- createPlanetReports (planetStarSystemId p) $ entityKey faction
    let planetGroups = groupPlanetReports planets planetReports
    starLanes <- selectList ([ StarLaneStarSystem1 ==. planetStarSystemId p ]
                         ||. [ StarLaneStarSystem2 ==. planetStarSystemId p ]) []
    starLaneReports <- createStarLaneReports (planetStarSystemId p) $ entityKey faction
    let starLaneGroups = groupStarLaneReports starLanes starLaneReports
    let candidates = (buildOCStarList starGroups) ++ (buildOCPlanetList planetGroups) ++ (buildOCStarLaneList starLaneGroups)
    _ <- mapM (observe faction candidates) buildings
    return ()

observe :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Entity Faction -> [ObservationCandidate] -> (Entity Building) -> ReaderT backend m ()
observe faction candidates building = do
    res <- liftIO $ randomRIO (0, (length candidates) - 1)
    let target = maybeGet candidates res
    _ <- observeTarget faction target
    return ()

observeTarget :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Entity Faction -> (Maybe ObservationCandidate) -> ReaderT backend m ()
observeTarget _ Nothing = do
    return ()
observeTarget faction (Just (OCStar starEntity _)) = do
    let star = entityVal starEntity
    let sid = entityKey starEntity
    date <- starDate
    aLuminosityClass <- liftIO $ chooseOne (Just (starSpectralType star)) Nothing
    aSpectralType <- liftIO $ chooseOne (Just (starLuminosityClass star)) Nothing
    let res = StarReport sid (starStarSystemId star) (Just (starName star)) 
                         aLuminosityClass aSpectralType
                         (entityKey faction) (timeCurrentTime date)
    _ <- insert res
    return ()
observeTarget _ _ = do
    return ()


data ObservationCandidate = OCStar (Entity Star) (Maybe CollatedStarReport)
                          | OCPlanet (Entity Planet) (Maybe CollatedPlanetReport)
                          | OCStarLane (Entity StarLane) (Maybe CollatedStarLaneReport)
    deriving Show

-- | given list of stars and collated reports, build list of pairs with star and maybe respective report
groupStarReports :: [Entity Star] -> [CollatedStarReport] -> [(Entity Star, Maybe CollatedStarReport)]
groupStarReports stars reports = 
    map fn stars
        where fn star = (star, matchingReport star)
              matchingReport star = find (\a -> csrStarId a == entityKey star) reports

-- | given list of planets and collated reports, build list of pairs with planet and maybe respective report
groupPlanetReports :: [Entity Planet] -> [CollatedPlanetReport] -> [(Entity Planet, Maybe CollatedPlanetReport)]
groupPlanetReports planets reports = 
    map fn planets
        where fn planet = (planet, matchingReport planet)
              matchingReport planet = find (\a -> cprPlanetId a == entityKey planet) reports

-- | given list of starlanes and collated reports, build list of pairs with starlane and maybe respective report
groupStarLaneReports :: [Entity StarLane] -> [CollatedStarLaneReport] -> [(Entity StarLane, Maybe CollatedStarLaneReport)]
groupStarLaneReports lanes reports =
    map fn lanes
        where fn lane = (lane, matchingReport lane)
              matchingReport lane = find (\a -> (cslSystemId1 a == (starLaneStarSystem1 $ entityVal lane) 
                                                   && (cslSystemId2 a == (starLaneStarSystem2 $ entityVal lane)))
                                                || (cslSystemId2 a == (starLaneStarSystem1 $ entityVal lane) 
                                                    && (cslSystemId1 a == (starLaneStarSystem2 $ entityVal lane)))) reports

buildOCStarList :: [(Entity Star, Maybe CollatedStarReport)] -> [ObservationCandidate]
buildOCStarList reports = filter needsObservation $ map combineFn reports
    where combineFn (star, report) = OCStar star report

buildOCPlanetList :: [(Entity Planet, Maybe CollatedPlanetReport)] -> [ObservationCandidate]
buildOCPlanetList reports = filter needsObservation $ map combineFn reports
    where combineFn (planet, report) = OCPlanet planet report

buildOCStarLaneList :: [(Entity StarLane, Maybe CollatedStarLaneReport)] -> [ObservationCandidate]
buildOCStarLaneList reports = filter needsObservation $ map combineFn reports
    where combineFn (lane, report) = OCStarLane lane report

needsObservation :: ObservationCandidate -> Bool
needsObservation (OCStar _ Nothing) = True
needsObservation (OCPlanet _ Nothing) = True
needsObservation (OCStarLane _ Nothing) = True
needsObservation (OCStar entity (Just report)) = 
    csrSpectralType report == (Just $ starSpectralType star)
    && csrLuminosityClass report == (Just $ starLuminosityClass star)
    where
        star = entityVal entity

needsObservation (OCPlanet entity (Just report)) = True
needsObservation (OCStarLane entity (Just report)) = True
