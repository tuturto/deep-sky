{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Simulation.Observations ( handleFactionObservations, ObservationCandidate(..)
                               , ObservationType(..), groupStarReports, groupPlanetReports
                               , groupStarLaneReports, buildOCStarList, buildOCPlanetList
                               , buildOCStarLaneList, needsObservation )
    where

import Import
import System.Random
import CustomTypes
import Report ( createPlanetReports, createStarLaneReports, createStarReports
              , CollatedPlanetReport(..), CollatedStarLaneReport(..), CollatedStarReport(..))
import Common
import News.Import (planetFoundNews, starFoundNews)
import Data.Maybe (fromJust)


-- | Generate reports for all kinds of things faction can currently observe
--   Currently these include forces on faction's own planets (population that is),
--   sensor stations on planets, forces on space (space ships and satellites) and
--   ground forces on other faction's planets
handleFactionObservations :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend,
    MonadIO m) =>
    Time -> Entity Faction -> ReaderT backend m ()
handleFactionObservations date faction = do
    -- observations by on faction's planets
    -- observations of space by planetary sensor arrays (SensorStation and such)
    -- observations by space forces
    -- observations by ground forces
    factionPlanets <- selectList [ PlanetOwnerId ==. Just (entityKey faction)] []
    populations <- selectList [ PlanetPopulationPlanetId <-. map entityKey factionPlanets
                              , PlanetPopulationPopulation >. 0 ] []
    let populatedPlanets = filter isPopulated factionPlanets
                            where isPopulated planet =  entityKey planet `elem`
                                    map (planetPopulationPlanetId . entityVal) populations
    mapM_ (doPlanetObservation date faction) populatedPlanets
    mapM_ (doSensorStationObservations date faction) populatedPlanets


-- | Let forces on populated planet observe their surroundings and write reports
doPlanetObservation :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Time -> Entity Faction -> Entity Planet -> ReaderT backend m ()
doPlanetObservation date faction planet = do
    let p = entityVal planet
    let observation = PlanetReport (entityKey planet) Nothing (planetStarSystemId p)
                                   (Just $ planetName p) (Just $ planetPosition p) (Just $ planetGravity p)
                                   (entityKey faction) (timeCurrentTime date)
    _ <- insert observation
    pops <- selectList [ PlanetPopulationPlanetId ==. entityKey planet] []
    let popObservations = map (\pop -> PlanetPopulationReport (entityKey planet) (Just $ planetPopulationRaceId $ entityVal pop)
                                                              (Just $ planetPopulationPopulation $ entityVal pop)
                                                              (entityKey faction) (timeCurrentTime date)) pops
    _ <- mapM insert popObservations
    buildings <- selectList [ BuildingPlanetId ==. entityKey planet] []
    let bObservations = map (\b -> BuildingReport (entityKey b) (entityKey planet) (Just $ buildingType (entityVal b))
                                                  (Just $ buildingLevel (entityVal b))
                                                  (Just $ buildingDamage (entityVal b))
                                                  (entityKey faction) (timeCurrentTime date)) buildings
    _ <- mapM insert bObservations
    return ()


-- | Let sensor stations on planet observe surrounding space
doSensorStationObservations :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Time -> Entity Faction -> Entity Planet -> ReaderT backend m ()
doSensorStationObservations date faction planet = do
    let p = entityVal planet
    buildings <- selectList [ BuildingPlanetId ==. entityKey planet
                            , BuildingType ==. SensorStation
                            , BuildingDamage <. 0.5 ] []
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
    let candidates = buildOCStarList starGroups ++ buildOCPlanetList planetGroups ++ buildOCStarLaneList starLaneGroups
    mapM_ (observeRandomTarget date faction candidates) buildings


-- | Observe random target on given list of observation candidates
observeRandomTarget :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Time -> Entity Faction -> [ObservationCandidate] -> Entity Building -> ReaderT backend m ()
observeRandomTarget date faction candidates building = do
    res <- liftIO $ randomRIO (0, length candidates - 1)
    let target = maybeGet res candidates
    _ <- observeTarget date faction target building
    return ()


-- | Observe given target
observeTarget :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Time -> Entity Faction -> Maybe ObservationCandidate -> Entity Building -> ReaderT backend m ()

observeTarget _ _ Nothing _ =
    return ()

observeTarget _ _ (Just OCStarLane {}) _ =
    return ()

observeTarget date faction (Just (OCStar starEntity _ observationType)) _ = do
    let star = entityVal starEntity
    let sid = entityKey starEntity
    aLuminosityClass <- liftIO $ chooseOne (Just (starSpectralType star)) Nothing
    aSpectralType <- liftIO $ chooseOne (Just (starLuminosityClass star)) Nothing
    let res = StarReport sid (starStarSystemId star) (Just (starName star))
                         aLuminosityClass aSpectralType
                         (entityKey faction) (timeCurrentTime date)
    system <- get (starStarSystemId star)
    let news = case observationType of
                NewObservation ->
                    Just $ starFoundNews star (Entity (starStarSystemId star) (fromJust system)) date (entityKey faction)
                UpdatedObservation ->
                    Nothing
    _ <- mapM insert news
    _ <- insert res
    return ()

observeTarget date faction (Just (OCPlanet planetEntity _ observationType)) _ = do
    let planet = entityVal planetEntity
    let pid = entityKey planetEntity
    aGravity <- liftIO $ chooseOne (Just (planetGravity planet)) Nothing
    let res = PlanetReport pid (planetOwnerId planet) (planetStarSystemId planet) (Just $ planetName planet)
                           (Just $ planetPosition planet) aGravity (entityKey faction) (timeCurrentTime date)
    system <- get (planetStarSystemId planet)
    let news = case observationType of
                NewObservation ->
                    Just $ planetFoundNews planetEntity (fromJust system) date (entityKey faction)
                UpdatedObservation ->
                    Nothing
    _ <- mapM insert news
    _ <- insert res
    return ()


data ObservationCandidate = OCStar (Entity Star) (Maybe CollatedStarReport) ObservationType
                          | OCPlanet (Entity Planet) (Maybe CollatedPlanetReport) ObservationType
                          | OCStarLane (Entity StarLane) (Maybe CollatedStarLaneReport)
    deriving Show


data ObservationType =
    NewObservation
    | UpdatedObservation
    deriving (Show, Read, Eq)


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
              matchingReport lane = find (\a -> (cslSystemId1 a == starLaneStarSystem1 (entityVal lane)
                                                   && (cslSystemId2 a == starLaneStarSystem2 (entityVal lane)))
                                                || (cslSystemId2 a == starLaneStarSystem1 (entityVal lane)
                                                    && (cslSystemId1 a == starLaneStarSystem2 (entityVal lane)))) reports


-- | Map list of star entities and respective reports to observation candidates and filter out fully observed items
buildOCStarList :: [(Entity Star, Maybe CollatedStarReport)] -> [ObservationCandidate]
buildOCStarList reports = filter needsObservation $ map combineFn reports
    where combineFn (star, report) = OCStar star report (observationType report)
          observationType Nothing = NewObservation
          observationType _ = UpdatedObservation


-- | Map list of planet entities and respective reports to observation candidates and filter out fully observed items
buildOCPlanetList :: [(Entity Planet, Maybe CollatedPlanetReport)] -> [ObservationCandidate]
buildOCPlanetList reports =
    filter needsObservation $ map combineFn reports
    where combineFn (planet, report) = OCPlanet planet report (observationType report)
          observationType Nothing = NewObservation
          observationType _ = UpdatedObservation


-- | Map list of starlane entities and respective reports to observation candidates and filter out fully observed items
buildOCStarLaneList :: [(Entity StarLane, Maybe CollatedStarLaneReport)] -> [ObservationCandidate]
buildOCStarLaneList reports = filter needsObservation $ map combineFn reports
    where combineFn (lane, report) = OCStarLane lane report


-- | Does given observation candidate need observing?
--   If given collated report contains different information than the object it's about, return True
needsObservation :: ObservationCandidate -> Bool
needsObservation (OCStar _ Nothing _) = True
needsObservation (OCPlanet _ Nothing _) = True
needsObservation OCStarLane {} = False

needsObservation (OCStar entity (Just report) _) =
    csrSpectralType report /= (Just $ starSpectralType star)
    || csrLuminosityClass report /= (Just $ starLuminosityClass star)
    || csrName report /= (Just $ starName star)
    where
        star = entityVal entity

needsObservation (OCPlanet entity (Just report) _) =
    cprOwnerId report /= planetOwnerId planet
    || cprName report /= (Just $ planetName planet)
    || cprPosition report /= (Just $ planetPosition planet)
    || cprGravity report /= (Just $ planetGravity planet)
    where
        planet = entityVal entity
