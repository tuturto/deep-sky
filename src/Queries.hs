{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Queries ( planetPopulationReports, shipsAtPlanet, ShipLandingStatus(..), planetConstructionQueue
               , populatedFarmingPlanets )
    where

import Import
import qualified Prelude as P
import qualified Database.Esqueleto as E
import Data.List ( nub )
import Common ( safeHead )
import CustomTypes ( BuildingType(..) )


-- | Load population reports of a planet and respective races
planetPopulationReports :: (MonadIO m, BackendCompatible SqlBackend backend,
                                PersistQueryRead backend, PersistUniqueRead backend) =>
                               Key Planet -> Key Faction -> ReaderT backend m [(Entity PlanetPopulationReport, Maybe (Entity Race))]
planetPopulationReports pId fId =
    E.select $
        E.from $ \(popReport `E.LeftOuterJoin` pRace) -> do
            E.on (popReport E.^. PlanetPopulationReportRaceId E.==. pRace E.?. RaceId )
            E.where_ (popReport E.^. PlanetPopulationReportPlanetId E.==. E.val pId
                      E.&&. popReport E.^. PlanetPopulationReportFactionId E.==. E.val fId)
            E.orderBy [ E.asc ( popReport E.^. PlanetPopulationReportPlanetId)
                      , E.asc ( popReport E.^. PlanetPopulationReportRaceId)
                      , E.asc ( popReport E.^. PlanetPopulationReportDate)
                      ]
            return (popReport, pRace)


-- | Load ships that are on or around a given planet and their faction info
shipsAtPlanet :: (MonadIO m, BackendCompatible SqlBackend backend,
                    PersistQueryRead backend, PersistUniqueRead backend) =>
                   Key Planet -> ShipLandingStatus -> ReaderT backend m [(Entity Ship, Entity Faction)]
shipsAtPlanet pId landingStatus = do
    let landed = case landingStatus of
                    ShipOnPlanet -> True
                    ShipInOrbit -> False
    E.select $
        E.from $ \(ship `E.InnerJoin` faction) -> do
                E.on (ship E.^. ShipOwnerId E.==. faction E.^. FactionId)
                E.where_ (ship E.^. ShipPlanetId E.==. E.val (Just pId)
                          E.&&. ship E.^. ShipLanded E.==. E.val landed)
                return (ship, faction)

data ShipLandingStatus = ShipOnPlanet | ShipInOrbit


-- | Load planet with construction queue
planetConstructionQueue :: (MonadIO m, BackendCompatible SqlBackend backend,
                            PersistQueryRead backend, PersistUniqueRead backend) =>
                           Key Planet -> ReaderT backend m (Maybe (Entity Planet), [Entity BuildingConstruction])
planetConstructionQueue pId = do
    res <- E.select $
            E.from $ \(planet `E.LeftOuterJoin` bConstruction) -> do
                E.on (bConstruction E.^. BuildingConstructionPlanetId E.==. planet E.^. PlanetId)
                E.where_ (planet E.^. PlanetId E.==. E.val pId)
                return (planet, bConstruction)
    let planet = fst <$> safeHead res
    return (planet, map snd res)


-- | Load planets with at least given population or farm count
populatedFarmingPlanets :: (MonadIO m, BackendCompatible SqlBackend backend
                           , PersistQueryRead backend, PersistUniqueRead backend) =>
                           Int -> Int -> Key Faction -> ReaderT backend m [Entity Planet]
populatedFarmingPlanets pop farms fId = do
    planets <- E.select $
        E.from $ \(planet `E.LeftOuterJoin` population `E.LeftOuterJoin` building) -> do
            E.on (building E.?. BuildingPlanetId E.==. E.just (planet E.^. PlanetId))
            E.on (population E.?. PlanetPopulationPlanetId E.==. E.just (planet E.^. PlanetId))
            E.where_ (planet E.^. PlanetOwnerId E.==. E.val (Just fId)
                      E.&&. building E.?. BuildingType E.==. E.val (Just Farm))
            E.orderBy [ E.asc (planet E.^. PlanetId) ]
            return (planet, population, building)
    let grouped = groupBy (\(a, _, _) (b, _, _) -> entityKey a == entityKey b) planets
    let counted = catMaybes $ fmap farmAndPopCount grouped
    let filtered = filter (\(_, p, f) ->
                                p >= pop
                                || f >= farms) counted
    let mapped = fmap (\(ent, _, _) -> ent) filtered
    return mapped


-- | Obtain planet's total population and total amount of farms
-- All tuples are considered to be for same planet
farmAndPopCount :: [(Entity Planet, Maybe (Entity PlanetPopulation), Maybe (Entity Building))] -> Maybe ((Entity Planet), Int, Int)
farmAndPopCount [] = Nothing
farmAndPopCount xs =
    let
        populationCount = (sum . (fmap (planetPopulationPopulation . entityVal)) . nub . catMaybes . (fmap (\(_, x, _) -> x))) xs
        farmCount = (length . nub . catMaybes . (fmap (\(_, _, x) -> x))) xs
        (planet, _, _) = P.head xs
    in
        Just $ (planet, populationCount, farmCount)
