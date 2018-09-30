{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Queries (loadPlanetPopulationReports, loadPlanetShips, ShipLandingStatus(..))
    where

import Import
import qualified Database.Esqueleto as E

-- | Load population reports of a planet and respective races
loadPlanetPopulationReports :: (MonadIO m, BackendCompatible SqlBackend backend,
                                PersistQueryRead backend, PersistUniqueRead backend) =>
                               Key Planet -> Key Faction -> ReaderT backend m [(Entity PlanetPopulationReport, Maybe (Entity Race))]
loadPlanetPopulationReports pId fId = do
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
loadPlanetShips :: (MonadIO m, BackendCompatible SqlBackend backend,
                    PersistQueryRead backend, PersistUniqueRead backend) =>
                   Key Planet -> ShipLandingStatus -> ReaderT backend m [(Entity Ship, Entity Faction)]
loadPlanetShips pId landingStatus = do
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