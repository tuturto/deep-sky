{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Handler.StarSystems
    ( getApiStarSystemsR, getStarSystemsR, getStarSystemR, getPlanetR
    , getApiPlanetR, getApiPlanetBuildingsR, getApiPlanetPopulationR
    , getApiStarsR, getApiAllPlanetsR, getApiPlanetStatusR, getApiStarSystemR )
    where

import Import
import Report ( collateReports, collateReport, planetStatusIconMapper )

import Common ( apiRequireFaction, apiNotFound, apiRequireViewSimulation )
import Queries ( planetPopulationReports, planetReports, starSystemReports )
import Handler.Home (getNewHomeR)


-- | serve client program and have it start displaying all known star systems
getStarSystemsR :: Handler Html
getStarSystemsR = getNewHomeR


-- | serve client program and have it start displaying specific star system
getStarSystemR :: StarSystemId -> Handler Html
getStarSystemR _ = getNewHomeR


-- | serve client program and have it start displaying specific planet
getPlanetR :: PlanetId -> Handler Html
getPlanetR _ = getNewHomeR


-- | api method to retrieve single star system
getApiStarSystemR :: StarSystemId -> Handler Value
getApiStarSystemR sId = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    reports <- runDB $ starSystemReports fId sId
    _ <- when (reports == mempty) apiNotFound
    return $ toJSON $ collateReport reports


-- | api method to retrieve all known star systems
getApiStarSystemsR :: Handler Value
getApiStarSystemsR = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    loadedSystemReports <- runDB $ selectList [ StarSystemReportFactionId ==. fId ]
                                              [ Asc StarSystemReportId
                                              , Asc StarSystemReportDate ]

    let systemReports = collateReports $ map entityVal loadedSystemReports
    return $ toJSON systemReports


-- | api method to retrieve all known stars
getApiStarsR :: Handler Value
getApiStarsR = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    loadedReports <- runDB $ selectList [ StarReportFactionId ==. fId ] [ Asc StarReportId
                                                                        , Asc StarReportDate ]
    let reports = collateReports $ map entityVal loadedReports
    return $ toJSON reports


-- | api method to retrieve all known planets
getApiAllPlanetsR :: Handler Value
getApiAllPlanetsR = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    loadedPlanetReports <- runDB $ selectList [ PlanetReportFactionId ==. fId ] [ Asc PlanetReportPlanetId
                                                                                , Asc PlanetReportDate ]
    let planetReport = collateReports $ map entityVal loadedPlanetReports
    return $ toJSON planetReport


-- | api method to retrieve specific planet
getApiPlanetR :: PlanetId -> Handler Value
getApiPlanetR planetId = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    reports <- runDB $ planetReports fId planetId
    return $ toJSON $ collateReport reports


-- | api method to retrieve buildings on a planet
getApiPlanetBuildingsR :: PlanetId -> Handler Value
getApiPlanetBuildingsR planetId = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    loadedBuildingReports <- runDB $ selectList [ BuildingReportPlanetId ==. planetId
                                                , BuildingReportFactionId ==. fId ] [ Asc BuildingReportBuildingId
                                                                                    , Asc BuildingReportDate ]
    let buildingReports = collateReports $ map entityVal loadedBuildingReports
    return $ toJSON buildingReports


-- | api method to retrieve population of a planet
getApiPlanetPopulationR :: PlanetId -> Handler Value
getApiPlanetPopulationR planetId = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    loadedPopReports <- runDB $ planetPopulationReports planetId fId
    let populationReports = collateReports $ map (\(a, b) -> (entityVal a, fmap entityVal b)) loadedPopReports
    return $ toJSON populationReports


getApiPlanetStatusR :: PlanetId -> Handler Value
getApiPlanetStatusR planetId = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    statuses <- runDB $ selectList [ PlanetStatusReportPlanetId ==. planetId
                                   , PlanetStatusReportFactionId ==. fId ]
                                   [ Desc PlanetStatusReportDate ]
    render <- getUrlRender
    let icons = planetStatusIconMapper render
    let report = collateReport $ map (\x -> (entityVal x, icons)) statuses
    return $ toJSON report
