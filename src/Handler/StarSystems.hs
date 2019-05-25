{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Handler.StarSystems ( getApiStarSystemsR, getStarSystemsR, getStarSystemR, getPlanetR
                           , getApiPlanetR, getApiPlanetBuildingsR, getApiPlanetPopulationR
                           , getApiStarsR, getApiAllPlanetsR, getApiPlanetStatusR )
    where

import Import
import Report ( collateReports, collateReport, planetStatusIconMapper )

import Common ( apiRequireFaction )
import Queries ( planetPopulationReports, planetReports )
import Handler.Home (getNewHomeR)


-- | serve client program and have it start displaying all known star systems
getStarSystemsR :: Handler Html
getStarSystemsR = getNewHomeR


-- | serve client program and have it start displaying specific star system
getStarSystemR :: Key StarSystem -> Handler Html
getStarSystemR _ = getNewHomeR


-- | serve client program and have it start displaying specific planet
getPlanetR :: Key StarSystem -> Key Planet -> Handler Html
getPlanetR _ _ = getNewHomeR


-- | api method to retrieve all known star systems
getApiStarSystemsR :: Handler Value
getApiStarSystemsR = do
    (_, _, _, factionId) <- apiRequireFaction
    loadedSystemReports <- runDB $ selectList [ StarSystemReportFactionId ==. factionId ] [ Asc StarSystemReportId
                                                                                          , Asc StarSystemReportDate ]
    let systemReports = collateReports $ map entityVal loadedSystemReports
    return $ toJSON systemReports


-- | api method to retrieve all known stars
getApiStarsR :: Handler Value
getApiStarsR = do
    (_, _, _, factionId) <- apiRequireFaction
    loadedReports <- runDB $ selectList [ StarReportFactionId ==. factionId ] [ Asc StarReportId
                                                                              , Asc StarReportDate ]
    let reports = collateReports $ map entityVal loadedReports
    return $ toJSON reports


-- | api method to retrieve all known planets
getApiAllPlanetsR :: Handler Value
getApiAllPlanetsR = do
    (_, _, _, fId) <- apiRequireFaction
    loadedPlanetReports <- runDB $ selectList [ PlanetReportFactionId ==. fId ] [ Asc PlanetReportPlanetId
                                                                                , Asc PlanetReportDate ]
    let planetReport = collateReports $ map entityVal loadedPlanetReports
    return $ toJSON planetReport


-- | api method to retrieve specific planet
getApiPlanetR :: Key Planet -> Handler Value
getApiPlanetR planetId = do
    (_, _, _, fId) <- apiRequireFaction
    reports <- runDB $ planetReports fId planetId
    return $ toJSON $ collateReport reports


-- | api method to retrieve buildings on a planet
getApiPlanetBuildingsR :: Key Planet -> Handler Value
getApiPlanetBuildingsR planetId = do
    (_, _, _, fId) <- apiRequireFaction
    loadedBuildingReports <- runDB $ selectList [ BuildingReportPlanetId ==. planetId
                                                , BuildingReportFactionId ==. fId ] [ Asc BuildingReportBuildingId
                                                                                    , Asc BuildingReportDate ]
    let buildingReports = collateReports $ map entityVal loadedBuildingReports
    return $ toJSON buildingReports


-- | api method to retrieve population of a planet
getApiPlanetPopulationR :: Key Planet -> Handler Value
getApiPlanetPopulationR planetId = do
    (_, _, _, fId) <- apiRequireFaction
    loadedPopReports <- runDB $ planetPopulationReports planetId fId
    let populationReports = collateReports $ map (\(a, b) -> (entityVal a, fmap entityVal b)) loadedPopReports
    return $ toJSON populationReports


getApiPlanetStatusR :: Key Planet -> Handler Value
getApiPlanetStatusR planetId = do
    (_, _, _, fId) <- apiRequireFaction
    statuses <- runDB $ selectList [ PlanetStatusReportPlanetId ==. planetId
                                   , PlanetStatusReportFactionId ==. fId ]
                                   [ Desc PlanetStatusReportDate ]
    render <- getUrlRender
    let icons = planetStatusIconMapper render
    let report = collateReport $ map (\x -> (entityVal x, icons)) statuses
    return $ toJSON report
