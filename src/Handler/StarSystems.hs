{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Handler.StarSystems ( getApiStarSystemsR, getStarSystemsR, getStarSystemR, getPlanetR
                           , getApiPlanetR, getApiPlanetBuildingsR, getApiPlanetPopulationR
                           , getApiStarsR, getApiAllPlanetsR )
    where

import Import
import Report ( collateReports, collateReport )

import Common (requireFaction, apiRequireFaction)
import Queries (loadPlanetPopulationReports)
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
    (_, _, factionId) <- requireFaction
    loadedSystemReports <- runDB $ selectList [ StarSystemReportFactionId ==. factionId ] [ Asc StarSystemReportId
                                                                                          , Asc StarSystemReportDate ]
    let systemReports = collateReports $ map entityVal loadedSystemReports
    return $ toJSON systemReports


-- | api method to retrieve all known stars
getApiStarsR :: Handler Value
getApiStarsR = do
    (_, _, factionId) <- requireFaction
    loadedReports <- runDB $ selectList [ StarReportFactionId ==. factionId ] [ Asc StarReportId
                                                                              , Asc StarReportDate ]
    let reports = collateReports $ map entityVal loadedReports
    return $ toJSON reports


-- | api method to retrieve all known planets
getApiAllPlanetsR :: Handler Value
getApiAllPlanetsR = do
    (_, _, fId) <- apiRequireFaction
    loadedPlanetReports <- runDB $ selectList [ PlanetReportFactionId ==. fId ] [ Asc PlanetReportPlanetId
                                                                                , Asc PlanetReportDate ]
    let planetReport = collateReports $ map entityVal loadedPlanetReports
    return $ toJSON planetReport


-- | api method to retrieve specific planet
getApiPlanetR :: Key Planet -> Handler Value
getApiPlanetR planetId = do
    (_, _, fId) <- apiRequireFaction
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId ==. planetId
                                              , PlanetReportFactionId ==. fId ] [ Asc PlanetReportDate ]
    let planetReport = collateReport $ map entityVal loadedPlanetReports
    return $ toJSON planetReport


-- | api method to retrieve buildings on a planet
getApiPlanetBuildingsR :: Key Planet -> Handler Value
getApiPlanetBuildingsR planetId = do
    (_, _, fId) <- apiRequireFaction
    loadedBuildingReports <- runDB $ selectList [ BuildingReportPlanetId ==. planetId
                                                , BuildingReportFactionId ==. fId ] [ Asc BuildingReportBuildingId
                                                                                    , Asc BuildingReportDate ]
    let buildingReports = collateReports $ map entityVal loadedBuildingReports
    return $ toJSON buildingReports

-- | api method to retrieve population of a planet
getApiPlanetPopulationR :: Key Planet -> Handler Value
getApiPlanetPopulationR planetId = do
    (_, _, fId) <- apiRequireFaction
    loadedPopReports <- runDB $ loadPlanetPopulationReports planetId fId
    let populationReports = collateReports $ map (\(a, b) -> (entityVal a, fmap entityVal b)) loadedPopReports
    return $ toJSON populationReports
