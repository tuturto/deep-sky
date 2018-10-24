{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}

module Handler.StarSystems ( getApiStarSystemsR, getStarSystemsR, getStarSystemR, getPlanetR
                           , getApiPlanetR, getApiPlanetBuildingsR, getApiPlanetPopulationR
                           , getApiStarsR, getApiAllPlanetsR )
    where

import Import
import Report ( collateReports, collateReport )

import Common (requireFaction, apiRequireFaction)
import Queries (loadPlanetPopulationReports)
import Handler.Home (getNewHomeR)

getApiStarSystemsR :: Handler Value
getApiStarSystemsR = do
    (_, _, factionId) <- requireFaction
    loadedSystemReports <- runDB $ selectList [ StarSystemReportFactionId ==. factionId ] [ Asc StarSystemReportId
                                                                                          , Asc StarSystemReportDate ]
    let systemReports = collateReports $ map entityVal loadedSystemReports
    return $ toJSON systemReports

getApiStarsR :: Handler Value
getApiStarsR = do
    (_, _, factionId) <- requireFaction
    loadedReports <- runDB $ selectList [ StarReportFactionId ==. factionId ] [ Asc StarReportId
                                                                              , Asc StarReportDate ]    
    let reports = collateReports $ map entityVal loadedReports
    return $ toJSON reports

getStarSystemsR :: Handler Html
getStarSystemsR = getNewHomeR

getStarSystemR :: Key StarSystem -> Handler Html
getStarSystemR _ = getNewHomeR

getPlanetR :: Key StarSystem -> Key Planet -> Handler Html
getPlanetR _ _ = getNewHomeR

getApiAllPlanetsR :: Handler Value 
getApiAllPlanetsR = do
    (_, _, fId) <- apiRequireFaction
    loadedPlanetReports <- runDB $ selectList [ PlanetReportFactionId ==. fId ] [ Asc PlanetReportPlanetId
                                                                                , Asc PlanetReportDate ]
    let planetReport = collateReports $ map entityVal loadedPlanetReports
    return $ toJSON planetReport

getApiPlanetR :: Key Planet -> Handler Value
getApiPlanetR planetId = do
    (_, _, fId) <- apiRequireFaction
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId ==. planetId
                                              , PlanetReportFactionId ==. fId ] [ Asc PlanetReportDate ]
    let planetReport = collateReport $ map entityVal loadedPlanetReports
    return $ toJSON planetReport

getApiPlanetBuildingsR :: Key Planet -> Handler Value
getApiPlanetBuildingsR planetId = do
    (_, _, fId) <- apiRequireFaction
    loadedBuildingReports <- runDB $ selectList [ BuildingReportPlanetId ==. planetId 
                                                , BuildingReportFactionId ==. fId ] [ Asc BuildingReportBuildingId
                                                                                    , Asc BuildingReportDate ]
    let buildingReports = collateReports $ map entityVal loadedBuildingReports
    return $ toJSON buildingReports

getApiPlanetPopulationR :: Key Planet -> Handler Value
getApiPlanetPopulationR planetId = do
    (_, _, fId) <- apiRequireFaction
    loadedPopReports <- runDB $ loadPlanetPopulationReports planetId fId
    let populationReports = collateReports $ map (\(a, b) -> (entityVal a, fmap entityVal b)) loadedPopReports
    return $ toJSON populationReports
