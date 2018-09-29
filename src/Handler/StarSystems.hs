{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Handler.StarSystems ( getApiStarSystemsR, getStarSystemsR, getStarSystemR, getPlanetR
                           , getApiPlanetR, getApiPlanetBuildingsR, getApiPlanetPopulationR )
    where

import Import
import Text.Blaze.Html5 (toMarkup)
import Report ( collateSystems, collatePopulations, collateBuildings, collatePlanet
              , createStarLaneReports, createPlanetReports, createStarReports, createSystemReport
              , cprName, CollatedStarSystemReport(..) )
import Widgets
import Database.Persist.Sql (fromSqlKey)
import Common (requireFaction, apiRequireFaction)
import Queries (loadPlanetPopulationReports)

getApiStarSystemsR :: Handler Value
getApiStarSystemsR = do
    _ <- apiRequireFaction
    loadedSystemReports <- runDB $ selectList [] [ Asc StarSystemReportId
                                                 , Asc StarSystemReportDate ]
    let systemReports = collateSystems $ map entityVal loadedSystemReports
    return $ toJSON systemReports

getStarSystemsR :: Handler Html
getStarSystemsR = do
    (_, _, factionId) <- requireFaction

    loadedSystemReports <- runDB $ selectList [ StarSystemReportFactionId ==. factionId ] [ Asc StarSystemReportId
                                                                                          , Asc StarSystemReportDate ]
    let systemReports = collateSystems $ map entityVal loadedSystemReports
    defaultLayout $ do
        setTitle "Deep Sky - Star systems"
        $(widgetFile "starsystems")

getStarSystemR :: Key StarSystem -> Handler Html
getStarSystemR systemId = do
    (_, _, factionId) <- requireFaction
    systemReport <- runDB $ createSystemReport systemId factionId
    starReports <- runDB $ createStarReports systemId factionId
    planetReports <- runDB $ createPlanetReports systemId factionId
    starLaneReports <- runDB $ createStarLaneReports systemId factionId

    let expl = "Deep Sky - " ++ case (cssrName systemReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown system"

    defaultLayout $ do
        setTitle $ toMarkup expl
        $(widgetFile "starsystem")

getPlanetR :: Key StarSystem -> Key Planet -> Handler Html
getPlanetR _ planetId = do
    (_, _, factionId) <- requireFaction
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId ==. planetId
                                              , PlanetReportFactionId ==. factionId ] [ Asc PlanetReportDate ]
    let planetReport = collatePlanet $ map entityVal loadedPlanetReports
    let planetKey = fromSqlKey planetId

    let expl = "Deep Sky - " ++ case (cprName planetReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown planet"
    defaultLayout $ do
        setTitle $ toMarkup expl 
        addScript $ StaticR js_planet_js
        addStylesheet $ StaticR css_site_css
        $(widgetFile "planet")

getApiPlanetR :: Key Planet -> Handler Value
getApiPlanetR planetId = do
    (_, _, fId) <- apiRequireFaction
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId ==. planetId
                                              , PlanetReportFactionId ==. fId ] [ Asc PlanetReportDate ]
    let planetReport = collatePlanet $ map entityVal loadedPlanetReports
    return $ toJSON planetReport

getApiPlanetBuildingsR :: Key Planet -> Handler Value
getApiPlanetBuildingsR planetId = do
    (_, _, fId) <- apiRequireFaction
    loadedBuildingReports <- runDB $ selectList [ BuildingReportPlanetId ==. planetId 
                                                , BuildingReportFactionId ==. fId ] [ Asc BuildingReportBuildingId
                                                                                    , Asc BuildingReportDate ]
    let buildingReports = collateBuildings $ map entityVal loadedBuildingReports
    return $ toJSON buildingReports

getApiPlanetPopulationR :: Key Planet -> Handler Value
getApiPlanetPopulationR planetId = do
    (_, _, fId) <- apiRequireFaction
    loadedPopReports <- runDB $ loadPlanetPopulationReports planetId fId
    let populationReports = collatePopulations $ map (\(a, b) -> (entityVal a, fmap entityVal b)) loadedPopReports
    return $ toJSON populationReports


