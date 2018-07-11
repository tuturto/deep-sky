{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.StarSystems where

import Import
import Text.Blaze.Html5
import Report
import Widgets
import MenuHelpers

getApiStarSystemsR :: Handler Value
getApiStarSystemsR = do
    loadedSystemReports <- runDB $ selectList [] [ Asc StarSystemReportId
                                                 , Asc StarSystemReportDate ]
    let systemReports = collateSystems $ Import.map entityVal loadedSystemReports
    return $ toJSON systemReports

getStarSystemsR :: Handler Html
getStarSystemsR = do
    (_, user) <- requireAuthPair   
    factionId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR

    loadedSystemReports <- runDB $ selectList [ StarSystemReportFactionId ==. factionId ] [ Asc StarSystemReportId
                                                                                          , Asc StarSystemReportDate ]
    let systemReports = collateSystems $ Import.map entityVal loadedSystemReports
    defaultLayout $ do
        setTitle "Deep Sky - Star systems"
        $(widgetFile "starsystems")

getStarSystemR :: Key StarSystem -> Handler Html
getStarSystemR systemId = do
    (_, user) <- requireAuthPair   
    factionId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR

    systemReport <- createSystemReport systemId factionId
    starReports <- createStarReports systemId factionId
    planetReports <- createPlanetReports systemId factionId
    starLaneReports <- createStarLaneReports systemId factionId

    let expl = "Deep Sky - " ++ case (cssrName systemReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown system"

    defaultLayout $ do
        setTitle $ toMarkup expl
        $(widgetFile "starsystem")

getPlanetR :: Key StarSystem -> Key Planet -> Handler Html
getPlanetR _ planetId = do
    (_, user) <- requireAuthPair   
    factionId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId ==. planetId
                                              , PlanetReportFactionId ==. factionId ] [ Asc PlanetReportDate ]
    let planetReport = collatePlanet $ Import.map entityVal loadedPlanetReports

    loadedBuildingReports <- runDB $ selectList [ BuildingReportPlanetId ==. planetId 
                                                , BuildingReportFactionId ==. factionId ] [ Asc BuildingReportBuildingId
                                                                                          , Asc BuildingReportDate ]
    let buildingReports = collateBuildings $ Import.map entityVal loadedBuildingReports

    loadedPopulationReports <- runDB $ selectList [ PlanetPopulationReportPlanetId ==. planetId
                                                  , PlanetPopulationReportFactionId ==. factionId ] [ Asc PlanetPopulationReportPlanetId
                                                                                                    , Asc PlanetPopulationReportRaceId
                                                                                                    , Asc PlanetPopulationReportDate ]
    let partialPopulationReports = collatePopulations $ Import.map entityVal loadedPopulationReports
    populationReports <- runDB $ Import.mapM addPopulationDetails partialPopulationReports

    let expl = "Deep Sky - " ++ case (cprName planetReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown planet"
    defaultLayout $ do
        setTitle $ toMarkup expl
        $(widgetFile "planet")

addPopulationDetails :: (BaseBackend backend ~ SqlBackend,
    MonadIO m, PersistStoreRead backend) =>
    CollatedPopulationReport -> ReaderT backend m CollatedPopulationReport
addPopulationDetails report = do
    aRace <- getMaybeEntity $ cpopRaceId report
    res <- case aRace of
                (Just x) -> return $ report { cpopRace = Just $ raceName x}
                Nothing  -> return report
    return res