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

getStarSystemsR :: Handler Html
getStarSystemsR = do
    (userId, _) <- requireAuthPair

    loadedSystemReports <- runDB $ selectList [ SolarSystemReportUserId ==. userId ] [ Asc SolarSystemReportId
                                                                                     , Asc SolarSystemReportDate ]
    let systemReports = collateSystems $ Import.map entityVal loadedSystemReports
    defaultLayout $ do
        setTitle "Deep Sky - Star systems"
        $(widgetFile "starsystems")

getStarSystemR :: Key SolarSystem -> Handler Html
getStarSystemR systemId = do
    (userId, _) <- requireAuthPair   
    
    systemReports <- runDB $ selectList [ SolarSystemReportSystemId ==. systemId
                                        , SolarSystemReportUserId ==. userId ] [ Asc SolarSystemReportDate ]
    let systemReport = collateSystem $ Import.map entityVal systemReports

    loadedStarReports <- runDB $ selectList [ StarReportSystemId ==. systemId
                                            , StarReportUserId ==. userId ] [ Asc StarReportId
                                                                            , Asc StarReportDate ]
    let starReports = collateStars $ Import.map entityVal loadedStarReports

    planets <- runDB $ selectList [ PlanetSystemId ==. systemId ] []
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId <-. (Import.map entityKey planets) 
                                              , PlanetReportUserId ==. userId ] [ Asc PlanetReportPlanetId
                                                                                , Asc PlanetReportDate ]
    let planetReports = collatePlanets $ Import.map entityVal loadedPlanetReports
    let expl = "Deep Sky - " ++ case (cssrName systemReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown system"

    defaultLayout $ do
        setTitle $ toMarkup expl
        $(widgetFile "starsystem")

getPlanetR :: Key SolarSystem -> Key Planet -> Handler Html
getPlanetR _ planetId = do
    (userId, _) <- requireAuthPair   

    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId ==. planetId
                                              , PlanetReportUserId ==. userId ] [ Asc PlanetReportDate ]
    let planetReport = collatePlanet $ Import.map entityVal loadedPlanetReports
    let expl = "Deep Sky - " ++ case (cprName planetReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown planet"
    defaultLayout $ do
        setTitle $ toMarkup expl
        $(widgetFile "planet")
