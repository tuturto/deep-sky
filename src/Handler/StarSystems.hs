{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.StarSystems where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Blaze.Html5
import Report
import Widgets

getStarSystemsR :: Handler Html
getStarSystemsR = do
    (userId, userName) <- requireAuthPair

    loadedSystemReports <- runDB $ selectList [ SolarSystemReportUserId ==. userId ] [ Asc SolarSystemReportId
                                                                                     , Asc SolarSystemReportDate ]
    starReports <- runDB $ selectList [ StarReportUserId ==. userId ] [ Asc StarReportId
                                                                      , Asc StarReportDate ]
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

    planets <- runDB $ selectList [ PlanetSystemId ==. systemId ] []
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId <-. (Import.map entityKey planets) 
                                              , PlanetReportUserId ==. userId ] [ Asc PlanetReportPlanetId
                                                                                , Asc PlanetReportDate ]
    let planetReports = collatePlanets $ Import.map entityVal loadedPlanetReports
    let title = "Deep Sky - " ++ case (cssrName systemReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown system"

    defaultLayout $ do
        setTitle $ toMarkup title
        $(widgetFile "starsystem")

getPlanetR :: Key SolarSystem -> Key Planet -> Handler Html
getPlanetR systemId planetId = do
    (userId, _) <- requireAuthPair   

    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId ==. planetId
                                              , PlanetReportUserId ==. userId ] [ Asc PlanetReportDate ]
    let planetReport = collatePlanet $ Import.map entityVal loadedPlanetReports
    let title = "Deep Sky - " ++ case (cprName planetReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown planet"
    defaultLayout $ do
        setTitle $ toMarkup title
        $(widgetFile "planet")
