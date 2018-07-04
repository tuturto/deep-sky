{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.StarSystems where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Report

getStarSystemsR :: Handler Html
getStarSystemsR = do
    (userId, userName) <- requireAuthPair

    loadedSystemReports <- runDB $ selectList [ SolarSystemReportUserId ==. userId ] [ Asc SolarSystemReportId
                                                                                     , Asc SolarSystemReportDate ]
    starReports <- runDB $ selectList [ StarReportUserId ==. userId ] [ Asc StarReportId
                                                                      , Asc StarReportDate ]
    let systemReports = collateSystems $ map entityVal loadedSystemReports
    defaultLayout $ do
        setTitle "Deep Sky - Star systems"
        $(widgetFile "starsystems")

getStarSystemR :: Key SolarSystem -> Handler Html
getStarSystemR systemId = do
    (userId, _) <- requireAuthPair   
    
    systemReports <- runDB $ selectList [ SolarSystemReportSystemId ==. systemId
                                        , SolarSystemReportUserId ==. userId ] [ Asc SolarSystemReportDate ]
    let systemReport = collateSystem $ map entityVal systemReports

    planets <- runDB $ selectList [ PlanetSystemId ==. systemId ] []
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId <-. (map entityKey planets) 
                                              , PlanetReportUserId ==. userId ] [ Asc PlanetReportPlanetId
                                                                                , Asc PlanetReportDate ]
    let planetReports = collatePlanets $ map entityVal loadedPlanetReports

    defaultLayout $ do
        setTitle "Deep Sky - Star systems"
        $(widgetFile "starsystem")
