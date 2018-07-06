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

    loadedSystemReports <- runDB $ selectList [ StarSystemReportUserId ==. userId ] [ Asc StarSystemReportId
                                                                                    , Asc StarSystemReportDate ]
    let systemReports = collateSystems $ Import.map entityVal loadedSystemReports
    defaultLayout $ do
        setTitle "Deep Sky - Star systems"
        $(widgetFile "starsystems")

getStarSystemR :: Key StarSystem -> Handler Html
getStarSystemR systemId = do
    (userId, _) <- requireAuthPair

    systemReport <- createSystemReport systemId userId
    starReports <- createStarReports systemId userId
    planetReports <- createPlanetReports systemId userId
    starLaneReports <- createStarLaneReports systemId userId

    let expl = "Deep Sky - " ++ case (cssrName systemReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown system"

    defaultLayout $ do
        setTitle $ toMarkup expl
        $(widgetFile "starsystem")

getPlanetR :: Key StarSystem -> Key Planet -> Handler Html
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
