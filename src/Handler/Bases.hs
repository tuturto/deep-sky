{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Bases where

import Import
import Report
import Widgets
import Data.List (head)
import Text.Blaze.Html5

getBasesR :: Handler Html
getBasesR = do
    (userId, _) <- requireAuthPair

    loadedPlanetReports <- runDB $ selectList [ PlanetReportOwnerId ==. Just userId ] [ Asc PlanetReportPlanetId
                                                                                      , Asc PlanetReportDate ]

    let planetReports = filter (\x -> Just userId == cprOwnerId x) $ collatePlanets $ Import.map entityVal loadedPlanetReports
    baseReports <- Import.mapM addBaseDetails planetReports

    defaultLayout $ do
        setTitle "Deep Sky - Bases"
        $(widgetFile "bases")

getBaseR :: Key Planet -> Handler Html
getBaseR planetId = do
    (userId, _) <- requireAuthPair

    loadedPlanetReports <- runDB $ selectList [ PlanetReportOwnerId ==. Just userId 
                                              , PlanetReportPlanetId ==. planetId ] [ Asc PlanetReportPlanetId
                                                                                    , Asc PlanetReportDate ]

    let planetReports = filter (\x -> Just userId == cprOwnerId x) $ collatePlanets $ Import.map entityVal loadedPlanetReports
    let planetReport =  Data.List.head planetReports

    baseReports <- Import.mapM addBaseDetails planetReports

    loadedBuildings <- runDB $ selectList [ BuildingPlanetId ==. planetId ] []
    let buildings = Import.map entityVal loadedBuildings

    let expl = "Deep Sky - " ++ case (cprName planetReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown planet"

    defaultLayout $ do
        setTitle $ toMarkup expl
        $(widgetFile "base")

addBaseDetails :: CollatedPlanetReport -> Handler CollatedBaseReport
addBaseDetails planetReport = do
    system <- runDB $ get (cprSystemId planetReport)
    let systemName = case system of
                        Just starSystem -> (starSystemName starSystem)
                        Nothing -> ""
    return $ CollatedBaseReport planetReport systemName
