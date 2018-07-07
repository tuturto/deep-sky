{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Bases where

import Import
import Report
import Widgets

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

addBaseDetails :: CollatedPlanetReport -> Handler CollatedBaseReport
addBaseDetails planetReport = do
    system <- runDB $ get (cprSystemId planetReport)
    let systemName = case system of
                        Just starSystem -> (starSystemName starSystem)
                        Nothing -> ""
    return $ CollatedBaseReport planetReport systemName
