{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Handler.Bases where

import Import
import Report
import Handler.StarSystems

getBasesR :: Handler Html
getBasesR = do
    (_, user) <- requireAuthPair
    factionId <- case (userFactionId user) of
        Just x -> return x
        Nothing -> redirect ProfileR

    loadedPlanetReports <- runDB $ selectList [ PlanetReportFactionId ==. factionId ] [ Asc PlanetReportPlanetId
                                                                                      , Asc PlanetReportDate ]

    let planetReports = filter (\x -> (Just factionId) == cprOwnerId x) $ collatePlanets $ map entityVal loadedPlanetReports
    baseReports <- mapM addBaseDetails planetReports

    defaultLayout $ do
        setTitle "Deep Sky - Bases"
        $(widgetFile "bases")

getBaseR :: Key StarSystem -> Key Planet -> Handler Html
getBaseR = getPlanetR

addBaseDetails :: CollatedPlanetReport -> Handler CollatedBaseReport
addBaseDetails planetReport = do
    system <- runDB $ get (cprSystemId planetReport)
    let systemName = case system of
                        Just starSystem -> (starSystemName starSystem)
                        Nothing -> ""
    return $ CollatedBaseReport planetReport systemName
