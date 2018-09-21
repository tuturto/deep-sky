{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Handler.Bases where

import Import
import Report
import Handler.StarSystems
import Common (requireFaction)

getBasesR :: Handler Html
getBasesR = do
    (_, _, factionId) <- requireFaction

    loadedPlanets <- runDB $ selectList [ PlanetOwnerId ==. (Just factionId)] [ Asc PlanetName ]
    let planetIds = map entityKey loadedPlanets
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId <-. planetIds ] [ Asc PlanetReportPlanetId
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
