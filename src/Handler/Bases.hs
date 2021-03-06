{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Handler.Bases where

import Import
import Report
import Common (requireFaction)

getBasesR :: Handler Html
getBasesR = do
    (_, _, _, factionId) <- requireFaction

    loadedPlanets <- runDB $ selectList [ PlanetOwnerId ==. Just factionId] [ Asc PlanetName ]
    let planetIds = map entityKey loadedPlanets
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId <-. planetIds ] [ Asc PlanetReportPlanetId
                                                                                     , Asc PlanetReportDate ]

    let planetReports = filter (\x -> Just factionId == cprOwnerId x) $ collateReports $ map entityVal loadedPlanetReports
    _ <- mapM addBaseDetails planetReports

    defaultLayout $ do
        setTitle "Deep Sky - Bases"
        $(widgetFile "bases")

getBaseR :: StarSystemId -> PlanetId -> Handler Html
getBaseR = undefined

addBaseDetails :: CollatedPlanetReport -> Handler CollatedBaseReport
addBaseDetails planetReport = do
    system <- runDB $ get (cprSystemId planetReport)
    let systemName = case system of
                        Just starSystem -> starSystemName starSystem
                        Nothing -> ""
    return $ CollatedBaseReport planetReport systemName
