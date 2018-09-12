{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Handler.StarSystems where

import Import
import Text.Blaze.Html5 (toMarkup)
import Report ( collateSystems, collatePopulations, collateBuildings, collatePlanet
              , createStarLaneReports, createPlanetReports, createStarReports, createSystemReport
              , CollatedPopulationReport(..), CollatedPlanetReport(..), CollatedStarSystemReport(..) )
import Widgets
import MenuHelpers
import Data.Maybe (fromJust)
import Database.Persist.Sql (fromSqlKey)

getApiStarSystemsR :: Handler Value
getApiStarSystemsR = do
    loadedSystemReports <- runDB $ selectList [] [ Asc StarSystemReportId
                                                 , Asc StarSystemReportDate ]
    let systemReports = collateSystems $ map entityVal loadedSystemReports
    return $ toJSON systemReports

getStarSystemsR :: Handler Html
getStarSystemsR = do
    (_, user) <- requireAuthPair   
    factionId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR

    loadedSystemReports <- runDB $ selectList [ StarSystemReportFactionId ==. factionId ] [ Asc StarSystemReportId
                                                                                          , Asc StarSystemReportDate ]
    let systemReports = collateSystems $ map entityVal loadedSystemReports
    defaultLayout $ do
        setTitle "Deep Sky - Star systems"
        $(widgetFile "starsystems")

getStarSystemR :: Key StarSystem -> Handler Html
getStarSystemR systemId = do
    (_, user) <- requireAuthPair   
    factionId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR

    systemReport <- runDB $ createSystemReport systemId factionId
    starReports <- runDB $ createStarReports systemId factionId
    planetReports <- runDB $ createPlanetReports systemId factionId
    starLaneReports <- runDB $ createStarLaneReports systemId factionId

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
    let planetReport = collatePlanet $ map entityVal loadedPlanetReports

    factions <- runDB $ selectList [] [ Asc FactionId ]
    loadLandedShips <- runDB $ selectList [ ShipPlanetId ==. Just planetId 
                                          , ShipLanded ==. True ] []
    let landedShips = fillFactions factions loadLandedShips
    loadOrbitingShips <- runDB $ selectList [ ShipPlanetId ==. Just planetId 
                                            , ShipLanded ==. False ] []
    let orbitingShips = fillFactions factions loadOrbitingShips
    let planetKey = fromSqlKey planetId

    let expl = "Deep Sky - " ++ case (cprName planetReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown planet"
    defaultLayout $ do
        setTitle $ toMarkup expl 
        addScript $ StaticR js_planet_js
        addStylesheet $ StaticR css_site_css
        $(widgetFile "planet")

-- | match entries in given faction and ship lists, producing (Ship, Faction) tuples
-- This is temporary fix, until esqueleto is included in stack
fillFactions :: (SemiSequence seq, Functor f,
                       Element seq ~ Entity Faction) =>
                      seq -> f (Entity Ship) -> f (Ship, Faction)
fillFactions factions ships =
    map fn ships
        where fn eShip = (entityVal eShip, entityVal $ fromJust faction)
                where faction = find compareIds factions
                      compareIds f = (shipOwnerId $ entityVal eShip) == (entityKey f)

addPopulationDetails :: (BaseBackend backend ~ SqlBackend,
    MonadIO m, PersistStoreRead backend) =>
    CollatedPopulationReport -> ReaderT backend m CollatedPopulationReport
addPopulationDetails report = do
    aRace <- getMaybeEntity $ cpopRaceId report
    res <- case aRace of
                (Just x) -> return $ report { cpopRace = Just $ raceName x}
                Nothing  -> return report
    return res

getApiPlanetR :: Key Planet -> Handler Value
getApiPlanetR planetId = do
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId ==. planetId
                                              , PlanetReportFactionId ==. fId ] [ Asc PlanetReportDate ]
    let planetReport = collatePlanet $ map entityVal loadedPlanetReports
    return $ toJSON planetReport

getApiPlanetBuildingsR :: Key Planet -> Handler Value
getApiPlanetBuildingsR planetId = do
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    loadedBuildingReports <- runDB $ selectList [ BuildingReportPlanetId ==. planetId 
                                                , BuildingReportFactionId ==. fId ] [ Asc BuildingReportBuildingId
                                                                                    , Asc BuildingReportDate ]
    let buildingReports = collateBuildings $ map entityVal loadedBuildingReports
    return $ toJSON buildingReports

getApiPlanetPopulationR :: Key Planet -> Handler Value
getApiPlanetPopulationR planetId = do
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    loadedPopulationReports <- runDB $ selectList [ PlanetPopulationReportPlanetId ==. planetId
                                                  , PlanetPopulationReportFactionId ==. fId ] [ Asc PlanetPopulationReportPlanetId
                                                                                              , Asc PlanetPopulationReportRaceId
                                                                                              , Asc PlanetPopulationReportDate ]
    let partialPopulationReports = collatePopulations $ map entityVal loadedPopulationReports
    populationReports <- runDB $ mapM addPopulationDetails partialPopulationReports
    return $ toJSON populationReports
