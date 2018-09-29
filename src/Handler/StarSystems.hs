{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Handler.StarSystems where

import Import
import Text.Blaze.Html5 (toMarkup)
import Report ( collateSystems, collatePopulations, collateBuildings, collatePlanet
              , createStarLaneReports, createPlanetReports, createStarReports, createSystemReport
              , CollatedPopulationReport(..), CollatedPlanetReport(..), CollatedStarSystemReport(..) )
import Widgets
import MenuHelpers
import Database.Persist.Sql (fromSqlKey)
import qualified Database.Esqueleto as E
import Common (requireFaction, apiRequireFaction)

getApiStarSystemsR :: Handler Value
getApiStarSystemsR = do
    _ <- apiRequireFaction
    loadedSystemReports <- runDB $ selectList [] [ Asc StarSystemReportId
                                                 , Asc StarSystemReportDate ]
    let systemReports = collateSystems $ map entityVal loadedSystemReports
    return $ toJSON systemReports

getStarSystemsR :: Handler Html
getStarSystemsR = do
    (_, _, factionId) <- requireFaction

    loadedSystemReports <- runDB $ selectList [ StarSystemReportFactionId ==. factionId ] [ Asc StarSystemReportId
                                                                                          , Asc StarSystemReportDate ]
    let systemReports = collateSystems $ map entityVal loadedSystemReports
    defaultLayout $ do
        setTitle "Deep Sky - Star systems"
        $(widgetFile "starsystems")

getStarSystemR :: Key StarSystem -> Handler Html
getStarSystemR systemId = do
    (_, _, factionId) <- requireFaction
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
    (_, _, factionId) <- requireFaction
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId ==. planetId
                                              , PlanetReportFactionId ==. factionId ] [ Asc PlanetReportDate ]
    let planetReport = collatePlanet $ map entityVal loadedPlanetReports

    landedShips <- runDB $ planetShips planetId True
    orbitingShips <- runDB $ planetShips planetId False
    let planetKey = fromSqlKey planetId

    let expl = "Deep Sky - " ++ case (cprName planetReport) of
                                    (Just x) -> x
                                    Nothing  -> "unknown planet"
    defaultLayout $ do
        setTitle $ toMarkup expl 
        addScript $ StaticR js_planet_js
        addStylesheet $ StaticR css_site_css
        $(widgetFile "planet")

-- | Load ships that are on or around a given planet and their faction info
planetShips :: (MonadIO m, BackendCompatible SqlBackend backend,
                PersistQueryRead backend, PersistUniqueRead backend) =>
               Key Planet -> Bool -> ReaderT backend m [(Entity Ship, Entity Faction)]
planetShips pId landed = do
    E.select $
        E.from $ \(ship `E.InnerJoin` faction) -> do
                E.on (ship E.^. ShipOwnerId E.==. faction E.^. FactionId)
                E.where_ (ship E.^. ShipPlanetId E.==. E.val (Just pId)
                          E.&&. ship E.^. ShipLanded E.==. E.val landed)
                return (ship, faction)

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
    (_, _, fId) <- apiRequireFaction
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId ==. planetId
                                              , PlanetReportFactionId ==. fId ] [ Asc PlanetReportDate ]
    let planetReport = collatePlanet $ map entityVal loadedPlanetReports
    return $ toJSON planetReport

getApiPlanetBuildingsR :: Key Planet -> Handler Value
getApiPlanetBuildingsR planetId = do
    (_, _, fId) <- apiRequireFaction
    loadedBuildingReports <- runDB $ selectList [ BuildingReportPlanetId ==. planetId 
                                                , BuildingReportFactionId ==. fId ] [ Asc BuildingReportBuildingId
                                                                                    , Asc BuildingReportDate ]
    let buildingReports = collateBuildings $ map entityVal loadedBuildingReports
    return $ toJSON buildingReports

getApiPlanetPopulationR :: Key Planet -> Handler Value
getApiPlanetPopulationR planetId = do
    (_, _, fId) <- apiRequireFaction
    loadedPopReports <- runDB $ loadPlanetPopulationReports planetId fId
    let populationReports = collatePopulations $ map (\(a, b) -> (entityVal a, fmap entityVal b)) loadedPopReports
    return $ toJSON populationReports

-- | Load population reports of a planet and respective races
loadPlanetPopulationReports :: (MonadIO m, BackendCompatible SqlBackend backend,
                                PersistQueryRead backend, PersistUniqueRead backend) =>
                               Key Planet -> Key Faction -> ReaderT backend m [(Entity PlanetPopulationReport, Maybe (Entity Race))]
loadPlanetPopulationReports pId fId = do
    E.select $
        E.from $ \(popReport `E.LeftOuterJoin` pRace) -> do
            E.on (popReport E.^. PlanetPopulationReportRaceId E.==. pRace E.?. RaceId )
            E.where_ (popReport E.^. PlanetPopulationReportPlanetId E.==. E.val pId
                      E.&&. popReport E.^. PlanetPopulationReportFactionId E.==. E.val fId)
            E.orderBy [ E.asc ( popReport E.^. PlanetPopulationReportPlanetId) 
                      , E.asc ( popReport E.^. PlanetPopulationReportRaceId)
                      , E.asc ( popReport E.^. PlanetPopulationReportDate)
                      ]
            return (popReport, pRace)
