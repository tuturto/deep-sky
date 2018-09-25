{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module Handler.Construction 
    ( getConstructionR, getApiBuildingsR, getApiPlanetConstQueueR, getApiBuildingConstructionIdR
    , putApiBuildingConstructionIdR, deleteApiBuildingConstructionIdR, postApiBuildingConstructionR
    )
    where

import Import
import qualified Prelude as P ( maximum, length )
import Common ( apiRequireFaction, fromDto, apiNotFound, apiInvalidArgs, apiOk )
import Buildings (building, BLevel(..))
import CustomTypes (BuildingType(..))
import Data.Aeson (ToJSON(..))
import Dto.Construction ( buildingConstructionToDto, shipConstructionToDto, ConstructionDto(..)
                        , constructionIndex )

getConstructionR :: Handler Html
getConstructionR = do
    defaultLayout $ do
        setTitle "Deep Sky - Construction"
        $(widgetFile "construction")

-- | Retrieve list of buildings available for given faction as JSON
--   In case multiple levels of a building are available, all are reported
getApiBuildingsR :: Handler Value
getApiBuildingsR = do
    _ <- apiRequireFaction
    let json = toJSON [ building SensorStation $ BLevel 1
                      , building ResearchComplex $ BLevel 1
                      , building Farm $ BLevel 1
                      , building ParticleAccelerator $ BLevel 1
                      , building NeutronDetector $ BLevel 1
                      , building BlackMatterScanner $ BLevel 1
                      , building GravityWaveSensor $ BLevel 1
                      ]
    return json

-- | Retrieve construction queue of a given planet as JSON
getApiPlanetConstQueueR :: Key Planet -> Handler Value
getApiPlanetConstQueueR planetId = do
    _ <- apiRequireFaction
    constructions <- runDB $ loadPlanetConstructionQueue planetId
    return $ toJSON constructions

-- | Retrieve details of given building construction
getApiBuildingConstructionIdR :: Key BuildingConstruction -> Handler Value
getApiBuildingConstructionIdR cId = do
    _ <- apiRequireFaction
    loadedConst <- runDB $ get cId
    construction <- case loadedConst of
                        Just x -> return x
                        Nothing -> apiNotFound
    return $ toJSON construction

-- | Create a new building construction
--   In case this method is called to insert ship nothing will be inserted
--   Returns current construction queue of the planet after the insert
postApiBuildingConstructionR :: Handler Value
postApiBuildingConstructionR = do
    _ <- apiRequireFaction
    msg <- requireJsonBody
    _ <- runDB $ createBuildingConstruction msg
    newConstructions <- runDB $ loadPlanetConstructionQueue $ bcdtoPlanet msg
    return $ toJSON newConstructions

-- | Update existing building construction
--   In case this method is called to update ship construction http 400 error will be returned
putApiBuildingConstructionIdR :: Key BuildingConstruction -> Handler Value
putApiBuildingConstructionIdR cId = do
    _ <- apiRequireFaction
    msg <- requireJsonBody
    newIndex <- if bcdtoIndex msg < 0
                then apiInvalidArgs [ "negative building index" ]
                else return $ bcdtoIndex msg
    loadedConst <- case msg of
                    ShipConstructionDto {} -> apiInvalidArgs [ "body contains ship" ]
                    BuildingConstructionDto {} -> runDB $ get cId
    construction <- case loadedConst of
                        Just x -> return x
                        Nothing -> apiNotFound
    let oldIndex = buildingConstructionIndex construction
    _ <- if oldIndex == newIndex
         then do
            queue <- runDB $ loadPlanetConstructionQueue $ bcdtoPlanet msg
            apiOk queue
         else return []
    let pId = buildingConstructionPlanetId construction
    -- TODO: clean up validation (newIndex, oldIndex, pId are needed values)
    -- TODO: make update a separate function
    newConstructions <- if newIndex < oldIndex
                        then runDB $ do
                            -- move construction to the new index
                            _ <- update cId [ BuildingConstructionIndex =. newIndex ]
                            -- move building constructions +1 if
                            -- a) they have smaller index than the old one
                            -- b) they have larger index than the new one
                            _ <- updateWhere [ BuildingConstructionPlanetId ==. pId
                                             , BuildingConstructionIndex >=. newIndex
                                             , BuildingConstructionIndex <. oldIndex
                                             , BuildingConstructionId !=. cId ]
                                             [ BuildingConstructionIndex +=. 1 ]
                            -- perform same move to ship constructions
                            _ <- updateWhere [ ShipConstructionPlanetId ==. Just pId
                                             , ShipConstructionIndex >=. newIndex 
                                             , ShipConstructionIndex <. oldIndex]
                                             [ ShipConstructionIndex +=. 1 ]
                            loadPlanetConstructionQueue $ bcdtoPlanet msg
                        else runDB $ do
                            -- TODO: go through this section once more and comment it
                            _ <- update cId [ BuildingConstructionIndex =. newIndex ]
                            _ <- updateWhere [ BuildingConstructionPlanetId ==. pId
                                             , BuildingConstructionIndex >=. newIndex
                                             , BuildingConstructionId !=. cId ]
                                             [ BuildingConstructionIndex +=. 1 ]
                            _ <- updateWhere [ BuildingConstructionPlanetId ==. pId
                                             , BuildingConstructionIndex >=. oldIndex
                                             , BuildingConstructionIndex <. newIndex
                                             , BuildingConstructionId !=. cId ]
                                             [ BuildingConstructionIndex -=. 1 ]
                            _ <- updateWhere [ ShipConstructionPlanetId ==. Just pId
                                             , ShipConstructionIndex >=. newIndex ]
                                             [ ShipConstructionIndex +=. 1 ]
                            _ <- updateWhere [ ShipConstructionPlanetId ==. Just pId
                                             , ShipConstructionIndex >=. oldIndex
                                             , ShipConstructionIndex <. newIndex ]
                                             [ ShipConstructionIndex -=. 1 ]
                            loadPlanetConstructionQueue $ bcdtoPlanet msg

    return $ toJSON newConstructions

-- | Delete building construction
deleteApiBuildingConstructionIdR :: Key BuildingConstruction -> Handler Value
deleteApiBuildingConstructionIdR cId = do
    _ <- apiRequireFaction
    loadedConst <- runDB $ get cId
    res <- runDB $ removeBuildingConstruction cId loadedConst
    return $ toJSON res



-- | transform building construction dto into building construction and save is into database
createBuildingConstruction :: (PersistQueryRead backend, MonadIO m,
                               PersistStoreWrite backend, BaseBackend backend ~ SqlBackend) =>
                              ConstructionDto -> ReaderT backend m (Maybe (Key BuildingConstruction))
createBuildingConstruction cDto = do
    currentConstructions <- loadPlanetConstructionQueue $ bcdtoPlanet cDto
    let nextIndex = if P.length currentConstructions == 0
                        then 0
                        else (P.maximum $ map constructionIndex currentConstructions) + 1
    mapM (\x -> insert x { buildingConstructionIndex = nextIndex }) $ fromDto cDto

-- | load construction queue of a given planet
loadPlanetConstructionQueue :: (PersistQueryRead backend,
                                MonadIO m, BaseBackend backend ~ SqlBackend) =>
                                Key Planet -> ReaderT backend m [ConstructionDto]
loadPlanetConstructionQueue planetId = do
    loadedBuildings <- selectList [ BuildingConstructionPlanetId ==. planetId ] []
    loadedShips <- selectList [ ShipConstructionPlanetId ==. Just planetId ] []
    let buildings = map buildingConstructionToDto loadedBuildings
    let ships = map shipConstructionToDto loadedShips
    let constructions = buildings ++ ships
    return constructions

-- | Remove building construction from database and update indexes of other buildings in the queue
removeBuildingConstruction :: (MonadIO m, PersistEntity record, PersistQueryWrite backend,
                               PersistEntityBackend record ~ BaseBackend backend,
                               BaseBackend backend ~ SqlBackend) =>
                               Key record -> Maybe BuildingConstruction -> ReaderT backend m [ConstructionDto]
removeBuildingConstruction bId (Just buildingInfo) = do
    let bIndex = buildingConstructionIndex buildingInfo
    let planetId = buildingConstructionPlanetId buildingInfo
    _ <- delete bId
    _ <- updateWhere [ BuildingConstructionPlanetId ==. planetId
                     , BuildingConstructionIndex >. bIndex ] [ BuildingConstructionIndex -=. 1 ]
    _ <- updateWhere [ ShipConstructionPlanetId ==. Just planetId
                     , ShipConstructionIndex >. bIndex ] [ ShipConstructionIndex -=. 1 ]
    newQueue <- loadPlanetConstructionQueue $ buildingConstructionPlanetId buildingInfo
    return newQueue

removeBuildingConstruction _ Nothing = do
    return []
 
-- TODO:
-- move building up in queue (put)
-- move building down in queue (put)
-- and ships
-- and general clean up of code

-- DONE:
-- load available buildings
-- load current construction queue
-- add building into queue (post)
-- remove building from queue (delete)
