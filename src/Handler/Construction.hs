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
import Construction ( MaybeBuildingConstruction(..) )


getConstructionR :: Handler Html
getConstructionR =
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
    (newIndex, oldIndex, pId) <- validatePut msg cId
    let direction = if newIndex < oldIndex
                    then 1
                    else -1
    let upperBound = max oldIndex newIndex
    let lowerBound = min oldIndex newIndex
    newConstructions <- runDB $ do
                        -- move construction to the new index
                        _ <- update cId [ BuildingConstructionIndex =. newIndex ]
                        -- move building constructions that have their index between bounds
                        _ <- updateWhere [ BuildingConstructionPlanetId ==. pId
                                         , BuildingConstructionIndex >=. lowerBound
                                         , BuildingConstructionIndex <=. upperBound
                                         , BuildingConstructionId !=. cId ]
                                         [ BuildingConstructionIndex +=. direction ]
                        -- perform same move to ship constructions
                        _ <- updateWhere [ ShipConstructionPlanetId ==. Just pId
                                         , ShipConstructionIndex >=. lowerBound
                                         , ShipConstructionIndex <=. upperBound ]
                                         [ ShipConstructionIndex +=. direction ]
                        loadPlanetConstructionQueue $ bcdtoPlanet msg

    return $ toJSON newConstructions


-- | Delete building construction
deleteApiBuildingConstructionIdR :: Key BuildingConstruction -> Handler Value
deleteApiBuildingConstructionIdR cId = do
    _ <- apiRequireFaction
    loadedConst <- runDB $ get cId
    res <- runDB $ removeBuildingConstruction cId loadedConst
    return $ toJSON res



-- | Validate update message of building construction
--   Method will return new index, old index and key for planet if everything is ok
--   In case message doesn't actually move construction anywhere (old and new index are same)
--   http 200 is returned with current construction queue as content
--   In case there is problem with the message, appropriate http error will be returned
validatePut :: ConstructionDto -> Key BuildingConstruction -> HandlerFor App (Int, Int, Key Planet)
validatePut msg cId = do
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
    return (newIndex, oldIndex, pId)


-- | transform building construction dto into building construction and save is into database
createBuildingConstruction :: (PersistQueryRead backend, MonadIO m,
                               PersistStoreWrite backend, BaseBackend backend ~ SqlBackend) =>
                              ConstructionDto -> ReaderT backend m (Maybe (Key BuildingConstruction))
createBuildingConstruction cDto = do
    currentConstructions <- loadPlanetConstructionQueue $ bcdtoPlanet cDto
    let nextIndex = if P.length currentConstructions == 0
                        then 0
                        else P.maximum (map constructionIndex currentConstructions) + 1
    mapM (\x -> insert x { buildingConstructionIndex = nextIndex }) $ unMaybeBuildingConstruction (fromDto cDto)


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
    loadPlanetConstructionQueue $ buildingConstructionPlanetId buildingInfo


removeBuildingConstruction _ Nothing =
    return []

-- TODO:
-- and ships
-- and general clean up of code
