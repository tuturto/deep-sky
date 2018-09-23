{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Handler.Construction 
    ( getConstructionR, getApiBuildingsR, getApiPlanetConstQueueR, getApiBuildingConstructionIdR
    , putApiBuildingConstructionIdR, deleteApiBuildingConstructionIdR, postApiBuildingConstructionR
    )
    where

import Import
import qualified Prelude as P ( maximum, length )
import Common (apiRequireFaction)
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
                        Nothing -> notFound
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
    loadedConst <- case msg of
                    ShipConstructionDto {} -> invalidArgs [ "body" ]
                    BuildingConstructionDto {} -> runDB $ get cId
    construction <- case loadedConst of
                        Just x -> return x
                        Nothing -> notFound
    -- update construction data
    -- save construction
    -- load construction
    -- return construction
    return $ toJSON construction

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
    let construction = dtoToBuildingConstruction cDto
    mapM (\x -> insert x { buildingConstructionIndex = nextIndex }) construction
    
    -- | Translate construction dto into building construction
--   In case construction dto is for a ship, Nothing is returned
dtoToBuildingConstruction :: ConstructionDto -> Maybe BuildingConstruction
dtoToBuildingConstruction cDto =
    case cDto of
        BuildingConstructionDto { bcdtoIndex = bIndex
                                , bcdtoLevel = bLevel
                                , bcdtoType = bType
                                , bcdtoPlanet = bPlanetId
                                } ->
            Just $ BuildingConstruction { buildingConstructionPlanetId = bPlanetId
                                        , buildingConstructionIndex = bIndex
                                        , buildingConstructionProgressBiologicals = 0
                                        , buildingConstructionProgressMechanicals = 0
                                        , buildingConstructionProgressChemicals = 0
                                        , buildingConstructionType = bType
                                        , buildingConstructionLevel = bLevel
                                        }
        ShipConstructionDto {} -> Nothing

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
