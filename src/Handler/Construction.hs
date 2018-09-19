{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Construction ( getConstructionR, getApiBuildingsR, getApiPlanetConstQueueR
                            , getApiBuildingConstructionIdR, putApiBuildingConstructionIdR
                            , deleteApiBuildingConstructionIdR, postApiBuildingConstructionR )
    where

import Import
import Buildings (building, BLevel(..))
import CustomTypes (BuildingType(..))
import Data.Aeson (ToJSON(..))
import Dto.Construction (buildingConstructionToDto, shipConstructionToDto, ConstructionDto(..))

getConstructionR :: Handler Html
getConstructionR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    defaultLayout $ do
        setTitle "Deep Sky - Construction"
        $(widgetFile "construction")

-- | Retrieve list of buildings available for given faction as JSON
--   In case multiple levels of a building are available, all are reported
getApiBuildingsR :: Handler Value
getApiBuildingsR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
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
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    loadedBuildings <- runDB $ selectList [ BuildingConstructionPlanetId ==. planetId ] []
    loadedShips <- runDB $ selectList [ ShipConstructionPlanetId ==. Just planetId ] []
    let buildings = map buildingConstructionToDto loadedBuildings
    let ships = map shipConstructionToDto loadedShips
    let constructions = buildings ++ ships
    return $ toJSON constructions

-- | Retrieve details of given building construction
getApiBuildingConstructionIdR :: Key BuildingConstruction -> Handler Value
getApiBuildingConstructionIdR cId = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    loadedConst <- runDB $ get cId
    construction <- case loadedConst of
                        Just x -> return x
                        Nothing -> notFound
    return $ toJSON construction

-- | Create a new building construction
--   In case this method is called to insert ship construction http 400 error will be returned
--   Returns current construction queue of the planet after the insert
postApiBuildingConstructionR :: Handler Value
postApiBuildingConstructionR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    msg <- requireJsonBody
    _ <- runDB $ insert $ dtoToBuildingConstruction msg
    -- load construction queue and return it
    return $ toJSON msg

dtoToBuildingConstruction :: ConstructionDto -> BuildingConstruction
dtoToBuildingConstruction cDto =
    case cDto of
        BuildingConstructionDto bId bName bIndex bLevel bType bPlanetId ->
            BuildingConstruction { buildingConstructionPlanetId = bPlanetId
                                 , buildingConstructionIndex = bIndex
                                 , buildingConstructionProgressBiologicals = 0
                                 , buildingConstructionProgressMechanicals = 0
                                 , buildingConstructionProgressChemicals = 0
                                 , buildingConstructionType = bType
                                 , buildingConstructionLevel = bLevel
                                 }

-- | Update existing building construction
--   In case this method is called to update ship construction http 400 error will be returned
putApiBuildingConstructionIdR :: Key BuildingConstruction -> Handler Value
putApiBuildingConstructionIdR cId = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
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
deleteApiBuildingConstructionIdR :: Key BuildingConstruction ->Handler Value
deleteApiBuildingConstructionIdR cId = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    msg <- requireJsonBody
    --loadedConst <- runDB $ get cId
    --construction <- case loadedConst of
    --                    Just x -> return x
    --                    Nothing -> notFound
    return $ toJSON (msg :: Text)

-- 

-- TODO:
-- add building into queue (post)
-- remove building from queue (delete)
-- move building up in queue (put)
-- move building down in queue (put)
-- and ships

-- DONE:
-- load available buildings
-- load current construction queue