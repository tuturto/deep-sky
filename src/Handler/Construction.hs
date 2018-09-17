{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Construction ( getConstructionR, getApiBuildingsR, getApiPlanetConstQueueR
                            , getApiBuildingConstructionR, putApiBuildingConstructionR
                            , deleteApiBuildingConstructionR, postApiPostBuildingConstructionR )
    where

import Import
import Buildings (building, BLevel(..))
import CustomTypes (BuildingType(..))
import Data.Aeson (ToJSON(..))
import Dto.Construction (buildingConstructionToDto, shipConstructionToDto)

getConstructionR :: Handler Html
getConstructionR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    defaultLayout $ do
        setTitle "Deep Sky - Construction"
        $(widgetFile "construction")

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

getApiBuildingConstructionR :: Key BuildingConstruction -> Handler Value
getApiBuildingConstructionR cId = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    loadedConst <- runDB $ get cId
    construction <- case loadedConst of
                        Just x -> return x
                        Nothing -> notFound
    return $ toJSON construction

postApiPostBuildingConstructionR :: Handler Value
postApiPostBuildingConstructionR = do
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

putApiBuildingConstructionR :: Key BuildingConstruction -> Handler Value
putApiBuildingConstructionR cId = do
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

deleteApiBuildingConstructionR :: Key BuildingConstruction ->Handler Value
deleteApiBuildingConstructionR cId = do
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