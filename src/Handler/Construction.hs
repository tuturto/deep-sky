{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Construction (getConstructionR, getApiBuildingsR, getApiPlanetConstQueueR)
    where

import Import
import Buildings (building, BLevel(..))
import CustomTypes (BuildingType(..))
import Data.Aeson (ToJSON(..))
import Dto.Construction (buildingConstructionToDto)

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
    loadedConstructions <- runDB $ selectList [ BuildingConstructionPlanetId ==. planetId ] []
    return $ toJSON $ map buildingConstructionToDto loadedConstructions

-- TODO:
-- load current construction queue
-- add building into queue
-- remove building from queue
-- move building up in queue
-- move building down in queue
-- and ships

-- DONE:
-- load available buildings
