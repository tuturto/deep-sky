{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Construction 
    ( getConstructionR, getApiBuildingsR, getApiPlanetConstQueueR, getApiBuildingConstructionIdR
    , putApiBuildingConstructionIdR, deleteApiBuildingConstructionIdR, postApiBuildingConstructionR )
    where

import Import
import qualified Prelude as P (maximum)
import Common (requireFaction)
import Buildings (building, BLevel(..))
import CustomTypes (BuildingType(..))
import Data.Aeson (ToJSON(..))
import Dto.Construction ( buildingConstructionToDto, shipConstructionToDto, ConstructionDto(..)
                        , constructionIndex )

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
    _ <- requireFaction
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
    _ <- requireFaction
    constructions <- runDB $ loadPlanetConstructionQueue planetId
    return $ toJSON constructions

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

-- | Retrieve details of given building construction
getApiBuildingConstructionIdR :: Key BuildingConstruction -> Handler Value
getApiBuildingConstructionIdR cId = do
    _ <- requireFaction
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
    _ <- requireFaction
    msg <- requireJsonBody
    currentConstructions <- runDB $ loadPlanetConstructionQueue $ bcdtoPlanet msg
    let nextIndex = (P.maximum $ map constructionIndex currentConstructions) + 1
    let construction = dtoToBuildingConstruction msg
    _ <- mapM (\x -> runDB $ insert x { buildingConstructionIndex = nextIndex }) construction
    newConstructions <- runDB $ loadPlanetConstructionQueue $ bcdtoPlanet msg
    return $ toJSON newConstructions

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

-- | Update existing building construction
--   In case this method is called to update ship construction http 400 error will be returned
putApiBuildingConstructionIdR :: Key BuildingConstruction -> Handler Value
putApiBuildingConstructionIdR cId = do
    _ <- requireFaction
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
deleteApiBuildingConstructionIdR _ = do
    _ <- requireFaction
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