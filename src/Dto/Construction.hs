{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}

module Dto.Construction 
  ( ConstructionDto(..), buildingConstructionToDto, shipConstructionToDto
  , constructionIndex )
  where

import CustomTypes (buildingTypeName, ShipType(..), BuildingType(..))
import Common (DtoTransform(..))
import Data.Aeson (object, (.=))
import Import

data ConstructionDto = BuildingConstructionDto
    { bcdtoId :: Key BuildingConstruction
    , bcdtoName :: Text
    , bcdtoIndex :: Int
    , bcdtoLevel :: Int
    , bcdtoType :: BuildingType
    , bcdtoPlanet :: Key Planet
    }
  | ShipConstructionDto
    { scdtoId :: Key ShipConstruction
    , scdtoName :: Text
    , scdtoShipType :: ShipType
    , scdtoIndex :: Int
    }
  deriving (Show, Read, Eq)

instance ToJSON ConstructionDto where
  toJSON (BuildingConstructionDto bId bName bIndex bLevel bType bPlanet) =
    object [ "id" .= bId
           , "name" .= bName
           , "index" .= bIndex
           , "level" .= bLevel
           , "type" .= bType
           , "planet" .= bPlanet
           ]

  toJSON (ShipConstructionDto sId sName sType sIndex) =
    object [ "id" .= sId
           , "name" .= sName
           , "shipType" .= sType
           , "index" .= sIndex
           ]

instance FromJSON ConstructionDto where
  parseJSON (Object v) =
    asum [ BuildingConstructionDto <$> v .: "id"
                                   <*> v .: "name"
                                   <*> v .: "index"
                                   <*> v .: "level"
                                   <*> v .: "type"
                                   <*> v .: "planet"
         , ShipConstructionDto <$> v .: "id"
                               <*> v .: "name"
                               <*> v .: "shipType"
                               <*> v .: "index"
         ]
  parseJSON _ = mzero

constructionIndex :: ConstructionDto -> Int
constructionIndex c =
  case c of
    BuildingConstructionDto { bcdtoIndex = x } -> x
    ShipConstructionDto { scdtoIndex = x } -> x

buildingConstructionToDto :: Entity BuildingConstruction -> ConstructionDto
buildingConstructionToDto bce =
    BuildingConstructionDto { bcdtoId = key
                            , bcdtoName = (buildingTypeName $ buildingConstructionType bc)
                            , bcdtoIndex = (buildingConstructionIndex bc)
                            , bcdtoLevel = (buildingConstructionLevel bc)
                            , bcdtoType = (buildingConstructionType bc)
                            , bcdtoPlanet = (buildingConstructionPlanetId bc)
                            }
  where
    bc = entityVal bce
    key = entityKey bce

shipConstructionToDto :: Entity ShipConstruction -> ConstructionDto
shipConstructionToDto sce =
  ShipConstructionDto key "TODO: ship name" Destroyer (shipConstructionIndex sc)
  where
    sc = entityVal sce
    key = entityKey sce

instance DtoTransform ConstructionDto (Maybe BuildingConstruction) where
    fromDto dto = 
        case dto of
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