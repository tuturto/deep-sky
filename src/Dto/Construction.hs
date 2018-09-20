{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Dto.Construction 
  ( ConstructionDto(..), buildingConstructionToDto, shipConstructionToDto
  , constructionIndex )
  where

import CustomTypes (buildingTypeName, ShipType(..), BuildingType(..))
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
    BuildingConstructionDto { bcdtoIndex = index } -> index
    ShipConstructionDto { scdtoIndex = index } -> index

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
