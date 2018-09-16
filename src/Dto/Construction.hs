{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Dto.Construction (ConstructionDto(..), buildingConstructionToDto, shipConstructionToDto)
  where

import CustomTypes (buildingTypeName, ShipType(..))
import Data.Aeson (object, (.=), (.:?))
import Import

data ConstructionDto = BuildingConstructionDto
    { bcdtoId :: Key BuildingConstruction
    , bcdtoName :: Text
    , bcdtoIndex :: Int
    }
  | ShipConstructionDto
    { scdtoId :: Key ShipConstruction
    , scdtoName :: Text
    , scdtoShipType :: ShipType
    , scdtoIndex :: Int
    }
  deriving (Show, Read, Eq)

instance ToJSON ConstructionDto where
  toJSON (BuildingConstructionDto bId bName bIndex) =
    object [ "id" .= bId
           , "name" .= bName
           , "index" .= bIndex
           ]

  toJSON (ShipConstructionDto sId sName sType sIndex) =
    object [ "id" .= sId
           , "name" .= sName
           , "shipType" .= sType
           , "index" .= sIndex
           ]

buildingConstructionToDto :: Entity BuildingConstruction -> ConstructionDto
buildingConstructionToDto bce =
  BuildingConstructionDto key (buildingTypeName $ buildingConstructionType bc)
    (buildingConstructionIndex bc)
  where
    bc = entityVal bce
    key = entityKey bce

shipConstructionToDto :: Entity ShipConstruction -> ConstructionDto
shipConstructionToDto sce =
  ShipConstructionDto key "TODO: ship name" Destroyer (shipConstructionIndex sc)
  where
    sc = entityVal sce
    key = entityKey sce
