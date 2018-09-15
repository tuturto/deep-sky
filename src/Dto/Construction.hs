{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Dto.Construction (ConstructionDto(..), buildingConstructionToDto)
  where

import CustomTypes (buildingTypeName)
import Data.Aeson (object, (.=), (.:?))
import Import

data ConstructionDto = BuildingConstructionDto
    { bcdtoId :: Key BuildingConstruction
    , bcdtoName :: Text
    }
  | ShipConstructionDto
    { scdtoId :: Key ShipConstruction
    , scdtoName :: Text
    }
  deriving (Show, Read, Eq)

instance ToJSON ConstructionDto where
  toJSON (BuildingConstructionDto bId bName) =
    object [ "id" .= bId
           , "name" .= bName
           ]

  toJSON (ShipConstructionDto sId sName) =
    object [ "id" .= sId
           , "name" .= sName
           ]

buildingConstructionToDto :: Entity BuildingConstruction -> ConstructionDto
buildingConstructionToDto bce =
  BuildingConstructionDto key (buildingTypeName $ buildingConstructionType bc)
  where
    bc = entityVal bce
    key = entityKey bce

shipConstructionToDto :: Entity ShipConstruction -> ConstructionDto
shipConstructionToDto sce =
  ShipConstructionDto key "TODO: name"
  where
    bc = entityVal sce
    key = entityKey sce
