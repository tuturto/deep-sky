{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Dto.Construction (ConstructionDto(..), buildingConstructionToDto)
  where

import Data.Aeson (object, (.=), (.:?))
import Import

data ConstructionDto = BuildingConstructionDto
    { bcdtoId :: Key BuildingConstruction
    , bcdtoName :: Text
    }
  | ShipConstructionDto
    { scdtoName :: Text
    }
  deriving (Show, Read, Eq)

instance ToJSON ConstructionDto where
  toJSON (BuildingConstructionDto bId bName) =
    object [ "id" .= bId
           , "name" .= bName
           ]

  toJSON (ShipConstructionDto name) =
    object [ "name" .= name
           ]

buildingConstructionToDto :: Entity BuildingConstruction -> ConstructionDto
buildingConstructionToDto bce =
  BuildingConstructionDto key "TODO: name"
  where
    bc = entityVal bce
    key = entityKey bce
