{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}

module Dto.Construction 
  ( ConstructionDto(..), buildingConstructionToDto, shipConstructionToDto
  , constructionIndex )
  where

import CustomTypes (buildingTypeName, ShipType(..), BuildingType(..))
import Resources (RawResources(..), RawResource(..), ConstructionLeft)
import Common (DtoTransform(..))
import Buildings (building, BLevel(..), buildingInfoCost)
import Data.Aeson (object, (.=))
import Import

data ConstructionDto = BuildingConstructionDto
    { bcdtoId :: Key BuildingConstruction
    , bcdtoName :: Text
    , bcdtoIndex :: Int
    , bcdtoLevel :: Int
    , bcdtoType :: BuildingType
    , bcdtoPlanet :: Key Planet
    , bcdtoCostLeft :: RawResources ConstructionLeft
    }
  | ShipConstructionDto
    { scdtoId :: Key ShipConstruction
    , scdtoName :: Text
    , scdtoShipType :: ShipType
    , scdtoIndex :: Int
    }
  deriving (Show, Read, Eq)

instance ToJSON ConstructionDto where
  toJSON bc@BuildingConstructionDto {} =
    object [ "id" .= bcdtoId bc
           , "name" .= bcdtoName bc
           , "index" .= bcdtoIndex bc
           , "level" .= bcdtoLevel bc
           , "type" .= bcdtoType bc
           , "planet" .= bcdtoPlanet bc
           , "costLeft" .= bcdtoCostLeft bc
           ]

  toJSON sc@ShipConstructionDto {} =
    object [ "id" .= scdtoId sc
           , "name" .= scdtoName sc
           , "shipType" .= scdtoShipType sc
           , "index" .= scdtoIndex sc
           ]

instance FromJSON ConstructionDto where
  parseJSON (Object v) =
    asum [ BuildingConstructionDto <$> v .: "id"
                                   <*> v .: "name"
                                   <*> v .: "index"
                                   <*> v .: "level"
                                   <*> v .: "type"
                                   <*> v .: "planet"
                                   <*> v .: "costLeft"
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
                            , bcdtoName = buildingTypeName $ buildingConstructionType bc
                            , bcdtoIndex = buildingConstructionIndex bc
                            , bcdtoLevel = buildingConstructionLevel bc
                            , bcdtoType = buildingConstructionType bc
                            , bcdtoPlanet = buildingConstructionPlanetId bc
                            , bcdtoCostLeft = RawResources mech bio chem
                            }
  where
    bc = entityVal bce
    key = entityKey bce
    template = building (buildingConstructionType bc) $ BLevel (buildingConstructionLevel bc)
    cost = buildingInfoCost template
    mech = RawResource $ unRawResource (ccdMechanicalCost cost) - buildingConstructionProgressMechanicals bc
    bio = RawResource $ unRawResource (ccdBiologicalCost cost) - buildingConstructionProgressBiologicals bc
    chem = RawResource $ unRawResource (ccdChemicalCost cost) - buildingConstructionProgressChemicals bc


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