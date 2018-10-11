{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Buildings where

import Data.Aeson (object, (.=), ToJSON(..))
import CustomTypes (RawResources(..), BuildingType(..), RawResource(..), ResourceCost)
import ClassyPrelude.Yesod   as Import

-- Types

data BuildingInfo = BuildingInfo
    { buildingInfoType :: BuildingType
    , buildingInfoName :: Text
    , buildingInfoLevel :: BLevel
    , buildingInfoCost :: RawResources ResourceCost
    , buildingInfoDescription :: Text
    }
    deriving (Show, Read, Eq)

newtype BLevel = BLevel { unBLevel :: Int }
    deriving (Show, Read, Eq)

-- Instances

instance ToJSON BuildingInfo where
    toJSON (BuildingInfo bType bName bLevel bCost bDesc) = 
        object [ "id" .= bType
               , "level" .= unBLevel bLevel
               , "name" .= bName
               , "cost" .= bCost
               , "description" .= bDesc
               ]

building :: BuildingType -> BLevel -> BuildingInfo
building SensorStation level =
    BuildingInfo SensorStation "Sensor station" level (RawResources (RawResource 250) (RawResource 50) (RawResource 50))
        "Collection of various sensors, designed to scan the space"
building Farm level =
    BuildingInfo Farm "Farm" level (RawResources (RawResource 100) (RawResource 100) (RawResource 50))
        "Hydrophonic fram producing fresh food and other biological matter"
building ResearchComplex level =
    BuildingInfo ResearchComplex "Research complex" level (RawResources (RawResource 250) (RawResource 50) (RawResource 250))
        "High-tech research station filled with databanks and computers"
building NeutronDetector level =
    BuildingInfo NeutronDetector "Neutron detector" level (RawResources (RawResource 500) (RawResource 10) (RawResource 250))
        "Sensitive detected hidden deep inside a mountain, capable of detecting neutrons"
building ParticleAccelerator level =
    BuildingInfo ParticleAccelerator "Particle accelerator" level (RawResources (RawResource 500) (RawResource 10) (RawResource 250))
        "Large particle collider used to research origins of matter"
building BlackMatterScanner level =
    BuildingInfo BlackMatterScanner "Black matter scanner" level (RawResources (RawResource 750) (RawResource 50) (RawResource 500))
        "Super sensitive network of sensors and computers used to scan universe for black matter"
building GravityWaveSensor level =
    BuildingInfo GravityWaveSensor "Gravity wave sensor" level (RawResources (RawResource 1000) (RawResource 50) (RawResource 750))
        "Massive network of sensors used to hunt for gravity waves"
