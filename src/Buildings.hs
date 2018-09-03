{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Buildings where

import Data.Aeson (object, (.=), ToJSON(..))
import Data.Aeson.TH 
import CustomTypes (TotalCost(..), BuildingType(..), Cost(..))
import Database.Persist.TH
import ClassyPrelude.Yesod   as Import

-- Types

data BuildingInfo = BuildingInfo
    { buildingInfoType :: BuildingType
    , buildingInfoName :: Text
    , buildingInfoLevel :: BLevel
    , buildingInfoCost :: TotalCost
    , buildingInfoDescription :: Text
    }
    deriving (Show, Read, Eq)

data BLevel = BLevel { unBLevel :: Int }
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
    BuildingInfo SensorStation "Sensor station" level (TotalCost (Cost 250) (Cost 50) (Cost 50))
        "Collection of various sensors, designed to scan the space"
building Farm level =
    BuildingInfo Farm "Farm" level (TotalCost (Cost 100) (Cost 100) (Cost 50))
        "Hydrophonic fram producing fresh food and other biological matter"
building ResearchComplex level =
    BuildingInfo ResearchComplex "Research complex" level (TotalCost (Cost 250) (Cost 50) (Cost 250))
        "High-tech research station filled with databanks and computers"
building NeutronDetector level =
    BuildingInfo NeutronDetector "Neutron detector" level (TotalCost (Cost 500) (Cost 10) (Cost 250))
        "Sensitive detected hidden deep inside a mountain, capable of detecting neutrons"
building ParticleAccelerator level =
    BuildingInfo ParticleAccelerator "Particle accelerator" level (TotalCost (Cost 500) (Cost 10) (Cost 250))
        "Large particle collider used to research origins of matter"
building BlackMatterScanner level =
    BuildingInfo BlackMatterScanner "Black matter scanner" level (TotalCost (Cost 750) (Cost 50) (Cost 500))
        "Super sensitive network of sensors and computers used to scan universe for black matter"
building GravityWaveSensor level =
    BuildingInfo GravityWaveSensor "Gravity wave sensor" level (TotalCost (Cost 1000) (Cost 50) (Cost 750))
        "Massive network of sensors used to hunt for gravity waves"
