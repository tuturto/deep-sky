{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Components where

import Data.Aeson (object, (.=), ToJSON(..))
import Data.Aeson.TH 
import CustomTypes (RawResources(..), RawResource(..), ComponentSlot(..), ResourceCost)
import Database.Persist.TH
import ClassyPrelude.Yesod   as Import

-- Types

data Component = Component
    { dCompId :: ComponentId
    , dCompLevel :: CLevel
    , dCompName :: String
    , dCompDescription :: String
    , dCompWeight :: Weight 
    , dCompSlot :: ComponentSlot
    , dCompType :: [ ComponentLevel ] 
    , dCompCost :: RawResources ResourceCost }
    deriving (Show, Read, Eq)

data ComponentLevel = ComponentLevel 
    { compLeLevel :: CLevel
    , compLeType :: ComponentType }
    deriving (Show, Read, Eq)

newtype CLevel = CLevel { unCLevel :: Int }
    deriving (Show, Read, Eq)

scaleLevel :: CLevel -> Int -> CLevel
scaleLevel (CLevel lvl) scale =
    CLevel $ lvl * scale

newtype Weight = Weight { unWeight :: Int }
    deriving (Show, Read, Eq)

data ComponentType = BridgeComponent
    | SensorComponent
    | EngineComponent
    | SupplyComponent
    deriving (Show, Read, Eq)

instance ToJSON Component where
    toJSON (Component idKey level name desc weight slots types cost) = 
        object [ "id" .= idKey
               , "level" .= unCLevel level
               , "name" .= name
               , "description" .= desc
               , "weight" .= unWeight weight
               , "slot" .= slots 
               , "types" .= array types
               , "cost" .= cost 
               ]

instance ToJSON ComponentLevel where
    toJSON (ComponentLevel level cType) =
        object [ "level" .= unCLevel level
               , "type" .= cType
               ]

data ComponentId = CidLongRangeSensors
    | CidArmour
    | CidBridge
    | CidEngine
    | CidSupplyPod
    deriving (Show, Read, Eq)

component :: ComponentId -> CLevel -> Component
component CidLongRangeSensors level = 
    Component CidLongRangeSensors level "Long range sensors" "Various scanners and sensors for long range observation" (Weight 5) SensorSlot 
        [ ComponentLevel level SensorComponent ] $ 
        RawResources (RawResource 1) (RawResource 1) (RawResource 1)
component CidArmour level =
    Component CidArmour level "Armour" "Heave protective plating against kinetic damage" (Weight 20) ArmourSlot 
        [] $ RawResources (RawResource 20) (RawResource 0) (RawResource 0)
component CidBridge level =
    Component CidBridge level "Bridge" "Nerve center of a ship, containing controls and instruments needed for steering the ship" (Weight 10) InnerSlot 
        [ ComponentLevel level BridgeComponent 
        , ComponentLevel (scaleLevel level 5) SupplyComponent ] $ RawResources (RawResource 10) (RawResource 5) (RawResource 10)
component CidEngine level =
    Component CidEngine level "Engine" "Two stage ion propulsion system" (Weight 2) EngineSlot 
        [ ComponentLevel level EngineComponent 
        , ComponentLevel (scaleLevel level 5) SupplyComponent ] $ RawResources (RawResource 15) (RawResource 0) (RawResource 10)
component CidSupplyPod level =
    Component CidSupplyPod level "Supply pod" "Storage system for supplies needed by the crew and the ship" (Weight 10) InnerSlot
        [ ComponentLevel (scaleLevel level 10) SupplyComponent ] $ RawResources (RawResource 5) (RawResource 50) (RawResource 5)

derivePersistField "ComponentType"
derivePersistField "ComponentId"
$(deriveJSON defaultOptions ''ComponentType)
$(deriveJSON defaultOptions ''ComponentId)
