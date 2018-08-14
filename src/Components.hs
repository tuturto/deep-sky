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
import CustomTypes 
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
    , dCompCost :: ComponentCost }
    deriving (Show, Read, Eq)

data ComponentCost = ComponentCost
    { ccdMechanicCost :: Cost
    , ccdBiologicalCost :: Cost
    , ccdChemicalCost :: Cost }
    deriving (Show, Read, Eq)

data ComponentLevel = ComponentLevel 
    { compLeLevel :: CLevel
    , compLeType :: ComponentType }
    deriving (Show, Read, Eq)

data Cost = Cost { unCost :: Int }
    deriving (Show, Read, Eq)

data CLevel = CLevel { unCLevel :: Int }
    deriving (Show, Read, Eq)

data Weight = Weight { unWeight :: Int }
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

instance ToJSON ComponentCost where
    toJSON (ComponentCost mech bio chem) =
        object [ "mechanical" .= unCost mech
               , "biological" .= unCost bio
               , "chemical" .= unCost chem 
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
    deriving (Show, Read, Eq)

component :: ComponentId -> CLevel -> Component
component CidLongRangeSensors level = 
    Component CidLongRangeSensors level "Long range sensors" "description" (Weight 5) SensorSlot 
        [ ComponentLevel level SensorComponent ] $ 
        ComponentCost (Cost 1) (Cost 1) (Cost 1)
component CidArmour level =
    Component CidArmour level "Armour" "description" (Weight 20) ArmourSlot 
        [] $ ComponentCost (Cost 20) (Cost 0) (Cost 0)
component CidBridge level =
    Component CidBridge level "Bridge" "description" (Weight 10) InnerSlot 
        [ ComponentLevel level BridgeComponent 
        , ComponentLevel level SupplyComponent ] $ ComponentCost (Cost 10) (Cost 5) (Cost 10)
component CidEngine level =
    Component CidEngine level "Engine" "description" (Weight 2) EngineSlot 
        [ ComponentLevel level EngineComponent ] $ ComponentCost (Cost 15) (Cost 0) (Cost 10)

derivePersistField "ComponentType"
derivePersistField "ComponentId"
$(deriveJSON defaultOptions ''ComponentType)
$(deriveJSON defaultOptions ''ComponentId)
