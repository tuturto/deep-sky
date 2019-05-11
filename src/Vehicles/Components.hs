{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Vehicles.Components
    ( Component(..), ComponentSlot(..), ComponentId(..), ComponentType(..), ComponentLevel(..)
    , ComponentPower(..), Weight(..), ChassisType(..), SlotAmount(..), ChassisName(..)
    , scaleLevel, components, requirements, componentRequirements )
    where

import Data.Aeson ( ToJSON(..), withScientific, withText )
import Data.Aeson.TH
    ( deriveJSON, defaultOptions, fieldLabelModifier  )
import Data.Scientific ( toBoundedInteger )
import Database.Persist.TH
import Database.Persist.Sql
import ClassyPrelude.Yesod   as Import
import Research.Data ( Technology(..) )
import Resources ( RawResources(..), RawResource(..), ResourceCost )


-- Types

data Component = Component
    { componentId :: ComponentId
    , componentLevel :: ComponentLevel
    , componentName :: String
    , componentDescription :: String
    , componentWeight :: Weight
    , componentSlot :: ComponentSlot
    , componentType :: [ ComponentPower ]
    , componentCost :: RawResources ResourceCost
    , componentChassisType :: ChassisType
    }
    deriving (Show, Read, Eq, Ord)


-- | Type of component and relative power of the component
data ComponentPower = ComponentPower
    { componentPowerLevel :: ComponentLevel
    , componentPowerType :: ComponentType
    }
    deriving (Show, Read, Eq, Ord)


-- | Relatively power of a component
newtype ComponentLevel = ComponentLevel { unComponentLevel :: Int }
    deriving (Show, Read, Eq, Ord, Num)


-- | Scale component power with scalar
scaleLevel :: ComponentLevel -> Int -> ComponentLevel
scaleLevel (ComponentLevel lvl) scale =
    ComponentLevel $ lvl * scale


-- | Weight of something
newtype Weight = Weight { unWeight :: Int }
    deriving (Show, Read, Eq, Ord, Num)


instance ToJSON Weight where
    toJSON = toJSON . unWeight


instance FromJSON Weight where
    parseJSON =
        withScientific "Weight"
                       (\x -> case toBoundedInteger x of
                            Nothing ->
                                mempty

                            Just n ->
                                return $ Weight n)


instance PersistField Weight where
    toPersistValue (Weight n) =
        PersistInt64 $ fromIntegral n

    fromPersistValue (PersistInt64 n) =
        Right $ Weight $ fromIntegral n

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql Weight where
    sqlType _ = SqlInt64


-- | Enumeration of different types of components
data ComponentType =
    BridgeComponent
    | SensorComponent
    | EngineComponent
    | StarSailComponent
    | SupplyComponent
    | MotiveComponent
    deriving (Show, Read, Eq, Ord)


-- | Type of chassis
data ChassisType =
    SpaceShip
    | AirShip
    | LandVehicle
    | TalosArmour
    | IcarusSuit
    deriving (Show, Read, Eq, Ord)


data ComponentId = ShipLongRangeSensors
    | ShipShortRangeSensors
    | ShipArmour
    | ShipHeavyArmour
    | ShipBridge
    | ShipSupplyPod
    | ShipStarSail
    | VehicleWheeledMotiveSystem
    | VehicleTrackedMotiveSystem
    | VehicleHoverMotiveSystem
    deriving (Show, Read, Eq, Ord, Bounded, Enum)


-- | Name of chassis
newtype ChassisName = MkChassisName { unChassisName :: Text }
    deriving (Show, Read, Eq)


instance ToJSON ChassisName where
    toJSON = toJSON . unChassisName


instance FromJSON ChassisName where
    parseJSON =
        withText "Chassis name"
            (\x -> return $ MkChassisName x)


instance PersistField ChassisName where
    toPersistValue (MkChassisName s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ MkChassisName s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql ChassisName where
    sqlType _ = SqlString


-- | List defining what kind of technology is required to unlock a specific component
requirements :: [(Maybe Technology, ComponentId)]
requirements =
    fmap (\x -> (componentRequirements x, x)) [minBound..maxBound]


-- | Given a component id, which technology is required to unlock it
componentRequirements :: ComponentId -> Maybe Technology
componentRequirements ShipLongRangeSensors = Just HighSensitivitySensors
componentRequirements ShipShortRangeSensors = Nothing
componentRequirements ShipArmour = Nothing
componentRequirements ShipHeavyArmour = Just HighTensileMaterials
componentRequirements ShipBridge = Nothing
componentRequirements ShipSupplyPod = Nothing
componentRequirements ShipStarSail = Nothing
componentRequirements VehicleWheeledMotiveSystem = Nothing
componentRequirements VehicleTrackedMotiveSystem = Nothing
componentRequirements VehicleHoverMotiveSystem = Just HoverCrafts


-- | Given a component level and id create a component
components :: ComponentLevel -> ComponentId -> Component
components level ShipStarSail =
    Component
        { componentId = ShipStarSail
        , componentLevel = level
        , componentName = "Star sails"
        , componentDescription = "Thin and strong sails capturing star winds"
        , componentWeight = Weight 15
        , componentSlot = SailSlot
        , componentType = [ ComponentPower level StarSailComponent ]
        , componentCost = RawResources (RawResource 10) (RawResource 0) (RawResource 10)
        , componentChassisType = SpaceShip
        }

components level ShipShortRangeSensors =
    Component
        { componentId = ShipShortRangeSensors
        , componentLevel = level
        , componentName = "Short range sensors"
        , componentDescription = "Various scanners and sensors for short range observation"
        , componentWeight = Weight 5
        , componentSlot = SensorSlot
        , componentType = [ ComponentPower level SensorComponent ]
        , componentCost = RawResources (RawResource 1) (RawResource 1) (RawResource 1)
        , componentChassisType = SpaceShip
        }

components level ShipLongRangeSensors =
    Component
        { componentId = ShipLongRangeSensors
        , componentLevel = level
        , componentName = "Long range sensors"
        , componentDescription = "Various scanners and sensors for long range observation"
        , componentWeight = Weight 10
        , componentSlot = SensorSlot
        , componentType = [ ComponentPower level SensorComponent ]
        , componentCost = RawResources (RawResource 5) (RawResource 5) (RawResource 5)
        , componentChassisType = SpaceShip
        }

components level ShipSupplyPod =
    Component
        { componentId = ShipSupplyPod
        , componentLevel = level
        , componentName = "Supply pod"
        , componentDescription = "Storage system for supplies needed by the crew and the ship"
        , componentWeight = Weight 10
        , componentSlot = InnerSlot
        , componentType = [ ComponentPower (scaleLevel level 10) SupplyComponent ]
        , componentCost = RawResources (RawResource 5) (RawResource 50) (RawResource 5)
        , componentChassisType = SpaceShip
        }

components level ShipBridge =
    Component
        { componentId = ShipBridge
        , componentLevel = level
        , componentName = "Bridge"
        , componentDescription = "Nerve center of a ship, containing controls and instruments needed for steering the ship"
        , componentWeight = Weight 10
        , componentSlot = InnerSlot
        , componentType = [ ComponentPower level BridgeComponent
                          , ComponentPower (scaleLevel level 5) SupplyComponent ]
        , componentCost = RawResources (RawResource 10) (RawResource 5) (RawResource 10)
        , componentChassisType = SpaceShip
        }

components level ShipArmour =
    Component
        { componentId = ShipArmour
        , componentLevel = level
        , componentName = "Armour"
        , componentDescription = "Heavy protective plating against kinetic damage"
        , componentWeight = Weight 20
        , componentSlot = ArmourSlot
        , componentType = [ ]
        , componentCost = RawResources (RawResource 20) (RawResource 0) (RawResource 0)
        , componentChassisType = SpaceShip
        }

components level ShipHeavyArmour =
    Component
        { componentId = ShipArmour
        , componentLevel = level
        , componentName = "Heavy armour"
        , componentDescription = "Extra heavy protective plating against kinetic damage"
        , componentWeight = Weight 30
        , componentSlot = ArmourSlot
        , componentType = [ ]
        , componentCost = RawResources (RawResource 30) (RawResource 0) (RawResource 0)
        , componentChassisType = SpaceShip
        }

components level VehicleWheeledMotiveSystem =
    Component
        { componentId = VehicleWheeledMotiveSystem
        , componentLevel = level
        , componentName = "Wheeled"
        , componentDescription = "Wheels allow fast movement on hard surfaces"
        , componentWeight = Weight 0
        , componentSlot = MotiveSlot
        , componentType = [ ComponentPower level MotiveComponent ]
        , componentCost = mempty
        , componentChassisType = LandVehicle
        }

components level VehicleTrackedMotiveSystem =
    Component
        { componentId = VehicleTrackedMotiveSystem
        , componentLevel = level
        , componentName = "Tracks"
        , componentDescription = "While slower than wheeled, tracked vehicles can often travel in places where wheeled can't"
        , componentWeight = Weight 0
        , componentSlot = MotiveSlot
        , componentType = [ ComponentPower level MotiveComponent ]
        , componentCost = RawResources (RawResource 30) (RawResource 0) (RawResource 0)
        , componentChassisType = LandVehicle
        }

components level VehicleHoverMotiveSystem =
    Component
        { componentId = VehicleTrackedMotiveSystem
        , componentLevel = level
        , componentName = "Hover system"
        , componentDescription = "Hover vehicles are able to travel both on land and over water"
        , componentWeight = Weight 0
        , componentSlot = MotiveSlot
        , componentType = [ ComponentPower level MotiveComponent ]
        , componentCost = RawResources (RawResource 30) (RawResource 0) (RawResource 0)
        , componentChassisType = LandVehicle
        }


instance ToJSON ComponentLevel where
    toJSON = toJSON . unComponentLevel


instance FromJSON ComponentLevel where
    parseJSON =
        withScientific "component level"
                       (\x -> case toBoundedInteger x of
                            Nothing ->
                                mempty

                            Just n ->
                                return $ ComponentLevel n)




data ComponentSlot =
    InnerSlot
    | OuterSlot
    | ArmourSlot
    | SensorSlot
    | WeaponSlot
    | EngineSlot
    | MotiveSlot
    | SailSlot
    deriving (Show, Read, Eq, Ord)


-- | Amount of slots
newtype SlotAmount = SlotAmount { unSlotAmount :: Int }
    deriving (Show, Read, Eq, Ord, Num)


instance ToJSON SlotAmount where
    toJSON = toJSON . unSlotAmount


instance FromJSON SlotAmount where
    parseJSON =
        withScientific "Weight"
                       (\x -> case toBoundedInteger x of
                            Nothing ->
                                mempty

                            Just n ->
                                return $ SlotAmount n)


instance PersistField SlotAmount where
    toPersistValue (SlotAmount n) =
        PersistInt64 $ fromIntegral n

    fromPersistValue (PersistInt64 n) =
        Right $ SlotAmount $ fromIntegral n

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql SlotAmount where
    sqlType _ = SqlInt64


derivePersistField "ComponentSlot"
derivePersistField "ComponentType"
derivePersistField "ComponentId"
derivePersistField "ChassisType"


$(deriveJSON defaultOptions ''ComponentType)
$(deriveJSON defaultOptions ''ComponentId)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 14 } ''ComponentPower)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 9 } ''Component)
$(deriveJSON defaultOptions ''ComponentSlot)
$(deriveJSON defaultOptions ''ChassisType)