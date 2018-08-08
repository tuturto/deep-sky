module Types exposing (..)

import Http

type Msg = AvailableComponents (Result Http.Error (List Component))
         | AddComponent Component
         | RemoveComponent Component

type alias Component = 
  { id : Int
  , name : String
  , description : String
  , weight : Int
  , slots : List EquipmentSlot
  , types : List EquipmentLevel
  }

type EquipmentSlot = InnerSlot
                   | OuterSlot
                   | ArmourSlot
                   | UnknownSlot

type EquipmentType = BridgeEquipment
                   | SensorEquipment                   
                   | UnknowEquipmentType

type EquipmentLevel = EquipmentLevel Int EquipmentType

type InstalledComponent = InstalledComponent Component Int

type alias Ship =
  { components : List InstalledComponent }

type alias Chassis =
  { name : String
  , maxTonnage : Int 
  , requiredTypes : List EquipmentLevel }

type alias Model =
  { components : List Component
  , ship : Ship
  , chassis : Chassis
  }

type alias ShipValidator = Model -> List (Maybe String)
