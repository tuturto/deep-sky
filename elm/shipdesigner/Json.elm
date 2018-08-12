module Json exposing (componentDecoder, chassisDecoder, shipDecoder)

import Json.Decode.Extra exposing ((|:))
import Json.Decode as Decode
import Types exposing (..)

stringToSlot : String -> Decode.Decoder EquipmentSlot
stringToSlot s =
  case s of
    "InnerSlot" -> Decode.succeed InnerSlot
    "OuterSlot" -> Decode.succeed OuterSlot
    "ArmourSlot" -> Decode.succeed ArmourSlot
    "SensorSlot" -> Decode.succeed SensorSlot
    "WeaponSlot" -> Decode.succeed WeaponSlot
    "EngineSlot" -> Decode.succeed EngineSlot
    _ -> Decode.fail "Unknown slot type"

slotDecoder : Decode.Decoder EquipmentSlot
slotDecoder =
  Decode.string |> Decode.andThen stringToSlot

stringToEqType : String -> Decode.Decoder EquipmentType
stringToEqType s =
  case s of
    "BridgeEquipment" -> Decode.succeed BridgeEquipment
    "SensorEquipment" -> Decode.succeed SensorEquipment
    "EngineEquipment" -> Decode.succeed EngineEquipment
    _ -> Decode.fail "Unknown component type"

equipmentLevelDecoder : Decode.Decoder EquipmentLevel
equipmentLevelDecoder =
  Decode.succeed EquipmentLevel
    |: (Decode.field "level" Decode.int)
    |: (Decode.field "type" Decode.string |> Decode.andThen stringToEqType)

componentCostDecoder : Decode.Decoder Cost
componentCostDecoder =
    Decode.succeed Cost
        |: (Decode.field "mechanical" Decode.int)
        |: (Decode.field "biological" Decode.int)
        |: (Decode.field "chemical" Decode.int)

componentDecoder : Decode.Decoder Component
componentDecoder =
  Decode.succeed Component
    |: (Decode.field "id" Decode.int)
    |: (Decode.field "name" Decode.string)
    |: (Decode.field "description" Decode.string)
    |: (Decode.field "weight" Decode.int)
    |: (Decode.field "slot" slotDecoder)
    |: (Decode.field "types" <| Decode.list equipmentLevelDecoder)
    |: (Decode.field "cost" <| componentCostDecoder)

chassisDecoder : Decode.Decoder Chassis
chassisDecoder =
  Decode.succeed Chassis
  |: (Decode.field "id" Decode.int)
  |: (Decode.field "name" Decode.string)
  |: (Decode.field "maxTonnage" Decode.int)
  |: (Decode.field "requiredTypes" <| Decode.list equipmentLevelDecoder)

installedComponentDecoder : Decode.Decoder InstalledComponent
installedComponentDecoder =
  Decode.succeed InstalledComponent
  |: (Decode.field "component" componentDecoder)
  |: (Decode.field "amount" Decode.int)

shipDecoder : Decode.Decoder Ship
shipDecoder =
  Decode.succeed Ship
  |: (Decode.field "components" <| Decode.list installedComponentDecoder)
  |: (Decode.field "name" Decode.string)