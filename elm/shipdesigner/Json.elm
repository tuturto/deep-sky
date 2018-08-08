module Json exposing (componentDecoder)

import Json.Decode.Extra exposing ((|:))
import Json.Decode as Decode
import Types exposing (..)

stringToSlot : String -> Decode.Decoder EquipmentSlot
stringToSlot s =
  case s of
    "InnerSlot" -> Decode.succeed InnerSlot
    "OuterSlot" -> Decode.succeed OuterSlot
    "ArmourSlot" -> Decode.succeed ArmourSlot
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

componentDecoder : Decode.Decoder Component
componentDecoder =
  Decode.succeed Component
    |: (Decode.field "id" Decode.int)
    |: (Decode.field "name" Decode.string)
    |: (Decode.field "description" Decode.string)
    |: (Decode.field "weight" Decode.int)
    |: (Decode.field "slots" <| Decode.list slotDecoder)
    |: (Decode.field "types" <| Decode.list equipmentLevelDecoder)