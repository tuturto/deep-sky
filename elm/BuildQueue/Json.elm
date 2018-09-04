module Json exposing ( buildingInfoDecoder )

import Json.Decode.Extra exposing ((|:))
import Json.Decode as Decode
import Json.Encode as Encode
import Types exposing (..)

costDecoder : Decode.Decoder Cost
costDecoder =
  Decode.succeed Cost
    |: (Decode.field "mechanical" Decode.int)
    |: (Decode.field "biological" Decode.int)
    |: (Decode.field "chemical" Decode.int)

stringToBuildingType : String -> Decode.Decoder BuildingType
stringToBuildingType s =
  case s of
    "SensorStation" -> Decode.succeed SensorStation
    "ResearchComplex" -> Decode.succeed ResearchComplex
    "Farm" -> Decode.succeed Farm
    "ParticleAccelerator" -> Decode.succeed ParticleAccelerator
    "NeutronDetector" -> Decode.succeed NeutronDetector
    "BlackMatterScanner" -> Decode.succeed BlackMatterScanner
    "GravityWaveSensor" -> Decode.succeed GravityWaveSensor
    _ -> Decode.fail "Unknown slot type"

buildingTypeDecoder : Decode.Decoder BuildingType
buildingTypeDecoder =
  Decode.string |> Decode.andThen stringToBuildingType

buildingInfoDecoder : Decode.Decoder BuildingInfo
buildingInfoDecoder =
  Decode.succeed BuildingInfo
  |: (Decode.field "id" buildingTypeDecoder)
  |: (Decode.field "level" Decode.int)
  |: (Decode.field "name" Decode.string)
  |: (Decode.field "cost" costDecoder)
  |: (Decode.field "description" Decode.string)
