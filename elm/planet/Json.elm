module Json exposing ( buildingInfoDecoder, buildingDecoder, populationDecoder, planetDetailsDecoder 
                     , constructionDecoder, buildingConstructionEncoder )

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

costEncoder : Cost -> Encode.Value
costEncoder cost =
  Encode.object [ ("mechanical", Encode.int cost.mechanical)
                , ("biological", Encode.int cost.biological)
                , ("chemical", Encode.int cost.chemical)
                ]

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

buildingTypeEncoder : BuildingType -> Encode.Value
buildingTypeEncoder bType =
  Encode.string <| case bType of
    SensorStation -> "SensorStation"
    ResearchComplex -> "ResearchComplex"
    Farm -> "Farm"
    ParticleAccelerator -> "ParticleAccelerator"
    NeutronDetector -> "NeutronDetector"
    BlackMatterScanner -> "BlackMatterScanner"
    GravityWaveSensor -> "GravityWaveSensor"

buildingInfoDecoder : Decode.Decoder BuildingInfo
buildingInfoDecoder =
  Decode.succeed BuildingInfo
  |: (Decode.field "id" buildingTypeDecoder)
  |: (Decode.field "level" Decode.int)
  |: (Decode.field "name" Decode.string)
  |: (Decode.field "cost" costDecoder)
  |: (Decode.field "description" Decode.string)  

buildingDecoder : Decode.Decoder Building
buildingDecoder =
  Decode.succeed Building
  |: (Decode.field "BuildingId" Decode.int)
  |: (Decode.field "Type" buildingTypeDecoder)
  |: (Decode.field "Damage" Decode.float)
  |: (Decode.field "Date" Decode.int)
  |: (Decode.field "Level" Decode.int)

populationDecoder : Decode.Decoder Population
populationDecoder =
  Decode.succeed Population
  |: (Decode.field "Race" Decode.string)
  |: (Decode.field "Population" Decode.int)
  |: (Decode.field "Date" Decode.int)

planetDetailsDecoder : Decode.Decoder PlanetDetails
planetDetailsDecoder =
  Decode.succeed PlanetDetails
  |: (Decode.field "PlanetId" Decode.int)
  |: (Decode.field "SystemId" Decode.int)
  |: (Decode.field "Name" Decode.string)
  |: (Decode.field "Position" Decode.int)
  |: (Decode.field "Gravity" Decode.float)
  |: (Decode.field "Date" Decode.int)

constructionDecoder : Decode.Decoder Construction
constructionDecoder =
  Decode.oneOf [ Decode.succeed ShipConstruction
                 |: shipConstructionDecoder
               , Decode.succeed BuildingConstruction
                 |: buildingConstructionDecoder ]

buildingConstructionDecoder : Decode.Decoder BuildingConstructionData
buildingConstructionDecoder =
  Decode.succeed BuildingConstructionData
  |: (Decode.field "id" Decode.int)
  |: (Decode.field "name" Decode.string)
  |: (Decode.field "index" Decode.int)
  |: (Decode.field "level" Decode.int)
  |: (Decode.field "type" buildingTypeDecoder)
  |: (Decode.field "planet" Decode.int)
  |: (Decode.field "costLeft" costDecoder)

buildingConstructionEncoder : BuildingConstructionData -> Encode.Value
buildingConstructionEncoder building =
  Encode.object [ ("id", Encode.int building.id)
                , ("name", Encode.string building.name)
                , ("index", Encode.int building.index)
                , ("level", Encode.int building.level)
                , ("type", buildingTypeEncoder building.buildingType)
                , ("planet", Encode.int building.planet)
                , ("costLeft", costEncoder building.costLeft)
                ]

shipConstructionDecoder : Decode.Decoder ShipConstructionData
shipConstructionDecoder =
  Decode.succeed ShipConstructionData
  |: (Decode.field "id" Decode.int)
  |: (Decode.field "name" Decode.string)
  |: (Decode.field "shipType" Decode.string)
  |: (Decode.field "index" Decode.int)
