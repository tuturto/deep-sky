module Json exposing ( componentDecoder, chassisDecoder, shipDecoder
                     , shipSaveEncoder )

import Json.Decode.Extra exposing ((|:))
import Json.Decode as Decode
import Json.Encode as Encode
import Types exposing (..)

stringToSlot : String -> Decode.Decoder ComponentSlot
stringToSlot s =
  case s of
    "InnerSlot" -> Decode.succeed InnerSlot
    "OuterSlot" -> Decode.succeed OuterSlot
    "ArmourSlot" -> Decode.succeed ArmourSlot
    "SensorSlot" -> Decode.succeed SensorSlot
    "WeaponSlot" -> Decode.succeed WeaponSlot
    "EngineSlot" -> Decode.succeed EngineSlot
    _ -> Decode.fail "Unknown slot type"

slotDecoder : Decode.Decoder ComponentSlot
slotDecoder =
  Decode.string |> Decode.andThen stringToSlot

slotEncoder : ComponentSlot -> Encode.Value
slotEncoder slot =
  Encode.string  <| case slot of
    InnerSlot -> "InnerSlot"
    OuterSlot -> "OuterSlot"
    ArmourSlot -> "ArmourSlot"
    SensorSlot -> "SensorSlot"
    WeaponSlot -> "WeaponSlot"
    EngineSlot -> "EngineSlot"

stringToCompType : String -> Decode.Decoder ComponentType
stringToCompType s =
  case s of
    "BridgeComponent" -> Decode.succeed BridgeComponent
    "SensorComponent" -> Decode.succeed SensorComponent
    "EngineComponent" -> Decode.succeed EngineComponent
    "SupplyComponent" -> Decode.succeed SupplyComponent
    _ -> Decode.fail "Unknown component type"

compTypeEncoder : ComponentType -> Encode.Value
compTypeEncoder ct =
  Encode.string  <| case ct of
    BridgeComponent -> "BridgeComponent"
    SensorComponent -> "SensorComponent"
    EngineComponent -> "EngineComponent"
    SupplyComponent -> "SupplyComponent"

componentLevelDecoder : Decode.Decoder ComponentLevel
componentLevelDecoder =
  Decode.succeed ComponentLevel
    |: (Decode.field "level" Decode.int)
    |: (Decode.field "type" Decode.string |> Decode.andThen stringToCompType)

componentLevelEncoder : ComponentLevel -> Encode.Value
componentLevelEncoder (ComponentLevel level compType) =
  Encode.object [ ("level", Encode.int level)
                , ("type", compTypeEncoder compType)
                ]

componentCostDecoder : Decode.Decoder Cost
componentCostDecoder =
  Decode.succeed Cost
    |: (Decode.field "mechanical" Decode.int)
    |: (Decode.field "biological" Decode.int)
    |: (Decode.field "chemical" Decode.int)

componentCostEncoder : Cost -> Encode.Value
componentCostEncoder cost =
  Encode.object [ ("mechanical", Encode.int cost.mechanical)
                , ("biological", Encode.int cost.biological)
                , ("chemical", Encode.int cost.chemical) 
                ]

stringToComponentId : String -> Decode.Decoder ComponentId
stringToComponentId s =
  case s of
    "CidLongRangeSensors" -> Decode.succeed CidLongRangeSensors
    "CidArmour" -> Decode.succeed CidArmour
    "CidBridge" -> Decode.succeed CidBridge
    "CidEngine" -> Decode.succeed CidEngine
    "CidSupplyPod" -> Decode.succeed CidSupplyPod
    _ ->Decode.fail "not implemented"

componentIdDecoder : Decode.Decoder ComponentId
componentIdDecoder =
  Decode.string |> Decode.andThen stringToComponentId

componentIdEncoder : ComponentId -> Encode.Value
componentIdEncoder compId = 
  Encode.string <| case compId of
    CidLongRangeSensors -> "CidLongRangeSensors"
    CidArmour -> "CidArmour"
    CidBridge -> "CidBridge"
    CidEngine -> "CidEngine"
    CidSupplyPod -> "CidSupplyPod"

componentDecoder : Decode.Decoder Component
componentDecoder =
  Decode.succeed Component
    |: (Decode.field "id" componentIdDecoder)
    |: (Decode.field "name" Decode.string)
    |: (Decode.field "description" Decode.string)
    |: (Decode.field "weight" Decode.int)
    |: (Decode.field "slot" slotDecoder)
    |: (Decode.field "types" <| Decode.list componentLevelDecoder)
    |: (Decode.field "cost" <| componentCostDecoder)
    |: (Decode.field "level" <| Decode.int)

componentEncoder : Component -> Encode.Value
componentEncoder component =
  Encode.object [ ("id", componentIdEncoder component.id)
                , ("name", Encode.string component.name)
                , ("description", Encode.string component.description)
                , ("weight", Encode.int component.weight)
                , ("slot", slotEncoder component.slot)
                , ("types", Encode.list <| List.map componentLevelEncoder component.types)
                , ("cost", componentCostEncoder component.cost)
                , ("level", Encode.int component.level) 
                ]

chassisDecoder : Decode.Decoder Chassis
chassisDecoder =
  Decode.succeed Chassis
  |: (Decode.field "id" Decode.int)
  |: (Decode.field "name" Decode.string)
  |: (Decode.field "maxTonnage" Decode.int)
  |: (Decode.field "requiredTypes" <| Decode.list componentLevelDecoder)

chassisEncoder : Chassis -> Encode.Value
chassisEncoder chassis =
  Encode.object [ ("id", Encode.int chassis.id)
                , ("name", Encode.string chassis.name)
                , ("maxTonnage", Encode.int chassis.maxTonnage)
                , ("requiredTypes", Encode.list <| List.map componentLevelEncoder chassis.requiredTypes)
                ]

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

installedComponentEncoder : InstalledComponent -> Encode.Value
installedComponentEncoder (InstalledComponent component amount) =
  Encode.object [ ("component", componentEncoder component)
                , ("amount", Encode.int amount )
                ]

shipSaveEncoder : Ship -> Maybe Chassis -> Encode.Value
shipSaveEncoder ship chassis = 
  case chassis of
    Just c -> Encode.object [ ("name", Encode.string ship.name) 
                            , ("components", Encode.list <| List.map installedComponentEncoder ship.components)
                            , ("chassis", chassisEncoder c)
                            ]
    Nothing -> Encode.object [ ("name", Encode.string ship.name) 
                             , ("components", Encode.list <| List.map installedComponentEncoder ship.components)
                             ]
