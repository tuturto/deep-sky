module Json exposing ( componentDecoder, chassisDecoder, shipDecoder
                     , shipSaveEncoder, dtoToShip )

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

stringToCompType : String -> Decode.Decoder ComponentType
stringToCompType s =
  case s of
    "BridgeComponent" -> Decode.succeed BridgeComponent
    "SensorComponent" -> Decode.succeed SensorComponent
    "EngineComponent" -> Decode.succeed EngineComponent
    "SupplyComponent" -> Decode.succeed SupplyComponent
    _ -> Decode.fail "Unknown component type"

componentLevelDecoder : Decode.Decoder ComponentLevel
componentLevelDecoder =
  Decode.succeed ComponentLevel
    |: (Decode.field "level" Decode.int)
    |: (Decode.field "type" Decode.string |> Decode.andThen stringToCompType)

componentCostDecoder : Decode.Decoder Cost
componentCostDecoder =
  Decode.succeed Cost
    |: (Decode.field "mechanical" Decode.int)
    |: (Decode.field "biological" Decode.int)
    |: (Decode.field "chemical" Decode.int)

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

componentDtoDecoder : Decode.Decoder ComponentDto
componentDtoDecoder =
  Decode.succeed ComponentDto
    |: (Decode.field "id" componentIdDecoder)
    |: (Decode.field "level" <| Decode.int)

componentEncoder : Component -> Encode.Value
componentEncoder component =
  Encode.object [ ("id", componentIdEncoder component.id)
                , ("level", Encode.int component.level) 
                ]

chassisDecoder : Decode.Decoder Chassis
chassisDecoder =
  Decode.succeed Chassis
  |: (Decode.field "id" Decode.int)
  |: (Decode.field "name" Decode.string)
  |: (Decode.field "maxTonnage" Decode.int)
  |: (Decode.field "requiredTypes" <| Decode.list componentLevelDecoder)

installedComponentDecoder : Decode.Decoder InstalledComponentDto
installedComponentDecoder =
  Decode.succeed InstalledComponentDto
  |: (Decode.field "component" componentDtoDecoder)
  |: (Decode.field "amount" Decode.int)

shipDecoder : Decode.Decoder ShipDto
shipDecoder =
  Decode.succeed ShipDto
  |: (Decode.field "components" <| Decode.list installedComponentDecoder)
  |: (Decode.field "name" Decode.string)
  |: (Decode.maybe <| Decode.field "id" Decode.int)
  |: (Decode.field "chassisId" Decode.int)

installedComponentEncoder : InstalledComponent -> Encode.Value
installedComponentEncoder (InstalledComponent component amount) =
  Encode.object [ ("component", componentEncoder component)
                , ("amount", Encode.int amount )
                ]

dtoToShip : ShipDto -> List Component -> List Chassis -> Ship
dtoToShip dto comps chassisList =
  let
    components = List.filterMap (dtoToComponent comps) dto.components
    chassis = chassisList 
              |> List.filter (\x -> x.id == dto.chassis) 
              |> List.head
  in
    Ship components dto.name chassis dto.id

dtoToComponent : List Component -> InstalledComponentDto -> Maybe InstalledComponent
dtoToComponent comps (InstalledComponentDto comp amount) = 
  let
    finder x = x.id == comp.id
    matches = List.filter finder comps
    match = List.head matches
  in
    case match of
      Nothing -> Nothing
      Just x -> Just <| InstalledComponent x amount

shipSaveEncoder : Ship -> Maybe Chassis -> Encode.Value
shipSaveEncoder ship chassis = 
  case chassis of
    Just c -> Encode.object [ ("name", Encode.string ship.name) 
                            , ("components", Encode.list <| List.map installedComponentEncoder ship.components)
                            , ("chassisId", Encode.int c.id)
                            ]
    Nothing -> Encode.object [ ("name", Encode.string ship.name) 
                             , ("components", Encode.list <| List.map installedComponentEncoder ship.components)
                             ]
