import Html exposing (..)
import Http
import Json.Decode.Extra exposing ((|:))
import Json.Decode as Decode
import Focus exposing (..)
import Types exposing (..)
import Render exposing (view)

main : Program Never Model Msg
main =
  program { init = init
          , view = view
          , update = update 
          , subscriptions = subscriptions }

-- MODEL

init : (Model, Cmd Msg)
init =
  let newModel = { components = []
                 , ship = Ship []
                 , chassis = { name = "Destroyer"
                             , maxTonnage = 150
                             , requiredTypes = [ EquipmentLevel 1 BridgeEquipment ] }
                 }
      url = "/api/components"
      cmd = Http.send AvailableComponents (Http.get url (Decode.list componentDecoder))
  in
    (newModel, cmd)

stringToSlot : String -> Decode.Decoder EquipmentSlot
stringToSlot s =
  case s of
    "InnerSlot" -> Decode.succeed InnerSlot
    "OuterSlot" -> Decode.succeed OuterSlot
    "ArmourSlot" -> Decode.succeed ArmourSlot
    _ -> Decode.succeed UnknownSlot

slotDecoder : Decode.Decoder EquipmentSlot
slotDecoder =
  Decode.string |> Decode.andThen stringToSlot

stringToEqType : String -> Decode.Decoder EquipmentType
stringToEqType s =
  case s of
    "BridgeEquipment" -> Decode.succeed BridgeEquipment
    "SensorEquipment" -> Decode.succeed SensorEquipment
    _ -> Decode.succeed UnknowEquipmentType

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

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AvailableComponents (Ok components) ->
      ( model & modelComponents .= components
      , Cmd.none)
    AvailableComponents (Err data) ->
      (model, Cmd.none)
    AddComponent component ->
      ( addComponent model component
      , Cmd.none)
    RemoveComponent component ->
      ( removeComponent model component
      , Cmd.none)

addComponent : Model -> Component -> Model
addComponent model component =
  let
    matches = List.filter search model.ship.components
    search (InstalledComponent item _) = item.id == component.id
    newCount = case List.head matches of
                Just (InstalledComponent _ count) -> count + 1
                Nothing -> 1
  in
    if newCount > 1 then
      let 
        compRemoved = model & modelShipF => shipComponentsF $= List.filter (\x -> not <| search x)
      in
        compRemoved & modelShipF => shipComponentsF $= List.append [InstalledComponent component newCount]
    else
      model & modelShipF => shipComponentsF $= List.append [InstalledComponent component 1]

removeComponent : Model -> Component -> Model
removeComponent model component =
  let
    matches = List.filter search model.ship.components
    search (InstalledComponent item _) = item.id == component.id
    newCount = case List.head matches of
                Just (InstalledComponent _ count) -> count - 1
                Nothing -> 0
  in
    if newCount == 0 then
      model & modelShipF => shipComponentsF $= List.filter (\x -> not <| search x)
    else
      let 
        compRemoved = model & modelShipF => shipComponentsF $= List.filter (\x -> not <| search x)
      in
        compRemoved & modelShipF => shipComponentsF $= List.append [InstalledComponent component newCount]

modelComponents : Setter Model Model (List Component) (List Component)
modelComponents f model = { model | components = f model.components }

modelShipF : Setter Model Model Ship Ship
modelShipF f model = { model | ship = f model.ship }

shipComponentsF : Setter Ship Ship (List InstalledComponent) (List InstalledComponent)
shipComponentsF f ship = { ship | components = f ship.components }

totalTonnage : Ship -> Int
totalTonnage ship =
  List.foldr (\(InstalledComponent component amount) acc -> component.weight * amount + acc) 0 ship.components

tonnageCheck : ShipValidator
tonnageCheck model =
  if totalTonnage model.ship > model.chassis.maxTonnage
  then [ Just "Ship design exceeds max tonnage" ]
  else [ Nothing ]

equipmentRequirementToString : EquipmentLevel -> String
equipmentRequirementToString (EquipmentLevel lvl eType) =
  case eType of
    BridgeEquipment ->
      "at least " ++ (toString lvl) ++ " points worth of bridges is required"
    SensorEquipment ->
      "at least " ++ (toString lvl) ++ " points worth of sensors is required"
    UnknowEquipmentType ->
      "unknown type"

componentCheck : ShipValidator
componentCheck model =
  let 
    checkSingle (EquipmentLevel n equipment) = 
      let 
        matching = List.filter (\(InstalledComponent comp level) -> 
          List.any (\(EquipmentLevel _ eType) -> eType == equipment) comp.types) model.ship.components
        total = List.foldr (\(InstalledComponent _ lvl) acc -> acc + lvl) 0 matching
      in
        if total >= n
        then Nothing
        else Just <| equipmentRequirementToString (EquipmentLevel n equipment)
  in
    List.map checkSingle model.chassis.requiredTypes

validators : List ShipValidator
validators = [ tonnageCheck 
             , componentCheck ]

validateDesign : Model -> List String
validateDesign model =
  List.filterMap identity <| List.concatMap (\x -> x model) validators