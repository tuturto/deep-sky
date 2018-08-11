import Html exposing (..)
import Http
import Json.Decode as Decode
import Focus exposing (..)
import Types exposing (..)
import Render
import Json

main : Program Never Model Msg
main =
  program { init = init
          , view = Render.view
          , update = update 
          , subscriptions = subscriptions }

-- MODEL

init : (Model, Cmd Msg)
init =
  let newModel = { components = []
                 , ship = Ship [] "enter ship name"
                 , chassis = Nothing
                 , chassisList = [ { id = 1
                                   , name = "Destroyer"
                                   , maxTonnage = 150
                                   , requiredTypes = [ EquipmentLevel 1 BridgeEquipment 
                                                     , EquipmentLevel 1 EngineEquipment ] }
                                 , { id = 2
                                   , name = "Satellite"
                                   , maxTonnage = 20
                                   , requiredTypes = [] }]
                 }                 
      url = "/api/components"
      cmd = Http.send AvailableComponents (Http.get url (Decode.list Json.componentDecoder))
  in
    (newModel, cmd)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AvailableComponents (Ok components) ->
      ( model & modelComponentsF .= components
      , Cmd.none)
    AvailableComponents (Err data) ->
      (model, Cmd.none)
    AddComponent component ->
      ( addComponent model component
      , Cmd.none)
    RemoveComponent component ->
      ( removeComponent model component
      , Cmd.none)
    NewShipName name ->
      ( model & modelShipF => shipNameF .= name
      , Cmd.none )
    ChassisSelected chassisId ->
      case chassisId of
        Just x ->
          ( model & modelChassisF .= (selectChassis model.chassisList x)
          , Cmd.none )
        Nothing ->
          ( model & modelChassisF .= Nothing
          , Cmd.none )

selectChassis : List Chassis -> Int -> Maybe Chassis
selectChassis chassisList chassisId =
  List.head <| List.filter (\chassis -> chassis.id == chassisId) chassisList

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
        (model & modelShipF => shipComponentsF $= List.filter (\x -> not <| search x)) 
          & modelShipF => shipComponentsF $= List.append [InstalledComponent component newCount]
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
      (model & modelShipF => shipComponentsF $= List.filter (\x -> not <| search x))
        & modelShipF => shipComponentsF $= List.append [InstalledComponent component newCount]
