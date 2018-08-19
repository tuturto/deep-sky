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
  ( { components = []
    , ship = Ship [] ""
    , chassis = Nothing
    , chassisList = []
    , errors = []
    , mode = EditMode
    }                 
  , Cmd.batch [ Http.send AvailableComponents (Http.get "/api/components" (Decode.list Json.componentDecoder)) 
              , Http.send AvailableChassis (Http.get "/api/chassis" (Decode.list Json.chassisDecoder))
              ]
  )

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
      , Cmd.none )
    AvailableComponents (Err _) ->
      ( model & modelErrorsF $= List.append [ "Failed to load component list" ]
      , Cmd.none )
    AddComponent component ->
      ( addComponent model component
      , Cmd.none )
    RemoveComponent component ->
      ( removeComponent model component
      , Cmd.none )
    NewShipName name ->
      ( model & modelShipF => shipNameF .= name
      , Cmd.none )
    AvailableChassis (Ok chassis) ->
      ( model & modelChassisListF .= chassis
      , Cmd.none )
    AvailableChassis (Err _) ->
      ( model & modelErrorsF $= List.append [ "Failed to load chassis list" ]
      , Cmd.none )
    ChassisSelected (Just chassisId) ->
      ( model & modelChassisF .= (selectChassis model.chassisList chassisId)
      , Cmd.none )
    ChassisSelected Nothing ->
      ( model & modelChassisF .= Nothing
      , Cmd.none )
    SaveDesign ->
      ( model
      , Http.send DesignSaved <| Http.post "/api/design" (Http.jsonBody <| Json.shipSaveEncoder model.ship model.chassis) Json.shipDecoder)
    DesignSaved (Ok ship) ->
      ( model & modelShipF .= Json.dtoToShip ship model.components
      , Cmd.none )
    DesignSaved (Err x) ->
      ( model & modelErrorsF $= List.append [ "Failed to save design" ]
      , Cmd.none )
    LoadDesign ->
      ( { model | mode = LoadMode }
      , Cmd.none )
    CancelLoad ->
      ( { model | mode = EditMode }
      , Cmd.none )
    ResetDesign ->
      ( model, Cmd.none )

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
