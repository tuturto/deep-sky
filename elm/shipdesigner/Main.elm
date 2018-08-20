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
    , ship = Ship [] "" Nothing
    , chassis = Nothing
    , chassisList = []
    , errors = []
    , mode = EditMode
    , designList = []
    }                 
  , Cmd.batch [ Http.send AvailableComponents (Http.get "/api/components" (Decode.list Json.componentDecoder))
              , Http.send AvailableChassis (Http.get "/api/chassis" (Decode.list Json.chassisDecoder))
              , Http.send AvailableDesigns (Http.get "/api/design" (Decode.list Json.shipDecoder))
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
    AvailableDesigns (Err x) ->
      ( model & modelErrorsF $= List.append [ "Failed to load design list" ]
      , Cmd.none )
    AvailableDesigns (Ok designs) ->
      ( model & modelDesignsF .= List.map (flip Json.dtoToShip <| model.components) designs
      , Cmd.none )
    ChassisSelected (Just chassisId) ->
      ( model & modelChassisF .= (selectChassis model.chassisList chassisId)
      , Cmd.none )
    ChassisSelected Nothing ->
      ( model & modelChassisF .= Nothing
      , Cmd.none )
    SaveDesign ->
      ( model
      , Http.send DesignSaved <| saveCommand model.ship model.chassis)
    DesignSaved (Ok ship) ->
      ( model & modelShipF .= Json.dtoToShip ship model.components
      , Cmd.none )
    DesignSaved (Err x) ->
      ( model & modelErrorsF $= List.append [ "Failed to save design" ]
      , Cmd.none )
    LoadDesign ->
      ( { model | mode = LoadMode }
      , Http.send AvailableDesigns (Http.get "/api/design" (Decode.list Json.shipDecoder)))
    CancelLoad ->
      ( { model | mode = EditMode }
      , Cmd.none )
    ResetDesign ->
      ( model, Cmd.none )


send : String -> String -> Http.Body -> Decode.Decoder a -> Http.Request a
send method url body decoder =
  Http.request
    { method = method
    , headers = []
    , url = url
    , body = body
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

saveCommand : Ship -> Maybe Chassis -> Http.Request ShipDto
saveCommand ship chassis =
  case ship.id of
    Nothing ->
      send "POST" "/api/design" (Http.jsonBody <| Json.shipSaveEncoder ship chassis) Json.shipDecoder
    Just id ->
      send "PUT" ("/api/design/" ++ toString id) (Http.jsonBody <| Json.shipSaveEncoder ship chassis) Json.shipDecoder

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
