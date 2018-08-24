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
    , ship = Ship [] "" Nothing Nothing
    , chassisList = []
    , errors = []
    , mode = EditMode
    , designList = []
    }                 
  , Cmd.batch [ Http.send (NetworkMsg << AvailableComponents) (Http.get "/api/components" (Decode.list Json.componentDecoder))
              , Http.send (NetworkMsg << AvailableChassis) (Http.get "/api/chassis" (Decode.list Json.chassisDecoder))
              , Http.send (NetworkMsg << AvailableDesigns) (Http.get "/api/design" (Decode.list Json.shipDecoder))
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
    EditMsg msg ->
      handleEditMsg msg model
    NetworkMsg msg ->
      handleNetworkMsg msg model
    ButtonMsg msg ->
      handleButtonMsg msg model

handleEditMsg : ShipMsg -> Model -> (Model, Cmd Msg)
handleEditMsg msg model =
  case msg of
    AddComponent component ->
      ( addComponent model component
      , Cmd.none )
    RemoveComponent component ->
      ( removeComponent model component
      , Cmd.none )
    NewShipName name ->
      ( model & modelShipF => shipNameF .= name
      , Cmd.none )
    ChassisSelected (Just chassisId) ->
      ( model & modelShipF => shipChassisF .= (selectChassis model.chassisList chassisId)
      , Cmd.none )
    ChassisSelected Nothing ->
      ( model & modelShipF => shipChassisF .= Nothing
      , Cmd.none )

handleButtonMsg : UiMsg -> Model -> (Model, Cmd Msg)
handleButtonMsg msg model =
  case msg of
    SaveDesign ->
      ( model
      , Http.send (NetworkMsg << DesignSaved) <| saveCommand model.ship model.ship.chassis)
    ShowLoadPanel ->
      ( { model | mode = LoadMode }
      , Http.send (NetworkMsg << AvailableDesigns) (Http.get "/api/design" (Decode.list Json.shipDecoder)))
    LoadDesign ship ->
      ( { model | ship = ship, mode = EditMode }
      , Cmd.none )
    CancelLoad ->
      ( { model | mode = EditMode }
      , Cmd.none )
    ResetDesign ->
      ( model & modelShipF .= (resetDesign model.designList model.ship.id)
      , Cmd.none )
    NewDesign ->
      ( model & modelShipF .= Ship [] "" Nothing Nothing
      , Cmd.none )
    CopyDesign ->
      ( model & modelShipF => shipIdF .= Nothing
      , Cmd.none )
    DeleteDesign design ->
      case design.id of
        Nothing -> ( model, Cmd.none )
        Just dId ->
          ( model
          , Http.send (NetworkMsg << DesignDeleted) (send "DELETE" ("/api/design/" ++ toString dId) Http.emptyBody Decode.int))

handleNetworkMsg : ApiMsg -> Model -> (Model, Cmd Msg)
handleNetworkMsg msg model =
  case msg of
    AvailableComponents (Ok components) ->
      ( model & modelComponentsF .= components
      , Cmd.none )
    AvailableComponents (Err _) ->
      ( model & modelErrorsF $= List.append [ "Failed to load component list" ]
      , Cmd.none )
    AvailableChassis (Ok chassis) ->
      ( model & modelChassisListF .= chassis
      , Cmd.none )
    AvailableChassis (Err _) ->
      ( model & modelErrorsF $= List.append [ "Failed to load chassis list" ]
      , Cmd.none )
    AvailableDesigns (Err _) ->
      ( model & modelErrorsF $= List.append [ "Failed to load design list" ]
      , Cmd.none )
    AvailableDesigns (Ok designs) ->
      ( model & modelDesignsF .= List.map (\x -> Json.dtoToShip x model.components model.chassisList) designs
      , Cmd.none )
    DesignDeleted (Ok id) ->
      (model & modelDesignsF $= (List.filter <| \x -> x.id /= Just id)
      , Cmd.none)
    DesignDeleted (Err _) ->
      ( model & modelErrorsF $= List.append [ "Failed to delete design" ]
      , Cmd.none )
    DesignSaved (Ok ship) ->
      ( model & modelShipF .= Json.dtoToShip ship model.components model.chassisList
      , Cmd.none )
    DesignSaved (Err x) ->
      ( model & modelErrorsF $= List.append [ "Failed to save design" ]
      , Cmd.none )

resetDesign : List Ship -> Maybe Int -> Ship
resetDesign designs id =
  let
    match = designs
            |> List.filter (\design -> design.id == id)
            |> List.head
  in
    case match of
      Nothing -> Ship [] "" Nothing Nothing
      Just design -> design

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
