import Html exposing ( programWithFlags )
import Http
import Types exposing (..)
import Json exposing ( buildingInfoDecoder, buildingDecoder, populationDecoder, planetDetailsDecoder, constructionDecoder )
import Json.Decode as Decode
import Render

main : Program Int Model Msg
main =
  programWithFlags { init = init
                   , view = Render.view
                   , update = update 
                   , subscriptions = subscriptions }

init : Int -> (Model, Cmd Msg)
init planetId = 
  ( { searchText = ""
    , buildings = []
    , availableBuildings = []
    , messages = []
    , planetId = planetId
    , population = []
    , planetDetails = Nothing
    , constructionQueue = [] }
  , Cmd.batch 
    [ Http.send (NetworkMsg << BuildingInfoLoaded) (Http.get "/api/construction/buildings" (Decode.list buildingInfoDecoder))
    , Http.send (NetworkMsg << BuildingsLoaded) (Http.get ("/api/planet/" ++ (toString planetId) ++ "/buildings") (Decode.list buildingDecoder))
    , Http.send (NetworkMsg << PopulationLoaded) (Http.get ("/api/planet/" ++ (toString planetId) ++ "/population") (Decode.list populationDecoder))
    , Http.send (NetworkMsg << PlanetDetailsLoaded) (Http.get ("/api/planet/" ++ (toString planetId)) planetDetailsDecoder)
    , Http.send (NetworkMsg << ConstructionsLoaded) (Http.get ("/api/construction/planet/" ++ (toString planetId)) (Decode.list constructionDecoder))
    ]
  )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TextSearch _ ->
      ( model
      , Cmd.none )
    NetworkMsg msg ->
      handleNetworkMessage msg model

handleNetworkMessage : ApiMsg -> Model -> (Model, Cmd Msg)
handleNetworkMessage msg model =
  case msg of
    BuildingInfoLoaded (Ok buildings) ->
      ( { model | availableBuildings = buildings }
      , Cmd.none )
    BuildingInfoLoaded (Err _) ->
      let
        messages = "Error loading building info" :: model.messages 
      in 
        ( { model | messages = messages }, Cmd.none)
    BuildingsLoaded (Ok buildings) ->
      ( { model | buildings = buildings }
      , Cmd.none )
    BuildingsLoaded (Err _) ->
      let
        messages = "Error loading buildings" :: model.messages 
      in 
        ( { model | messages = messages }, Cmd.none)
    PopulationLoaded (Ok population) ->
      ( { model | population = population }
      , Cmd.none )
    PopulationLoaded (Err _) ->
      let
        messages = "Error loading population" :: model.messages 
      in 
        ( { model | messages = messages }, Cmd.none)
    PlanetDetailsLoaded (Ok details) ->
      ( { model | planetDetails = Just details }
      , Cmd.none )
    PlanetDetailsLoaded (Err _) ->
      let
        messages = "Error loading planet details" :: model.messages 
      in 
        ( { model | messages = messages }, Cmd.none)
    ConstructionsLoaded (Ok queue) ->
      ( { model | constructionQueue = queue }
      , Cmd.none )
    ConstructionsLoaded (Err err) ->
      let
        messages = "Error loading construction queue" :: model.messages 
      in 
        ( { model | messages = messages }, Cmd.none)
