import Html exposing ( program )
import Http
import Types exposing (..)
import Json exposing ( buildingInfoDecoder )
import Json.Decode as Decode
import Render

main : Program Never Model Msg
main =
  program { init = init
          , view = Render.view
          , update = update 
          , subscriptions = subscriptions }

init : (Model, Cmd Msg)
init = 
  ( { searchText = ""
    , availableBuildings = [] }
  , Cmd.batch 
    [
       Http.send (NetworkMsg << BuildingsAvailable) (Http.get "/api/construction/buildings" (Decode.list buildingInfoDecoder))
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
    BuildingsAvailable (Ok buildings) ->
      ( { model | availableBuildings = buildings }
      , Cmd.none )
    BuildingsAvailable (Err _) ->
      (model, Cmd.none)

