import Html exposing (program)
import Types exposing (..)
import Render

main : Program Never Model Msg
main =
  program { init = init
          , view = Render.view
          , update = update 
          , subscriptions = subscriptions }

init : (Model, Cmd Msg)
init = ( { searchText = "" }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TextSearch _ ->
      ( model
      , Cmd.none )
