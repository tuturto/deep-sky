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
init = ( { message = "Hello" }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> 
      ( model
      , Cmd.none )
