module Views.Research exposing (init, page)

import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..))
import Html exposing (..)
import Html.Attributes exposing (..)


page : Model -> Html Msg
page model =
    div []
        [ text "Hello from Research" ]


init : Model -> Cmd msg
init model =
    Cmd.none
