module Views.Construction exposing (init, page)

import Data.Common exposing (PlanetId)
import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..))
import Html exposing (..)
import Html.Attributes exposing (..)


page : Model -> Html Msg
page model =
    div []
        [ text "Hello from Construction" ]


init : Model -> Cmd Msg
init model =
    Cmd.none
