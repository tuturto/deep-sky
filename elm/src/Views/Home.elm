module Views.Home exposing (init, page)

import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..))
import Html exposing (Html, div, text)


page : Model -> Html Msg
page _ =
    div []
        [ text "Hello from Home" ]


init : Model -> Cmd msg
init _ =
    Cmd.none
