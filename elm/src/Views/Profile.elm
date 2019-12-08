module Views.Profile exposing (init, page)

import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..))
import Html exposing (Html, div, text)


page : Model -> Html Msg
page _ =
    div []
        [ text "Hello from Profile" ]


init : Model -> Cmd msg
init _ =
    Cmd.none
