module Views.Construction exposing (init, page)

import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..))
import Html exposing (Html, div, text)


page : Model -> Html Msg
page _ =
    div []
        [ text "Hello from Construction" ]


init : Model -> Cmd Msg
init _ =
    Cmd.none
