module Views.Admin.People.Add exposing (init, page, update)

import Data.Model exposing (Model, Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


{-| Render add person view
-}
page : Model -> Html Msg
page model =
    div [] []


{-| Initialize data retrieval from server
-}
init : Model -> Cmd Msg
init model =
    Cmd.none


{-| Handle incoming messages
-}
update : msg -> Model -> ( Model, Cmd Msg )
update message model =
    ( model, Cmd.none )
