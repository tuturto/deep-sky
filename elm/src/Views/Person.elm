module Views.Person exposing (init, page, update)

{-| Page displaying person details. The shown information depends on human
intelligence level. The higher the level, the more details are shown.
-}

import Api.People exposing (personDetails)
import Data.Common exposing (PersonId)
import Data.Model exposing (Model, Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import ViewModels.Person exposing (PersonRMsg(..), PersonViewModel)


{-| Initiate retrieval of data needed by this page
-}
init : PersonId -> Model -> Cmd Msg
init pId model =
    Cmd.batch [ personDetails (PersonMessage << PersonDetailsReceived) pId ]


{-| Render page of displaying the person
-}
page : PersonId -> Model -> Html Msg
page pId model =
    div [] [ text "to be done" ]


{-| Handle messages specific to this page
-}
update : PersonRMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
