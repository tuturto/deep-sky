module Views.Admin.Main exposing (init, page, update)

import Accessors exposing (get, over, set)
import Accessors.Library exposing (try)
import Api.Admin exposing (getSimulationStatus, putSimulationStatus)
import Data.Accessors
    exposing
        ( adminRA
        , currentPageA
        , currentTimeA
        , errorsA
        , processTurnA
        , simulationA
        , statusA
        , timeA
        )
import Data.Admin exposing (SystemStatus(..))
import Data.Common exposing (Route(..), error, nextStarDate)
import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ViewModels.Admin.Main exposing (AdminRMsg(..))
import Views.Admin.Menu exposing (adminLayout, mainMenu)
import Views.Helpers exposing (href)


{-| Render admin view
-}
page : Model -> Html Msg
page model =
    adminLayout mainMenu
        []
        model


{-| Initialize data retrieval from server
-}
init : Model -> Cmd Msg
init model =
    getSimulationStatus (AdminMessage << SimulationStatusReceived)


{-| Handle incoming messages
-}
update : AdminRMsg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SimulationStatusReceived (Err err) ->
            ( set (adminRA << simulationA) Nothing model
                |> over errorsA (\errors -> error err "Failed to load simulation status" :: errors)
            , Cmd.none
            )

        SimulationStatusReceived (Ok simulation) ->
            ( set (adminRA << simulationA) (Just simulation) model
                |> set currentTimeA (Just simulation.time)
            , Cmd.none
            )

        ChangeStatusRequested ProcessingTurn ->
            ( model
            , case model.adminR.simulation of
                Just simulation ->
                    putSimulationStatus (AdminMessage << SimulationStatusReceived)
                        (set timeA (nextStarDate simulation.time) simulation)

                Nothing ->
                    Cmd.none
            )

        ChangeStatusRequested status ->
            ( model
            , case model.adminR.simulation of
                Just simulation ->
                    putSimulationStatus (AdminMessage << SimulationStatusReceived)
                        (set statusA status simulation)

                Nothing ->
                    Cmd.none
            )
