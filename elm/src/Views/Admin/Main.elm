module Views.Admin.Main exposing
    ( init
    , isReady
    , page
    , update
    )

import Accessors exposing (over, set)
import Api.Admin exposing (getSimulationStatus, putSimulationStatus)
import Data.Accessors
    exposing
        ( adminRA
        , currentTimeA
        , errorsA
        , simulationA
        , statusA
        , timeA
        )
import Data.Admin exposing (SystemStatus(..))
import Data.Common exposing (Route(..), error, nextStarDate)
import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..))
import Html exposing (Html)
import RemoteData exposing (RemoteData(..))
import ViewModels.Admin.Main exposing (AdminRMsg(..))
import Views.Admin.Menu exposing (adminLayout, mainMenu)


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
init _ =
    getSimulationStatus (AdminMessage << SimulationStatusReceived)


isReady : Model -> Bool
isReady model =
    let
        vm =
            model.adminR
    in
    vm.simulation
        |> RemoteData.isLoading
        |> not


{-| Handle incoming messages
-}
update : AdminRMsg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SimulationStatusReceived (Failure err) ->
            ( set (adminRA << simulationA) (Failure err) model
                |> over errorsA (\errors -> error err "Failed to load simulation status" :: errors)
            , Cmd.none
            )

        SimulationStatusReceived (Success simulation) ->
            ( set (adminRA << simulationA) (Success simulation) model
                |> set currentTimeA (Just simulation.time)
            , Cmd.none
            )

        SimulationStatusReceived Loading ->
            ( model
            , Cmd.none
            )

        SimulationStatusReceived NotAsked ->
            ( model
            , Cmd.none
            )

        ChangeStatusRequested ProcessingTurn ->
            ( set (adminRA << simulationA) Loading model
            , case model.adminR.simulation of
                Success simulation ->
                    putSimulationStatus (AdminMessage << SimulationStatusReceived)
                        (set timeA (nextStarDate simulation.time) simulation)

                _ ->
                    Cmd.none
            )

        ChangeStatusRequested status ->
            ( set (adminRA << simulationA) Loading model
            , case model.adminR.simulation of
                Success simulation ->
                    putSimulationStatus (AdminMessage << SimulationStatusReceived)
                        (set statusA status simulation)

                _ ->
                    Cmd.none
            )
