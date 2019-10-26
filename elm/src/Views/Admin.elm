module Views.Admin exposing (init, page, update)

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
import Data.Common exposing (error, nextStarDate)
import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ViewModels.Admin
    exposing
        ( AdminRMsg(..)
        , DisplayMode(..)
        )


{-| Render admin view
-}
page : Model -> Html Msg
page model =
    case model.adminR.currentPage of
        AdminMenu ->
            displayMainMenu model

        SimulationStatusMenu ->
            displaySimulationStatus model

        PeopleMenu ->
            displayPeople model


{-| Display main menu
-}
displayMainMenu : Model -> Html Msg
displayMainMenu model =
    div []
        [ simulationStatusBlock model
        , div [ class "row" ]
            [ div [ class "col-lg-6" ]
                -- left column
                [ div [ class "row" ]
                    [ div [ class "col-lg-4" ]
                        [ div
                            [ class "btn btn-primary btn-block command-button"
                            , onClick <| AdminMessage <| DisplayPage SimulationStatusMenu
                            ]
                            [ text "Simulation status" ]
                        ]
                    , div [ class "col-lg-8" ]
                        [ div [ class "space-top-sm" ] [ text "Set simulation on maintenance or online. Trigger processing of next turn." ] ]
                    ]
                , div [ class "row" ]
                    [ div [ class "col-lg-4" ]
                        [ div
                            [ class "btn btn-primary btn-block command-button"
                            , onClick <| AdminMessage <| DisplayPage PeopleMenu
                            ]
                            [ text "People" ]
                        ]
                    , div [ class "col-lg-8" ]
                        [ div [ class "space-top-sm" ] [ text "View, edit and create people." ] ]
                    ]
                ]
            , div [ class "col-lg-6" ]
                -- right column
                []
            ]
        ]


{-| Display simulation status page
-}
displaySimulationStatus : Model -> Html Msg
displaySimulationStatus model =
    div []
        [ simulationStatusBlock model
        , div [ class "row space-top" ]
            [ div [ class "col-lg-2" ]
                [ div
                    [ class "btn btn-primary btn-block command-button"
                    , onClick <| AdminMessage <| DisplayPage AdminMenu
                    ]
                    [ text "Admin menu" ]
                ]
            ]
        ]


statusButton : Maybe SystemStatus -> SystemStatus -> String -> Html Msg
statusButton status target txt =
    case status of
        Just x ->
            if x /= target then
                div [ class "col-lg-2" ]
                    [ div
                        [ class "btn btn-default btn-block btn-sm command-button"
                        , onClick (AdminMessage <| ChangeStatusRequested target)
                        ]
                        [ text txt ]
                    ]

            else
                div [ class "col-lg-2" ]
                    [ div
                        [ class "btn btn-primary btn-block btn-sm command-button"
                        ]
                        [ text txt ]
                    ]

        Nothing ->
            div [] []


{-| html div displaying current system status
-}
simulationStatusBlock : Model -> Html Msg
simulationStatusBlock model =
    let
        status =
            get (adminRA << simulationA << try << statusA) model
    in
    div [ class "row" ]
        [ div [ class "col-lg-2" ] [ text " " ]
        , statusButton status Offline "Offline"
        , statusButton status Maintenance "Maintenance"
        , statusButton status Online "Online"
        , statusButton status ProcessingTurn "Processing turn"
        ]


{-| Display people page
-}
displayPeople : Model -> Html Msg
displayPeople model =
    div [] [ simulationStatusBlock model ]


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

        DisplayPage newPage ->
            ( set (adminRA << currentPageA) newPage model
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
