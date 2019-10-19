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
        , timeA
        )
import Data.Common exposing (error, nextStarDate)
import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ViewModels.Admin
    exposing
        ( ActionStatus(..)
        , AdminRMsg(..)
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
        [ div [ class "row" ]
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
        [ div [ class "row" ]
            [ div [ class "col-lg-2" ] (processTurnButton model)
            , div [ class "col-lg-2" ] (confirmProcessTurnButton model)
            , div [ class "col-lg-2" ] (cancelProcessTurnButton model)
            ]
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


{-| html for process turn button
-}
processTurnButton : Model -> List (Html Msg)
processTurnButton model =
    let
        buttonClass =
            if processTurnButtonActive model then
                "btn btn-primary btn-block command-button"

            else
                "btn btn-primary btn-block command-button disabled"
    in
    [ div
        [ class buttonClass
        , onClick <| AdminMessage <| ProcessTurn Requested
        ]
        [ text "Process turn" ]
    ]


{-| html for confirmation button
-}
confirmProcessTurnButton : Model -> List (Html Msg)
confirmProcessTurnButton model =
    if confirmProcessTurnButtonVisible model then
        [ div
            [ class "btn btn-primary btn-block command-button"
            , onClick <| AdminMessage <| ProcessTurn Confirmed
            ]
            [ text "Confirm" ]
        ]

    else
        []


{-| html for cancel process turn button
-}
cancelProcessTurnButton : Model -> List (Html Msg)
cancelProcessTurnButton model =
    if confirmProcessTurnButtonVisible model then
        [ div
            [ class "btn btn-primary btn-block command-button"
            , onClick <| AdminMessage <| ProcessTurn Inactive
            ]
            [ text "Cancel" ]
        ]

    else
        []


{-| Is process turn button active
-}
processTurnButtonActive : Model -> Bool
processTurnButtonActive model =
    case model.adminR.simulation of
        Just _ ->
            True

        Nothing ->
            False


{-| Is confirm turn processing button available
-}
confirmProcessTurnButtonVisible : Model -> Bool
confirmProcessTurnButtonVisible model =
    if processTurnButtonActive model then
        case model.adminR.processTurn of
            Requested ->
                True

            _ ->
                False

    else
        False


{-| Display people page
-}
displayPeople : Model -> Html Msg
displayPeople model =
    div [] [ text "people" ]


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

        ProcessTurn Inactive ->
            ( set (adminRA << processTurnA) Inactive model
            , Cmd.none
            )

        ProcessTurn Requested ->
            ( set (adminRA << processTurnA) Requested model
            , Cmd.none
            )

        ProcessTurn Confirmed ->
            ( set (adminRA << processTurnA) Confirmed model
            , case model.adminR.simulation of
                Just simulation ->
                    putSimulationStatus (AdminMessage << SimulationStatusReceived)
                        (set timeA (nextStarDate simulation.time) simulation)

                Nothing ->
                    Cmd.none
            )
