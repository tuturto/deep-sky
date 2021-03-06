module Views.Admin.Menu exposing
    ( adminLayout
    , mainMenu
    , personMenu
    , statusMenu
    )

import Data.Admin exposing (SystemStatus(..))
import Data.Common exposing (Route(..))
import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..))
import Html exposing (Attribute, Html, a, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..), WebData)
import ViewModels.Admin.Main exposing (AdminRMsg(..))
import Views.Helpers exposing (href)


adminLayout : List (Html Msg) -> List (Html Msg) -> Model -> Html Msg
adminLayout menu content model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-lg-12" ]
                [ simulationStatusBlock model ]
            ]
        , div [ class "row" ]
            [ div [ class "col-lg-2" ]
                menu
            , div [ class "col-lg-10 space-top-sm" ]
                content
            ]
        ]


sideMenu : MenuLevel -> List (Html Msg)
sideMenu level =
    [ div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ a [ href AdminListPeopleR, buttonClass StatusMenu level ] [ text "Status" ] ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ a [ href AdminListPeopleR, buttonClass PersonMenu level ] [ text "People" ] ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ a [ href AdminR, buttonClass MainMenu level ] [ text "Admin" ] ]
        ]
    ]


buttonClass : MenuLevel -> MenuLevel -> Attribute Msg
buttonClass level current =
    if level == current then
        class "btn btn-block btn-primary command-button"

    else
        class "btn btn-block btn-default command-button"


type MenuLevel
    = MainMenu
    | StatusMenu
    | PersonMenu


statusMenu : List (Html Msg)
statusMenu =
    sideMenu StatusMenu


personMenu : List (Html Msg)
personMenu =
    sideMenu PersonMenu


mainMenu : List (Html Msg)
mainMenu =
    sideMenu MainMenu


{-| html div displaying current system status
-}
simulationStatusBlock : Model -> Html Msg
simulationStatusBlock model =
    let
        status =
            RemoteData.map .status model.adminR.simulation
    in
    div [ class "row" ]
        [ div [ class "col-lg-2" ] [ text " " ]
        , statusButton status Offline "Offline"
        , statusButton status Maintenance "Maintenance"
        , statusButton status Online "Online"
        , statusButton status ProcessingTurn "Processing turn"
        ]


statusButton : WebData SystemStatus -> SystemStatus -> String -> Html Msg
statusButton status target txt =
    case status of
        Success x ->
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

        _ ->
            div [ class "col-lg-2" ]
                [ div
                    [ class "btn btn-default btn-block btn-sm command-button"
                    ]
                    [ text txt ]
                ]

