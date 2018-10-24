module Views.Helpers exposing
    ( biologicalsToText
    , chemicalsToText
    , href
    , infoPanel
    , mechanicalsToText
    , starDateToString
    , starDateToText
    , twinPanels
    )

import Data.Common
    exposing
        ( InfoPanelStatus(..)
        , Resources
        , Route
        , StarDate(..)
        , routeToString
        , unBio
        , unChem
        , unMech
        )
import Data.Model exposing (Model, Msg(..))
import Html exposing (Html, div, hr, i, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


starDateToString : StarDate -> String
starDateToString (StarDate currentTime) =
    if modBy 10 currentTime == 0 then
        (String.fromFloat <| toFloat currentTime / 10) ++ ".0"

    else
        String.fromFloat <| toFloat currentTime / 10


starDateToText : Maybe StarDate -> Html msg
starDateToText time =
    case time of
        Just x ->
            text <| starDateToString x

        Nothing ->
            text "-"


biologicalsToText : Maybe Resources -> List (Html msg)
biologicalsToText resources =
    let
        score =
            case resources of
                Just x ->
                    unBio x.biological
                        |> String.fromInt
                        |> text

                Nothing ->
                    text "-"
    in
    [ i [ class "fas fa-leaf" ] []
    , score
    ]


mechanicalsToText : Maybe Resources -> List (Html msg)
mechanicalsToText resources =
    let
        score =
            case resources of
                Just x ->
                    unMech x.mechanical
                        |> String.fromInt
                        |> text

                Nothing ->
                    text "-"
    in
    [ i [ class "fas fa-cogs" ] []
    , score
    ]


chemicalsToText : Maybe Resources -> List (Html msg)
chemicalsToText resources =
    let
        score =
            case resources of
                Just x ->
                    unChem x.chemical
                        |> String.fromInt
                        |> text

                Nothing ->
                    text "-"
    in
    [ i [ class "fas fa-flask" ] []
    , score
    ]


href : Route -> Html.Attribute msg
href route =
    Html.Attributes.href <| routeToString route


twinPanels : (Model -> List (Html Msg)) -> (Model -> List (Html Msg)) -> (Model -> List (Html Msg))
twinPanels left right model =
    [ div [ class "row" ]
        [ div [ class "col-lg-6" ]
            (left model)
        , div [ class "col-lg-6" ]
            (right model)
        ]
    ]


infoPanel : String -> (Model -> List (Html Msg)) -> InfoPanelStatus -> Msg -> Msg -> Model -> List (Html Msg)
infoPanel title generator status openMsg closeMsg model =
    [ div [ class "row info-panel-header" ]
        [ div [ class "col-lg-10" ]
            [ text title ]
        , div [ class "col-lg-2" ]
            [ i [ class "fas fa-sync" ] []
            , case status of
                InfoPanelOpen ->
                    i [ class "fas fa-chevron-up small-space-left", onClick closeMsg ] []

                InfoPanelClosed ->
                    i [ class "fas fa-chevron-down small-space-left", onClick openMsg ] []
            ]
        ]
    ]
        ++ (case status of
                InfoPanelOpen ->
                    generator model

                InfoPanelClosed ->
                    []
           )
        ++ [ hr [] [] ]
