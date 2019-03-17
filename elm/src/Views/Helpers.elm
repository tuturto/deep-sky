module Views.Helpers exposing
    ( InfoPanelConfig
    , PagingConfig
    , PanelSizing(..)
    , biologicalsToText
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
import Html exposing (Html, div, hr, i, span, text)
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


twinPanels : PanelSizing -> (Model -> List (Html msg)) -> (Model -> List (Html msg)) -> (Model -> List (Html msg))
twinPanels sizing left right model =
    let
        ( lpSize, rpSize ) =
            case sizing of
                EqualPanels ->
                    ( "col-lg-6", "col-lg-6" )

                LeftPanelSmall ->
                    ( "col-lg-4", "col-lg-8" )

                RightPanelSmall ->
                    ( "col-lg-8", "col-lg-4" )
    in
    [ div [ class "row" ]
        [ div [ class lpSize ]
            (left model)
        , div [ class rpSize ]
            (right model)
        ]
    ]


{-| Info panel layout
-}
type PanelSizing
    = EqualPanels
    | LeftPanelSmall
    | RightPanelSmall


{-| Configuration for info panel
-}
type alias InfoPanelConfig =
    { title : String
    , currentStatus : InfoPanelStatus
    , openingMessage : Msg
    , closingMessage : Msg
    , refreshMessage : Maybe Msg
    }


{-| Configuration for info panel paging
-}
type alias PagingConfig =
    { pageSize : Int
    , currentPage : Int
    , maxPage : Int
    , pageChangedMessage : Int -> Msg
    }


{-| General infopanel that is displayed on a page
Supports opening and closing displayed info, refreshing data from server and
paginating the data
-}
infoPanel : InfoPanelConfig -> Maybe PagingConfig -> (Model -> List (Html Msg)) -> Model -> List (Html Msg)
infoPanel config pagingConfig generator model =
    let
        ( lSize, rSize ) =
            case pagingConfig of
                Just _ ->
                    ( "col-lg-9", "col-lg-3" )

                Nothing ->
                    ( "col-lg-10", "col-lg-2" )
    in
    [ div [ class "row info-panel-header" ]
        [ div [ class lSize ]
            [ text config.title ]
        , div [ class rSize ]
            [ span [ class "pull-right" ] <| infoPanelButtons config pagingConfig
            ]
        ]
    ]
        ++ (case config.currentStatus of
                InfoPanelOpen ->
                    generator model

                InfoPanelClosed ->
                    []
           )
        ++ [ hr [] [] ]


{-| Button block for info panel
Contains according to configuration: refresh button, paging controls and open/close button
-}
infoPanelButtons : InfoPanelConfig -> Maybe PagingConfig -> List (Html Msg)
infoPanelButtons config pagingConfig =
    refreshButton config ++ pagingButtons pagingConfig ++ openCloseButton config


{-| Refresh button for info panel
Created button will create configured refresh message when clicked
-}
refreshButton : InfoPanelConfig -> List (Html Msg)
refreshButton config =
    [ i
        (case config.refreshMessage of
            Just message ->
                [ class "fas fa-sync", onClick message ]

            Nothing ->
                []
        )
        []
    ]


{-| Open / Close button for info panel
Created button will create configured open or close message when clicked.
Created message depends on the state of the info panel
-}
openCloseButton : InfoPanelConfig -> List (Html Msg)
openCloseButton config =
    [ case config.currentStatus of
        InfoPanelOpen ->
            i [ class "fas fa-chevron-up small-space-left", onClick config.closingMessage ] []

        InfoPanelClosed ->
            i [ class "fas fa-chevron-down small-space-left", onClick config.openingMessage ] []
    ]


{-| Paging buttons with current page and last page display in the middle
Created buttons will create configured message when clicked, providing that the transition makes
sense with the current config. For example, when current page is 0, no message would be created
when first page button or previous page button is clicked. Similarly last page button and next
page buttons only create message when current page is not the last one.
-}
pagingButtons : Maybe PagingConfig -> List (Html Msg)
pagingButtons config =
    case config of
        Nothing ->
            []

        Just pagingConfig ->
            let
                currPage =
                    pagingConfig.currentPage

                maxPage =
                    pagingConfig.maxPage
            in
            [ if currPage > 0 then
                i [ class "fas fa-fast-backward small-space-left", onClick (pagingConfig.pageChangedMessage 0) ] []

              else
                i [ class "fa fa-fast-backward small-space-left" ] []
            , if currPage > 0 then
                i [ class "fas fa-step-backward small-space-left", onClick (pagingConfig.pageChangedMessage (currPage - 1)) ] []

              else
                i [ class "fa fa-step-backward small-space-left" ] []
            , text <| String.fromInt (pagingConfig.currentPage + 1)
            , text " / "
            , text <| String.fromInt (pagingConfig.maxPage + 1)
            , if currPage < maxPage then
                i [ class "fas fa-step-forward small-space-left", onClick (pagingConfig.pageChangedMessage (currPage + 1)) ] []

              else
                i [ class "fa fa-step-forward small-space-left" ] []
            , if currPage < maxPage then
                i [ class "fas fa-fast-forward small-space-left", onClick (pagingConfig.pageChangedMessage maxPage) ] []

              else
                i [ class "fa fa-fast-forward small-space-left" ] []
            ]
