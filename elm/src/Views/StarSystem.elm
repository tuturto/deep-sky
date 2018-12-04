module Views.StarSystem exposing (init, page, update)

import Accessors exposing (get, over, set)
import Api.StarSystem exposing (getPlanetsCmd, getStarSystemsCmd, getStarsCmd)
import Data.Accessors
    exposing
        ( planetsA
        , planetsStatusA
        , starLanesStatusA
        , starListStatusA
        , starSystemsA
        , starSystemsRA
        , starsA
        , systemDetailsStatusA
        )
import Data.Common
    exposing
        ( InfoPanelStatus(..)
        , Route(..)
        , StarSystemId(..)
        , locationToString
        , unStarSystemId
        )
import Data.Model exposing (Model, Msg(..))
import Data.StarSystem
    exposing
        ( PlanetPosition(..)
        , gravityToString
        , stellarClassification
        , unPlanetPosition
        )
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (andThen, withDefault)
import ViewModels.StarSystem exposing (StarSystemRMsg(..))
import Views.Helpers exposing (href, infoPanel, starDateToString, twinPanels)


page : StarSystemId -> Model -> Html Msg
page systemId model =
    div [] <| twinPanels (leftPanel systemId) (rightPanel systemId) model


leftPanel : StarSystemId -> Model -> List (Html Msg)
leftPanel systemId model =
    let
        viewModel =
            model.starSystemsR
    in
    infoPanel "System details"
        (systemDetails systemId)
        viewModel.systemDetailsStatus
        (StarSystemMessage <| SystemDetailsStatusChanged InfoPanelOpen)
        (StarSystemMessage <| SystemDetailsStatusChanged InfoPanelClosed)
        model
        ++ infoPanel "Stars"
            (starsInfo systemId)
            viewModel.starListStatus
            (StarSystemMessage <| StarListStatusChanged InfoPanelOpen)
            (StarSystemMessage <| StarListStatusChanged InfoPanelClosed)
            model
        ++ infoPanel "Star lanes"
            (starLanesInfo systemId)
            viewModel.starLanesStatus
            (StarSystemMessage <| StarLaneListStatusChanged InfoPanelOpen)
            (StarSystemMessage <| StarLaneListStatusChanged InfoPanelClosed)
            model


rightPanel : StarSystemId -> Model -> List (Html Msg)
rightPanel systemId model =
    let
        viewModel =
            model.starSystemsR
    in
    infoPanel "Planets"
        (planetsInfo systemId)
        viewModel.planetsStatus
        (StarSystemMessage <| PlanetListStatusChanged InfoPanelOpen)
        (StarSystemMessage <| PlanetListStatusChanged InfoPanelClosed)
        model


systemDetails : StarSystemId -> Model -> List (Html Msg)
systemDetails systemId model =
    let
        system =
            get starSystemsA model
                |> andThen (Dict.get (unStarSystemId systemId))
    in
    div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "Location" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
        :: [ div [ class "row" ]
                [ div [ class "col-lg-4" ]
                    [ system
                        |> andThen (\x -> Just x.name)
                        |> withDefault ""
                        |> text
                    ]
                , div [ class "col-lg-4" ]
                    [ system
                        |> andThen (\x -> Just <| locationToString x.location)
                        |> withDefault ""
                        |> text
                    ]
                , div [ class "col-lg-4" ]
                    [ system
                        |> andThen (\x -> Just <| starDateToString x.date)
                        |> withDefault ""
                        |> text
                    ]
                ]
           ]


starsInfo : StarSystemId -> Model -> List (Html Msg)
starsInfo systemId model =
    div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "Class" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
        :: (get starsA model
                |> andThen (Dict.get (unStarSystemId systemId))
                |> withDefault []
                |> List.map
                    (\entry ->
                        div [ class "row" ]
                            [ div [ class "col-lg-4" ] [ text entry.name ]
                            , div [ class "col-lg-4" ] [ text <| stellarClassification entry ]
                            , div [ class "col-lg-4" ] [ text <| starDateToString entry.date ]
                            ]
                    )
           )


starLanesInfo : StarSystemId -> Model -> List (Html Msg)
starLanesInfo systemId model =
    []


planetsInfo : StarSystemId -> Model -> List (Html Msg)
planetsInfo systemId model =
    div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Name" ]
        , div [ class "col-lg-1" ] [ text "Pos." ]
        , div [ class "col-lg-2" ] [ text "Gravity" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
        :: (get planetsA model
                |> andThen (Dict.get (unStarSystemId systemId))
                |> withDefault []
                |> List.map
                    (\entry ->
                        div [ class "row" ]
                            [ div [ class "col-lg-4" ]
                                [ a [ href (PlanetR systemId entry.id) ] [ text entry.name ] ]
                            , div [ class "col-lg-1" ]
                                [ entry.position
                                    |> andThen (\(PlanetPosition x) -> Just <| String.fromInt x)
                                    |> withDefault ""
                                    |> text
                                ]
                            , div [ class "col-lg-2" ]
                                [ entry.gravity
                                    |> andThen (\gravity -> Just <| gravityToString gravity)
                                    |> withDefault ""
                                    |> text
                                ]
                            , div [ class "col-lg-4" ] [ text <| starDateToString entry.date ]
                            ]
                    )
           )


init : StarSystemId -> Model -> Cmd Msg
init systemId model =
    Cmd.batch
        [ getStarsCmd model
        , getStarSystemsCmd model
        , getPlanetsCmd model
        ]


update : StarSystemRMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SystemDetailsStatusChanged status ->
            ( set (starSystemsRA << systemDetailsStatusA) status model, Cmd.none )

        StarListStatusChanged status ->
            ( set (starSystemsRA << starListStatusA) status model, Cmd.none )

        StarLaneListStatusChanged status ->
            ( set (starSystemsRA << starLanesStatusA) status model, Cmd.none )

        PlanetListStatusChanged status ->
            ( set (starSystemsRA << planetsStatusA) status model, Cmd.none )