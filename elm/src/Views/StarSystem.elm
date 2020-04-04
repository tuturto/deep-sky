module Views.StarSystem exposing (init, page, update)

import Accessors exposing (get, over, set)
import Api.StarSystem
    exposing
        ( getPlanetsCmd
        , getStarSystemCmd
        , getStarsCmd
        )
import Data.Accessors
    exposing
        ( errorsA
        , planetsStatusA
        , starLanesStatusA
        , starListStatusA
        , starSystemA
        , starSystemRA
        , systemDetailsStatusA
        )
import Data.Common
    exposing
        ( InfoPanelStatus(..)
        , Route(..)
        , StarSystemId(..)
        , error
        , locationToString
        , unPlanetName
        , unStarName
        , unStarSystemId
        , unStarSystemName
        )
import Data.Model exposing (Model, Msg(..))
import Data.PersonNames exposing (displayName, unShortTitle)
import Data.StarSystem
    exposing
        ( PlanetPosition(..)
        , gravityToString
        , stellarClassification
        )
import Dict
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, id)
import Maybe exposing (andThen, withDefault)
import ViewModels.StarSystem exposing (StarSystemRMsg(..))
import Views.Helpers
    exposing
        ( PanelSizing(..)
        , href
        , infoPanel
        , starDateToString
        , twinPanels
        )


page : StarSystemId -> Model -> Html Msg
page systemId model =
    div [] <| twinPanels EqualPanels (leftPanel systemId) (rightPanel systemId) model


leftPanel : StarSystemId -> Model -> List (Html Msg)
leftPanel systemId model =
    let
        viewModel =
            model.starSystemR
    in
    infoPanel
        { title = "System details"
        , currentStatus = viewModel.systemDetailsStatus
        , openingMessage = StarSystemMessage <| SystemDetailsStatusChanged InfoPanelOpen
        , closingMessage = StarSystemMessage <| SystemDetailsStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        (systemDetails systemId)
        model
        ++ infoPanel
            { title = "Stars"
            , currentStatus = viewModel.starListStatus
            , openingMessage = StarSystemMessage <| StarListStatusChanged InfoPanelOpen
            , closingMessage = StarSystemMessage <| StarListStatusChanged InfoPanelClosed
            , refreshMessage = Nothing
            }
            Nothing
            (starsInfo systemId)
            model
        ++ infoPanel
            { title = "Star lanes"
            , currentStatus = viewModel.starLanesStatus
            , openingMessage = StarSystemMessage <| StarLaneListStatusChanged InfoPanelOpen
            , closingMessage = StarSystemMessage <| StarLaneListStatusChanged InfoPanelClosed
            , refreshMessage = Nothing
            }
            Nothing
            (starLanesInfo systemId)
            model


rightPanel : StarSystemId -> Model -> List (Html Msg)
rightPanel systemId model =
    let
        viewModel =
            model.starSystemR
    in
    infoPanel
        { title = "Planets"
        , currentStatus = viewModel.planetsStatus
        , openingMessage = StarSystemMessage <| PlanetListStatusChanged InfoPanelOpen
        , closingMessage = StarSystemMessage <| PlanetListStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        (planetsInfo systemId)
        model


systemDetails : StarSystemId -> Model -> List (Html Msg)
systemDetails _ model =
    let
        name =
            model.starSystemR.starSystem
                |> andThen (\x -> Just (unStarSystemName x.name))
                |> withDefault ""
                |> text

        location =
            model.starSystemR.starSystem
                |> andThen (\x -> Just <| locationToString x.location)
                |> withDefault ""
                |> text

        date =
            model.starSystemR.starSystem
                |> andThen (\x -> Just <| starDateToString x.date)
                |> withDefault ""
                |> text

        title =
            model.starSystemR.starSystem
                |> andThen (\x -> x.rulerTitle)
                |> andThen (Just << unShortTitle)
                |> withDefault " "

        rulerText =
            model.starSystemR.starSystem
                |> andThen (\x -> x.rulerName)
                |> andThen (\x -> Just <| displayName x)
                |> andThen (\x -> Just <| title ++ " " ++ x)
                |> withDefault "No ruler"
                |> text

        rulerLink =
            model.starSystemR.starSystem
                |> andThen (\x -> x.rulerId)
                |> andThen (\rId -> Just (a [ href (PersonR rId) ] [ rulerText ]))
    in
    [ div [ class "row" ]
        [ div [ class "col-lg-4 design-panel-title" ] [ text "Name" ]
        , div [ id "system-name", class "col-lg-8" ] [ name ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-4 design-panel-title" ] [ text "Location" ]
        , div [ class "col-lg-8" ] [ location ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-4 design-panel-title" ] [ text "Ruler" ]
        , div [ class "col-lg-8" ]
            [ case rulerLink of
                Nothing ->
                    rulerText

                Just link ->
                    link
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-4 design-panel-title" ] [ text "Date" ]
        , div [ class "col-lg-8" ] [ date ]
        ]
    ]


starsInfo : StarSystemId -> Model -> List (Html Msg)
starsInfo systemId model =
    div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "Class" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
        :: (model.stars
                |> andThen (Dict.get (unStarSystemId systemId))
                |> withDefault []
                |> List.map
                    (\entry ->
                        div [ class "row" ]
                            [ div [ class "col-lg-4" ] [ text (unStarName entry.name) ]
                            , div [ class "col-lg-4" ] [ text <| stellarClassification entry ]
                            , div [ class "col-lg-4" ] [ text <| starDateToString entry.date ]
                            ]
                    )
           )


starLanesInfo : StarSystemId -> Model -> List (Html Msg)
starLanesInfo _ _ =
    []


planetsInfo : StarSystemId -> Model -> List (Html Msg)
planetsInfo systemId model =
    div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Name" ]
        , div [ class "col-lg-1" ] [ text "Pos." ]
        , div [ class "col-lg-2" ] [ text "Gravity" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
        :: (model.planets
                |> andThen (Dict.get (unStarSystemId systemId))
                |> withDefault []
                |> List.map
                    (\entry ->
                        div [ class "row" ]
                            [ div [ class "col-lg-4" ]
                                [ a [ href (PlanetR entry.id) ] [ text (unPlanetName entry.name) ] ]
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



--TODO: get stars and planets specifically for this star system


init : StarSystemId -> Model -> Cmd Msg
init systemId model =
    Cmd.batch
        [ getStarSystemCmd (StarSystemMessage << StarSystemReceived) systemId
        ]


update : StarSystemRMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SystemDetailsStatusChanged status ->
            ( set (starSystemRA << systemDetailsStatusA) status model, Cmd.none )

        StarListStatusChanged status ->
            ( set (starSystemRA << starListStatusA) status model, Cmd.none )

        StarLaneListStatusChanged status ->
            ( set (starSystemRA << starLanesStatusA) status model, Cmd.none )

        PlanetListStatusChanged status ->
            ( set (starSystemRA << planetsStatusA) status model, Cmd.none )

        StarSystemReceived (Ok system) ->
            ( set (starSystemRA << starSystemA) (Just system) model
            , Cmd.none
            )

        StarSystemReceived (Err err) ->
            ( set (starSystemRA << starSystemA) Nothing model
                |> over errorsA (\errors -> error err "Failed to load star system" :: errors)
            , Cmd.none
            )
