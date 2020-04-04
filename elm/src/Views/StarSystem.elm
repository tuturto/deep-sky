module Views.StarSystem exposing
    ( init
    , isLoading
    , page
    , update
    )

import Accessors exposing (get, over, set)
import Api.StarSystem
    exposing
        ( getPlanetsCmd
        , getStarSystem
        , getStars
        )
import Data.Accessors
    exposing
        ( errorsA
        , planetsStatusA
        , starLanesStatusA
        , starListStatusA
        , starSystemA
        , starSystemRA
        , starsA
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
import Maybe
import RemoteData exposing (RemoteData(..))
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
            starsInfo
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
                |> RemoteData.andThen (\x -> Success (unStarSystemName x.name))
                |> RemoteData.withDefault ""
                |> text

        location =
            model.starSystemR.starSystem
                |> RemoteData.andThen (\x -> Success <| locationToString x.location)
                |> RemoteData.withDefault ""
                |> text

        date =
            model.starSystemR.starSystem
                |> RemoteData.andThen (\x -> Success <| starDateToString x.date)
                |> RemoteData.withDefault ""
                |> text

        title =
            model.starSystemR.starSystem
                |> RemoteData.andThen (\x -> Success x.rulerTitle)
                |> RemoteData.andThen (\x -> Success <| Maybe.map unShortTitle x)
                |> RemoteData.withDefault Nothing
                |> Maybe.withDefault " "

        rulerText =
            model.starSystemR.starSystem
                |> RemoteData.andThen (\x -> Success x.rulerName)
                |> RemoteData.andThen (\x -> Success <| Maybe.map displayName x)
                |> RemoteData.andThen (\x -> Success <| Maybe.map (\y -> title ++ " " ++ y) x)
                |> RemoteData.withDefault Nothing
                |> Maybe.withDefault "No ruler"
                |> text

        rulerLink =
            model.starSystemR.starSystem
                |> RemoteData.andThen (\x -> Success x.rulerId)
                |> RemoteData.andThen
                    (\x ->
                        Success
                            (Maybe.map
                                (\rId ->
                                    a [ href (PersonR rId) ] [ rulerText ]
                                )
                                x
                            )
                    )
                |> RemoteData.withDefault Nothing
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


starsInfo : Model -> List (Html Msg)
starsInfo model =
    div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "Class" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
        :: (model.starSystemR.stars
                |> RemoteData.toMaybe
                |> Maybe.withDefault []
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
                |> Maybe.andThen (Dict.get (unStarSystemId systemId))
                |> Maybe.withDefault []
                |> List.map
                    (\entry ->
                        div [ class "row" ]
                            [ div [ class "col-lg-4" ]
                                [ a [ href (PlanetR entry.id) ] [ text (unPlanetName entry.name) ] ]
                            , div [ class "col-lg-1" ]
                                [ entry.position
                                    |> Maybe.andThen (\(PlanetPosition x) -> Just <| String.fromInt x)
                                    |> Maybe.withDefault ""
                                    |> text
                                ]
                            , div [ class "col-lg-2" ]
                                [ entry.gravity
                                    |> Maybe.andThen (\gravity -> Just <| gravityToString gravity)
                                    |> Maybe.withDefault ""
                                    |> text
                                ]
                            , div [ class "col-lg-4" ] [ text <| starDateToString entry.date ]
                            ]
                    )
           )



--TODO: get stars and planets specifically for this star system


init : StarSystemId -> Model -> Cmd Msg
init systemId _ =
    Cmd.batch
        [ getStarSystem (StarSystemMessage << StarSystemReceived) systemId
        , getStars (StarSystemMessage << StarsReceived) (Just systemId)
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

        StarSystemReceived NotAsked ->
            ( model
            , Cmd.none
            )

        StarSystemReceived Loading ->
            ( model
            , Cmd.none
            )

        StarSystemReceived (Success system) ->
            ( set (starSystemRA << starSystemA) (Success system) model
            , Cmd.none
            )

        StarSystemReceived (Failure err) ->
            ( set (starSystemRA << starSystemA) (Failure err) model
                |> over errorsA (\errors -> error err "Failed to load star system" :: errors)
            , Cmd.none
            )

        StarsReceived NotAsked ->
            ( model
            , Cmd.none
            )

        StarsReceived Loading ->
            ( model
            , Cmd.none
            )

        StarsReceived (Success stars) ->
            ( set (starSystemRA << starsA) (Success stars) model
            , Cmd.none
            )

        StarsReceived (Failure err) ->
            ( over errorsA (\errors -> error err "Failed to load stars" :: errors) model
                |> set (starSystemRA << starsA) (Failure err)
            , Cmd.none
            )


isLoading : Model -> Bool
isLoading model =
    let
        vm =
            model.starSystemR
    in
    RemoteData.isLoading vm.stars
        || RemoteData.isLoading vm.starSystem
