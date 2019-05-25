module Views.Planet exposing (init, page, update)

{-| Page displaying planetary details. The information shown in based on
all reports gathered by the player's faction and thus might not reflect
the actual situation on the planet, especially if the reports are old
or inaccurate.
-}

import Accessors exposing (get, over, set)
import Api.Construction
    exposing
        ( deleteConstructionCmd
        , getAvailableBuildingsCmd
        , getConstructionsCmd
        , postBuildingConstructionCmd
        , putConstructionCmd
        )
import Api.StarSystem
    exposing
        ( getBuildingsCmd
        , getPlanetCmd
        , getPlanetsCmd
        , getPopulationsCmd
        , getStarSystemsCmd
        , planetStatus
        )
import Data.Accessors
    exposing
        ( availableBuildingsA
        , buildingSearchTextA
        , buildingsA
        , buildingsStatusA
        , constructionStatusA
        , constructionsA
        , errorsA
        , indexA
        , landedShipsStatusA
        , orbitingShipsStatusA
        , planetA
        , planetDetailsStatusA
        , planetRA
        , planetStatusesStatusA
        , planetsA
        , populationStatusA
        , populationsA
        )
import Data.Common
    exposing
        ( BuildingId(..)
        , ConstructionId(..)
        , InfoPanelStatus(..)
        , PlanetId(..)
        , Route(..)
        , StarSystemId(..)
        , error
        , unBuildingId
        , unPlanetId
        , unStarSystemId
        )
import Data.Construction
    exposing
        ( Building
        , BuildingConstructionData
        , BuildingDamage(..)
        , BuildingInfo
        , BuildingLevel(..)
        , Construction(..)
        , ConstructionIndex(..)
        , buildingDamageToString
        , buildingTypeToString
        , constructionIndex
        , constructionName
        , constructionWorkLeft
        , unBuildingDamage
        , unBuildingLevel
        , unConstructionIndex
        )
import Data.Model exposing (Model, Msg(..))
import Data.People exposing (displayName)
import Data.StarSystem
    exposing
        ( Planet
        , PlanetStatus
        , PlanetStatusInfo
        , Population
        , gravityToString
        , unInhabitants
        , unPlanetPosition
        , unRace
        )
import Data.User exposing (Role(..))
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (andThen, withDefault)
import Navigation exposing (parseLocation)
import Url exposing (Url)
import ViewModels.Planet exposing (PlanetRMsg(..), PlanetViewModel)
import Views.Helpers
    exposing
        ( PanelSizing(..)
        , biologicalsToText
        , chemicalsToText
        , href
        , infoPanel
        , mechanicalsToText
        , starDateToString
        , twinPanels
        )


{-| Render page of displaying given planet
-}
page : StarSystemId -> PlanetId -> Model -> Html Msg
page systemId planetId model =
    div [] <| twinPanels EqualPanels (leftPanel systemId planetId) (rightPanel systemId planetId) model


{-| Left side panel of the page
-}
leftPanel : StarSystemId -> PlanetId -> Model -> List (Html Msg)
leftPanel systemId planetId model =
    let
        planet =
            model.planetR.planet

        population =
            get populationsA model
                |> andThen (Dict.get (unPlanetId planetId))

        constructions =
            get constructionsA model
                |> andThen (Dict.get (unPlanetId planetId))
    in
    infoPanel
        { title = "Details"
        , currentStatus = model.planetR.planetDetailsStatus
        , openingMessage = PlanetMessage <| PlanetDetailsStatusChanged InfoPanelOpen
        , closingMessage = PlanetMessage <| PlanetDetailsStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        (planetDetails planet)
        model
        ++ infoPanel
            { title = "Planet status"
            , currentStatus = model.planetR.planetStatusesStatus
            , openingMessage = PlanetMessage <| PlanetStatusesStatusChanged InfoPanelOpen
            , closingMessage = PlanetMessage <| PlanetStatusesStatusChanged InfoPanelClosed
            , refreshMessage = Nothing
            }
            Nothing
            (statusList model.planetStatus)
            model
        ++ infoPanel
            { title = "Population"
            , currentStatus = model.planetR.populationStatus
            , openingMessage = PlanetMessage <| PopulationStatusChanged InfoPanelOpen
            , closingMessage = PlanetMessage <| PopulationStatusChanged InfoPanelClosed
            , refreshMessage = Nothing
            }
            Nothing
            (populationDetails population)
            model
        ++ infoPanel
            { title = "Construction queue"
            , currentStatus = model.planetR.constructionStatus
            , openingMessage = PlanetMessage <| ConstructionStatusChanged InfoPanelOpen
            , closingMessage = PlanetMessage <| ConstructionStatusChanged InfoPanelClosed
            , refreshMessage = Nothing
            }
            Nothing
            (constructionQueue constructions)
            model


{-| Right side panel of the page
-}
rightPanel : StarSystemId -> PlanetId -> Model -> List (Html Msg)
rightPanel systemId planetId model =
    let
        buildings =
            get buildingsA model
                |> andThen (Dict.get (unPlanetId planetId))
    in
    infoPanel
        { title = "Buildings"
        , currentStatus = model.planetR.buildingsStatus
        , openingMessage = PlanetMessage <| BuildingsStatusChanged InfoPanelOpen
        , closingMessage = PlanetMessage <| BuildingsStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        (buildingsList buildings)
        model
        ++ infoPanel
            { title = "Landed ships"
            , currentStatus = model.planetR.landedShipsStatus
            , openingMessage = PlanetMessage <| LandedShipsStatusChanged InfoPanelOpen
            , closingMessage = PlanetMessage <| LandedShipsStatusChanged InfoPanelClosed
            , refreshMessage = Nothing
            }
            Nothing
            (landedShips Nothing)
            model
        ++ infoPanel
            { title = "Orbiting ships"
            , currentStatus = model.planetR.orbitingShipsStatus
            , openingMessage = PlanetMessage <| OrbitingShipsStatusChanged InfoPanelOpen
            , closingMessage = PlanetMessage <| OrbitingShipsStatusChanged InfoPanelClosed
            , refreshMessage = Nothing
            }
            Nothing
            (landedShips Nothing)
            model


{-| Construction queue senction, containing all currently queued constructions
and search portion where user can queue up more constructions
-}
constructionQueue : Maybe (List Construction) -> Model -> List (Html Msg)
constructionQueue constructions model =
    currentQueue constructions model
        ++ [ hr [] [] ]
        ++ searchField model
        ++ searchResults model


{-| Render list of constructions currently queued
-}
currentQueue : Maybe (List Construction) -> Model -> List (Html Msg)
currentQueue constructions model =
    let
        maxIndex =
            withDefault [] constructions
                |> List.map (unConstructionIndex << constructionIndex)
                |> List.maximum
                |> withDefault 0
    in
    [ div [ class "row design-panel-title" ]
        [ div [ class "col-lg-1" ] []
        , div [ class "col-lg-6" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "Cost left" ]
        ]
    ]
        ++ (withDefault [] constructions
                |> List.sortBy (unConstructionIndex << constructionIndex)
                |> List.map (queueItem maxIndex)
           )


{-| Render single item in construction queue
-}
queueItem : Int -> Construction -> Html Msg
queueItem maxIndex item =
    let
        cIndex =
            unConstructionIndex <| constructionIndex item

        up =
            if cIndex > 0 then
                i [ class "fas fa-angle-up", onClick (PlanetMessage <| MoveConstruction item (ConstructionIndex <| cIndex - 1)) ] []

            else
                i [ class "fas fa-angle-up" ] []

        down =
            if cIndex < maxIndex then
                i [ class "fas fa-angle-down", onClick (PlanetMessage <| MoveConstruction item (ConstructionIndex <| cIndex + 1)) ] []

            else
                i [ class "fas fa-angle-down" ] []
    in
    div [ class "row" ]
        [ div [ class "col-lg-1" ] [ up, down ]
        , div [ class "col-lg-6" ] [ text <| constructionName item ]
        , div [ class "col-lg-4" ]
            ((biologicalsToText <| Just <| constructionWorkLeft item)
                ++ [ span [ class "small-space-left" ] [ text " " ] ]
                ++ (mechanicalsToText <| Just <| constructionWorkLeft item)
                ++ [ span [ class "small-space-left" ] [ text " " ] ]
                ++ (chemicalsToText <| Just <| constructionWorkLeft item)
            )
        , div [ class "col-lg-1" ]
            [ i [ class "fas fa-trash-alt", onClick (PlanetMessage <| DeleteConstructionFromQueue item) ] [] ]
        ]


{-| Search field for construction queue portion
-}
searchField : Model -> List (Html Msg)
searchField model =
    [ div [ class "row" ]
        [ div [ class "col-lg-6" ]
            [ input
                [ type_ "text"
                , placeholder "Search"
                , value (get (planetRA << buildingSearchTextA) model)
                , onInput (PlanetMessage << BuildingSearch)
                , style "width" "100%"
                ]
                []
            ]
        , i [ class "fas fa-times-circle", onClick (PlanetMessage ClearBuildingSearch) ] []
        ]
    ]


{-| Search results for construction queue
-}
searchResults : Model -> List (Html Msg)
searchResults model =
    get availableBuildingsA model
        |> withDefault []
        |> List.sortWith (\a b -> compare a.name b.name)
        |> List.filter
            (\x ->
                String.isEmpty (get (planetRA << buildingSearchTextA) model)
                    || String.contains (String.toLower (get (planetRA << buildingSearchTextA) model))
                        (String.toLower x.name)
            )
        |> List.map searchResult


{-| render single search result (building info)
-}
searchResult : BuildingInfo -> Html Msg
searchResult info =
    div [ class "row" ]
        [ div [ class "col-lg-7", onClick (PlanetMessage <| QueueConstruction info) ]
            [ text
                (info.name
                    ++ " ("
                    ++ (String.fromInt <| unBuildingLevel info.level)
                    ++ ")"
                )
            ]
        , div [ class "col-lg-5" ]
            ((biologicalsToText <| Just info.cost)
                ++ [ span [ class "small-space-left" ] [ text " " ] ]
                ++ (mechanicalsToText <| Just info.cost)
                ++ [ span [ class "small-space-left" ] [ text " " ] ]
                ++ (chemicalsToText <| Just info.cost)
            )
        ]


{-| List of currently landed ship on the planet
-}
landedShips : Maybe (List a) -> Model -> List (Html Msg)
landedShips ships model =
    [ div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "Class" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
    ]


{-| List of ships currently orbiting the planet
-}
orbitingShips : Maybe (List a) -> Model -> List (Html Msg)
orbitingShips ships model =
    [ div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "Class" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
    ]


{-| Details of the planet
-}
planetDetails : Maybe Planet -> Model -> List (Html Msg)
planetDetails planet model =
    [ div [ class "row" ]
        [ div [ class "col-lg-2 design-panel-title" ] [ text "Name" ]
        , div [ class "col-lg-4" ]
            [ planet
                |> andThen (\x -> Just x.name)
                |> withDefault "-"
                |> text
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-2 design-panel-title" ] [ text "Position" ]
        , div [ class "col-lg-4" ]
            [ planet
                |> andThen (\x -> andThen (Just << unPlanetPosition) x.position)
                |> andThen (Just << String.fromInt)
                |> withDefault "-"
                |> text
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-2 design-panel-title" ] [ text "Gravity" ]
        , div [ class "col-lg-4" ]
            [ planet
                |> andThen (\x -> andThen (Just << gravityToString) x.gravity)
                |> withDefault "-"
                |> text
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-2 design-panel-title" ] [ text "Ruler" ]
        , div [ class "col-lg-4" ]
            [ let
                rulerText =
                    planet
                        |> andThen (\x -> x.rulerName)
                        |> andThen (\x -> Just <| displayName x)
                        |> withDefault "No ruler"
                        |> text
              in
              case planet |> andThen (\x -> x.rulerId) of
                Nothing ->
                    rulerText

                Just rId ->
                    a [ href (PersonR rId) ] [ rulerText ]
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-2 design-panel-title" ] [ text "Date" ]
        , div [ class "col-lg-4" ]
            [ planet
                |> andThen (\x -> Just <| starDateToString x.date)
                |> withDefault "-"
                |> text
            ]
        ]
    ]


statusList : Maybe PlanetStatus -> Model -> List (Html Msg)
statusList status model =
    case status of
        Nothing ->
            []

        Just x ->
            [ div [ class "row" ]
                [ div [ class "col-lg-12" ] <|
                    List.map planetStatusIcon x.status
                ]
            ]


planetStatusIcon : PlanetStatusInfo -> Html Msg
planetStatusIcon status =
    div [ class "hover-section" ]
        [ img [ class "planet-status-icon", src status.icon ] []
        , span [ class "hover-text" ]
            [ text status.description ]
        ]


{-| List of populations currently inhabiting the planet
-}
populationDetails : Maybe (List Population) -> Model -> List (Html Msg)
populationDetails population model =
    [ div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Race" ]
        , div [ class "col-lg-4" ] [ text "Inhabitants" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
    ]
        ++ List.map populationRow (withDefault [] population)


{-| Single row in population list
-}
populationRow : Population -> Html Msg
populationRow population =
    div [ class "row" ]
        [ div [ class "col-lg-4" ] [ text <| unRace population.race ]
        , div [ class "col-lg-4" ] [ text <| String.fromInt <| unInhabitants population.inhabitants ]
        , div [ class "col-lg-4" ] [ text <| starDateToString population.date ]
        ]


{-| List of buildings currently on the planet
-}
buildingsList : Maybe (List Building) -> Model -> List (Html Msg)
buildingsList buildings model =
    [ div [ class "row design-panel-title" ]
        [ div [ class "col-lg-5" ] [ text "Type" ]
        , div [ class "col-lg-3" ] [ text "Damage" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
    ]
        ++ List.map buildingRow (withDefault [] buildings)


{-| Single building on the building list
-}
buildingRow : Building -> Html Msg
buildingRow building =
    let
        buildingName =
            buildingTypeToString building.buildingType
                ++ " ("
                ++ (String.fromInt <| unBuildingLevel building.level)
                ++ ")"
    in
    div [ class "row" ]
        [ div [ class "col-lg-5" ] [ text <| buildingName ]
        , div [ class "col-lg-3" ] [ text <| buildingDamageToString building.damage ]
        , div [ class "col-lg-4" ] [ text <| starDateToString building.date ]
        ]


{-| Initiate retrieval of data needed by this page
-}
init : StarSystemId -> PlanetId -> Model -> Cmd Msg
init sId pId model =
    Cmd.batch
        [ getPlanetCmd (PlanetMessage << PlanetDetailsReceived) pId
        , getPopulationsCmd model pId
        , getBuildingsCmd model pId
        , getConstructionsCmd model pId
        , getStarSystemsCmd model
        , getAvailableBuildingsCmd model
        , planetStatus model pId
        ]


{-| Handle messages specific to this page
-}
update : PlanetRMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlanetDetailsStatusChanged status ->
            ( set (planetRA << planetDetailsStatusA) status model, Cmd.none )

        PopulationStatusChanged status ->
            ( set (planetRA << populationStatusA) status model, Cmd.none )

        BuildingsStatusChanged status ->
            ( set (planetRA << buildingsStatusA) status model, Cmd.none )

        LandedShipsStatusChanged status ->
            ( set (planetRA << landedShipsStatusA) status model, Cmd.none )

        OrbitingShipsStatusChanged status ->
            ( set (planetRA << orbitingShipsStatusA) status model, Cmd.none )

        ConstructionStatusChanged status ->
            ( set (planetRA << constructionStatusA) status model, Cmd.none )

        MoveConstruction construction index ->
            let
                newConstruction =
                    case construction of
                        BuildingConstruction data ->
                            BuildingConstruction <| set indexA index data

                        ShipConstruction data ->
                            ShipConstruction <| set indexA index data
            in
            ( model, putConstructionCmd newConstruction )

        DeleteConstructionFromQueue construction ->
            ( model
            , deleteConstructionCmd construction
            )

        BuildingSearch name ->
            ( set (planetRA << buildingSearchTextA) name model
            , Cmd.none
            )

        ClearBuildingSearch ->
            ( set (planetRA << buildingSearchTextA) "" model
            , Cmd.none
            )

        QueueConstruction building ->
            let
                construction =
                    mapInfoToConstruction model building
            in
            case construction of
                Just x ->
                    ( model
                    , postBuildingConstructionCmd x
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        PlanetStatusesStatusChanged status ->
            ( set (planetRA << planetStatusesStatusA) status model, Cmd.none )

        PlanetDetailsReceived (Ok planet) ->
            ( set (planetRA << planetA) (Just planet) model
            , Cmd.none
            )

        PlanetDetailsReceived (Err err) ->
            ( set (planetRA << planetA) Nothing model
                |> over errorsA (\errors -> error err "Failed to load planet details" :: errors)
            , Cmd.none
            )


{-| Maps building info to construction
Model is used to deduct which planet is currently being viewed based on the url.
In case this deduction fails or no planet is being viewed, function returns Nothing.
-}
mapInfoToConstruction : Model -> BuildingInfo -> Maybe Construction
mapInfoToConstruction model info =
    let
        planetId =
            currentPlanet model.url

        constructions =
            andThen
                (\x ->
                    get constructionsA model
                        |> andThen (Dict.get (unPlanetId x))
                )
                planetId

        newIndex =
            withDefault [] constructions
                |> List.map (unConstructionIndex << constructionIndex)
                |> List.maximum
                |> withDefault -1
                |> (\x -> x + 1)
                |> ConstructionIndex
    in
    andThen
        (\x ->
            Just <|
                BuildingConstruction
                    { id = ConstructionId 0
                    , name = info.name
                    , index = newIndex
                    , level = info.level
                    , buildingType = info.infoType
                    , planet = x
                    , workLeft = info.cost
                    }
        )
        planetId


{-| Deduct which planet is being viewed based on url
-}
currentPlanet : Url -> Maybe PlanetId
currentPlanet url =
    let
        location =
            parseLocation url
    in
    case location of
        PlanetR _ planetId ->
            Just planetId

        _ ->
            Nothing
