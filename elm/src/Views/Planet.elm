module Views.Planet exposing
    ( init
    , isLoading
    , page
    , update
    )

{-| Page displaying planetary details. The information shown in based on
all reports gathered by the player's faction and thus might not reflect
the actual situation on the planet, especially if the reports are old
or inaccurate.
-}

import Accessors exposing (get, over, set)
import Api.Construction
    exposing
        ( deleteConstruction
        , getAvailableBuildings
        , getConstructions
        , postBuildingConstruction
        , putConstruction
        )
import Api.StarSystem
    exposing
        ( getBuildings
        , getPlanet
        , getPlanetStatus
        , getPopulations
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
        , planetStatusA
        , planetStatusesStatusA
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
        , unPlanetId
        , unPlanetName
        )
import Data.Construction
    exposing
        ( Building
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
        , unBuildingLevel
        , unConstructionIndex
        )
import Data.Model exposing (Model, Msg(..))
import Data.PersonNames exposing (displayName, unShortTitle)
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
import Dict
import Html exposing (Html, a, div, hr, i, img, input, span, text)
import Html.Attributes exposing (class, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe
import Navigation exposing (parseLocation)
import RemoteData exposing (RemoteData(..), WebData)
import SaveData
import Url exposing (Url)
import ViewModels.Planet exposing (PlanetRMsg(..))
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
page : PlanetId -> Model -> Html Msg
page planetId model =
    div [] <| twinPanels EqualPanels (leftPanel planetId) (rightPanel planetId) model


{-| Left side panel of the page
-}
leftPanel : PlanetId -> Model -> List (Html Msg)
leftPanel _ model =
    infoPanel
        { title = "Details"
        , currentStatus = model.planetR.planetDetailsStatus
        , openingMessage = PlanetMessage <| PlanetDetailsStatusChanged InfoPanelOpen
        , closingMessage = PlanetMessage <| PlanetDetailsStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        (planetDetails model.planetR.planet)
        model
        ++ infoPanel
            { title = "Planet status"
            , currentStatus = model.planetR.planetStatusesStatus
            , openingMessage = PlanetMessage <| PlanetStatusesStatusChanged InfoPanelOpen
            , closingMessage = PlanetMessage <| PlanetStatusesStatusChanged InfoPanelClosed
            , refreshMessage = Nothing
            }
            Nothing
            (statusList model.planetR.planetStatus)
            model
        ++ infoPanel
            { title = "Population"
            , currentStatus = model.planetR.populationStatus
            , openingMessage = PlanetMessage <| PopulationStatusChanged InfoPanelOpen
            , closingMessage = PlanetMessage <| PopulationStatusChanged InfoPanelClosed
            , refreshMessage = Nothing
            }
            Nothing
            (populationDetails model.planetR.populations)
            model
        ++ infoPanel
            { title = "Construction queue"
            , currentStatus = model.planetR.constructionStatus
            , openingMessage = PlanetMessage <| ConstructionStatusChanged InfoPanelOpen
            , closingMessage = PlanetMessage <| ConstructionStatusChanged InfoPanelClosed
            , refreshMessage = Nothing
            }
            Nothing
            (constructionQueue model.planetR.constructions)
            model


{-| Right side panel of the page
-}
rightPanel : PlanetId -> Model -> List (Html Msg)
rightPanel _ model =
    infoPanel
        { title = "Buildings"
        , currentStatus = model.planetR.buildingsStatus
        , openingMessage = PlanetMessage <| BuildingsStatusChanged InfoPanelOpen
        , closingMessage = PlanetMessage <| BuildingsStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        (buildingsList model.planetR.buildings)
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
constructionQueue : WebData (List Construction) -> Model -> List (Html Msg)
constructionQueue constructions model =
    currentQueue constructions model
        ++ (hr [] []
                :: searchField model
           )
        ++ searchResults model


{-| Render list of constructions currently queued
-}
currentQueue : WebData (List Construction) -> Model -> List (Html Msg)
currentQueue constructions _ =
    let
        maxIndex =
            RemoteData.withDefault [] constructions
                |> List.map (unConstructionIndex << constructionIndex)
                |> List.maximum
                |> Maybe.withDefault 0
    in
    div [ class "row design-panel-title" ]
        [ div [ class "col-lg-1" ] []
        , div [ class "col-lg-6" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "Cost left" ]
        ]
        :: (RemoteData.withDefault [] constructions
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
                ++ (span [ class "small-space-left" ] [ text " " ]
                        :: (mechanicalsToText <| Just <| constructionWorkLeft item)
                   )
                ++ (span [ class "small-space-left" ] [ text " " ]
                        :: (chemicalsToText <| Just <| constructionWorkLeft item)
                   )
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
    get availableBuildingsA model.planetR
        |> RemoteData.withDefault []
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
                ++ (span [ class "small-space-left" ] [ text " " ]
                        :: (mechanicalsToText <| Just info.cost)
                   )
                ++ (span [ class "small-space-left" ] [ text " " ]
                        :: (chemicalsToText <| Just info.cost)
                   )
            )
        ]


{-| List of currently landed ship on the planet
-}
landedShips : Maybe (List a) -> Model -> List (Html Msg)
landedShips _ _ =
    [ div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "Class" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
    ]


{-| List of ships currently orbiting the planet
-}
orbitingShips : Maybe (List a) -> Model -> List (Html Msg)
orbitingShips _ _ =
    [ div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "Class" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
    ]


{-| Details of the planet
-}
planetDetails : WebData Planet -> Model -> List (Html Msg)
planetDetails planet _ =
    [ div [ class "row" ]
        [ div [ class "col-lg-2 design-panel-title" ] [ text "Name" ]
        , div [ class "col-lg-4" ]
            [ planet
                |> RemoteData.toMaybe
                |> Maybe.andThen (\x -> Just (unPlanetName x.name))
                |> Maybe.withDefault "-"
                |> text
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-2 design-panel-title" ] [ text "Position" ]
        , div [ class "col-lg-4" ]
            [ planet
                |> RemoteData.toMaybe
                |> Maybe.andThen (\x -> Maybe.andThen (Just << unPlanetPosition) x.position)
                |> Maybe.andThen (Just << String.fromInt)
                |> Maybe.withDefault "-"
                |> text
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-2 design-panel-title" ] [ text "Gravity" ]
        , div [ class "col-lg-4" ]
            [ planet
                |> RemoteData.toMaybe
                |> Maybe.andThen (\x -> Maybe.andThen (Just << gravityToString) x.gravity)
                |> Maybe.withDefault "-"
                |> text
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-2 design-panel-title" ] [ text "Ruler" ]
        , div [ class "col-lg-10" ]
            [ let
                title =
                    planet
                        |> RemoteData.toMaybe
                        |> Maybe.andThen (\x -> x.rulerTitle)
                        |> Maybe.andThen (Just << unShortTitle)
                        |> Maybe.withDefault " "

                rulerText =
                    planet
                        |> RemoteData.toMaybe
                        |> Maybe.andThen (\x -> x.rulerName)
                        |> Maybe.andThen (\x -> Just <| displayName x)
                        |> Maybe.andThen (\x -> Just <| title ++ " " ++ x)
                        |> Maybe.withDefault "No ruler"
                        |> text
              in
              case
                planet
                    |> RemoteData.toMaybe
                    |> Maybe.andThen (\x -> x.rulerId)
              of
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
                |> RemoteData.toMaybe
                |> Maybe.andThen (\x -> Just <| starDateToString x.date)
                |> Maybe.withDefault "-"
                |> text
            ]
        ]
    ]


statusList : WebData PlanetStatus -> Model -> List (Html Msg)
statusList status _ =
    status
        |> RemoteData.toMaybe
        |> Maybe.andThen
            (\x ->
                Just
                    [ div [ class "row" ]
                        [ div [ class "col-lg-12" ] <|
                            List.map planetStatusIcon x.status
                        ]
                    ]
            )
        |> Maybe.withDefault []


planetStatusIcon : PlanetStatusInfo -> Html Msg
planetStatusIcon status =
    div [ class "hover-section" ]
        [ img [ class "planet-status-icon", src status.icon ] []
        , span [ class "hover-text" ]
            [ text status.description ]
        ]


{-| List of populations currently inhabiting the planet
-}
populationDetails : WebData (List Population) -> Model -> List (Html Msg)
populationDetails population _ =
    div [ class "row design-panel-title" ]
        [ div [ class "col-lg-4" ] [ text "Race" ]
        , div [ class "col-lg-4" ] [ text "Inhabitants" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
        :: List.map populationRow (RemoteData.withDefault [] population)


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
buildingsList : WebData (List Building) -> Model -> List (Html Msg)
buildingsList buildings _ =
    div [ class "row design-panel-title" ]
        [ div [ class "col-lg-5" ] [ text "Type" ]
        , div [ class "col-lg-3" ] [ text "Damage" ]
        , div [ class "col-lg-4" ] [ text "Date" ]
        ]
        :: List.map buildingRow (RemoteData.withDefault [] buildings)


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
init : PlanetId -> Model -> Cmd Msg
init pId _ =
    Cmd.batch
        [ getPlanet (PlanetMessage << PlanetDetailsReceived) pId
        , getPopulations (PlanetMessage << PopulationReceived) pId
        , getBuildings (PlanetMessage << BuildingsReceived) pId
        , getConstructions (PlanetMessage << ConstructionsReceived) pId
        , getAvailableBuildings (PlanetMessage << AvailableBuildingsReceived)
        , getPlanetStatus (PlanetMessage << PlanetStatusReceived) pId
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
            ( model
            , putConstruction (PlanetMessage << ConstructionsReceived << SaveData.toWebData) newConstruction
            )

        DeleteConstructionFromQueue construction ->
            ( model
            , deleteConstruction (PlanetMessage << ConstructionsReceived << SaveData.toWebData) construction
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
                    , postBuildingConstruction (PlanetMessage << ConstructionsReceived << SaveData.toWebData) x
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        PlanetStatusesStatusChanged status ->
            ( set (planetRA << planetStatusesStatusA) status model, Cmd.none )

        PlanetDetailsReceived NotAsked ->
            ( model
            , Cmd.none
            )

        PlanetDetailsReceived Loading ->
            ( model
            , Cmd.none
            )

        PlanetDetailsReceived (Success planet) ->
            ( set (planetRA << planetA) (Success planet) model
            , Cmd.none
            )

        PlanetDetailsReceived (Failure err) ->
            ( set (planetRA << planetA) (Failure err) model
                |> over errorsA (\errors -> error err "Failed to load planet details" :: errors)
            , Cmd.none
            )

        PopulationReceived NotAsked ->
            ( model
            , Cmd.none
            )

        PopulationReceived Loading ->
            ( model
            , Cmd.none
            )

        PopulationReceived (Success population) ->
            ( set (planetRA << populationsA) (Success population) model
            , Cmd.none
            )

        PopulationReceived (Failure err) ->
            ( set (planetRA << populationsA) (Failure err) model
                |> over errorsA (\errors -> error err "Failed to load planet population" :: errors)
            , Cmd.none
            )

        BuildingsReceived NotAsked ->
            ( model
            , Cmd.none
            )

        BuildingsReceived Loading ->
            ( model
            , Cmd.none
            )

        BuildingsReceived (Success buildings) ->
            ( set (planetRA << buildingsA) (Success buildings) model
            , Cmd.none
            )

        BuildingsReceived (Failure err) ->
            ( set (planetRA << buildingsA) (Failure err) model
                |> over errorsA (\errors -> error err "Failed to load planet buildings" :: errors)
            , Cmd.none
            )

        AvailableBuildingsReceived NotAsked ->
            ( model
            , Cmd.none
            )

        AvailableBuildingsReceived Loading ->
            ( model
            , Cmd.none
            )

        AvailableBuildingsReceived (Success buildings) ->
            ( set (planetRA << availableBuildingsA) (Success buildings) model
            , Cmd.none
            )

        AvailableBuildingsReceived (Failure err) ->
            ( set (planetRA << availableBuildingsA) (Failure err) model
                |> over errorsA (\errors -> error err "Failed to load available buildings" :: errors)
            , Cmd.none
            )

        ConstructionsReceived NotAsked ->
            ( model
            , Cmd.none
            )

        ConstructionsReceived Loading ->
            ( model
            , Cmd.none
            )

        ConstructionsReceived (Success constructions) ->
            ( set (planetRA << constructionsA) (Success constructions) model
            , Cmd.none
            )

        ConstructionsReceived (Failure err) ->
            ( set (planetRA << constructionsA) (Failure err) model
                |> over errorsA (\errors -> error err "Failed to load constructions" :: errors)
            , Cmd.none
            )

        PlanetStatusReceived NotAsked ->
            ( model
            , Cmd.none
            )

        PlanetStatusReceived Loading ->
            ( model
            , Cmd.none
            )

        PlanetStatusReceived (Success status) ->
            ( set (planetRA << planetStatusA) (Success status) model
            , Cmd.none
            )

        PlanetStatusReceived (Failure err) ->
            ( set (planetRA << planetStatusA) (Failure err) model
                |> over errorsA (\errors -> error err "Failed to load planet status" :: errors)
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
            RemoteData.toMaybe model.planetR.constructions

        newIndex =
            Maybe.withDefault [] constructions
                |> List.map (unConstructionIndex << constructionIndex)
                |> List.maximum
                |> Maybe.withDefault -1
                |> (\x -> x + 1)
                |> ConstructionIndex
    in
    Maybe.andThen
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
        PlanetR planetId ->
            Just planetId

        _ ->
            Nothing


isLoading : Model -> Bool
isLoading model =
    let
        vm =
            model.planetR
    in
    RemoteData.isLoading vm.planet
        || RemoteData.isLoading vm.planetStatus
        || RemoteData.isLoading vm.populations
        || RemoteData.isLoading vm.buildings
        || RemoteData.isLoading vm.availableBuildings
        || RemoteData.isLoading vm.constructions
