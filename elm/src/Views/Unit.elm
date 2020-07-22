module Views.Unit exposing
    ( init
    , isLoading
    , page
    , update
    )

import Accessors exposing (over, set)
import Api.StarSystem exposing (getPlanet, getStarSystem)
import Api.Units exposing (getUnitDetails)
import Data.Accessors
    exposing
        ( activeTabA
        , crewMessagesInfoPanelStatusA
        , crewSpaceInfoPanelStatusA
        , crewTabCurrentPageA
        , crewTabStatusA
        , errorsA
        , planetA
        , starSystemA
        , unitA
        , unitRA
        )
import Data.Common
    exposing
        ( InfoPanelStatus(..)
        , Route(..)
        , UnitId
        , error
        , maxPage
        , unPlanetName
        , unStarSystemName
        )
import Data.Model exposing (Model, Msg(..))
import Data.PersonNames exposing (displayName)
import Data.Vehicles
    exposing
        ( Band(..)
        , CrewReport
        , ShipDetails
        , ShipLocation(..)
        , Unit(..)
        , UnitName(..)
        , VehicleDetails
        , VehicleLocation(..)
        , crewPositionToString
        , crewRankToString
        , positionOrdering
        , rankOrdering
        , unCrewSpace
        , unDesignName
        , unShipName
        , unitStats
        )
import Html
    exposing
        ( Html
        , a
        , div
        , span
        , table
        , tbody
        , td
        , text
        , tfoot
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (class)
import Ordering exposing (Ordering)
import RemoteData exposing (RemoteData(..))
import ViewModels.Unit
    exposing
        ( Tab(..)
        , UnitRMsg(..)
        , UnitViewModel
        )
import Views.Helpers
    exposing
        ( PanelSizing(..)
        , TabConfig
        , TabStatus(..)
        , href
        , infoPanel
        , tabControl
        , twinPanels
        )


tabConfig : Model -> TabConfig Tab
tabConfig model =
    { tabList = [ GeneralInfo, Crew, Orders, DamageControl, Stats, Log ]
    , isActive =
        \t ->
            if model.unitR.activeTab == t then
                ActiveTab

            else
                NonActiveTab
    , activeMsg = UnitMessage << TabActivated
    , tabText = tabToString
    }


page : Model -> Html Msg
page model =
    div []
        (commonInfo model.unitR
            ++ [ div [ class "row" ]
                    [ div [ class "col-lg-12" ] [ tabControl (tabConfig model) ]
                    ]
               , div [ class "row" ]
                    [ div [ class "col-lg-12" ] (tabContent model) ]
               ]
        )


{-| Render common info block for unit
-}
commonInfo : UnitViewModel -> List (Html Msg)
commonInfo vm =
    case vm.unit of
        Success (Ship details) ->
            shipCommonInfo vm details

        Success (Vehicle details) ->
            vehicleCommonInfo vm details

        _ ->
            emptyCommonInfo


{-| Render common info when unit details are not known
-}
emptyCommonInfo : List (Html Msg)
emptyCommonInfo =
    [ div [ class "row" ]
        [ div [ class "col-lg-2 panel-table-heading" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "-" ]
        , div [ class "col-lg-2 panel-table-heading" ] [ text "Class" ]
        , div [ class "col-lg-4" ] [ text "-" ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-2 panel-table-heading" ] [ text "Location" ]
        , div [ class "col-lg-4" ] [ text "-" ]
        ]
    ]


{-| Render common info block for ship
-}
shipCommonInfo : UnitViewModel -> ShipDetails -> List (Html Msg)
shipCommonInfo vm ship =
    [ div [ class "row" ]
        [ div [ class "col-lg-2 panel-table-heading" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text <| unShipName ship.name ]
        , div [ class "col-lg-2 panel-table-heading" ] [ text "Class" ]
        , div [ class "col-lg-4" ] [ text <| unDesignName ship.designName ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-2 panel-table-heading" ] [ text "Location" ]
        , div [ class "col-lg-4" ] [ shipLocation vm ship.location ]
        ]
    ]


shipLocation : UnitViewModel -> Maybe ShipLocation -> Html Msg
shipLocation vm location =
    let
        planetName =
            RemoteData.map (unPlanetName << .name) vm.planet
                |> RemoteData.withDefault "unknown planet"

        systemName =
            RemoteData.map (unStarSystemName << .name) vm.starSystem
                |> RemoteData.withDefault "unknown system"
    in
    case location of
        Just (PlanetarySpace pId band) ->
            case band of
                BandSurface ->
                    span []
                        [ text <| "landed on "
                        , a [ href (PlanetR pId) ]
                            [ text planetName ]
                        ]

                BandOrbit ->
                    span []
                        [ text <| "orbiting "
                        , a [ href (PlanetR pId) ]
                            [ text planetName ]
                        ]

                _ ->
                    span []
                        [ text <| "band "
                        , text <| bandToString band
                        , text " around "
                        , a [ href (PlanetR pId) ]
                            [ text planetName ]
                        ]

        Just (SystemSpace sId band) ->
            case band of
                BandSurface ->
                    text <| "scorchio"

                BandOrbit ->
                    span []
                        [ text <| "orbiting star in "
                        , a [ href (StarSystemR sId) ]
                            [ text systemName ]
                        ]

                _ ->
                    span []
                        [ text <| "band "
                        , text <| bandToString band
                        , text " in "
                        , a [ href (StarSystemR sId) ]
                            [ text systemName ]
                        ]

        Nothing ->
            text "unknown"


bandToString : Band -> String
bandToString b =
    case b of
        BandSurface ->
            "surface"

        BandOrbit ->
            "orbit"

        BandA ->
            "A"

        BandB ->
            "B"

        BandC ->
            "C"

        BandD ->
            "D"

        BandE ->
            "E"

        BandF ->
            "F"

        BandG ->
            "G"


{-| Render common info block for vehicle
-}
vehicleCommonInfo : UnitViewModel -> VehicleDetails -> List (Html Msg)
vehicleCommonInfo _ _ =
    []


tabToString : Tab -> String
tabToString tab =
    case tab of
        GeneralInfo ->
            "General"

        Crew ->
            "Crew"

        Orders ->
            "Orders"

        Log ->
            "Log"

        DamageControl ->
            "Damage control"

        Stats ->
            "Stats"


tabContent : Model -> List (Html Msg)
tabContent model =
    case model.unitR.activeTab of
        GeneralInfo ->
            [ generalTab model ]

        Crew ->
            crewTab model

        Orders ->
            [ ordersTab model ]

        Log ->
            [ logTab model ]

        DamageControl ->
            [ damageControlTab model ]

        Stats ->
            [ statsTab model ]


generalTab : Model -> Html Msg
generalTab _ =
    div [ class "row" ]
        [ div [ class "col-lg-12" ] [ text "general tab" ] ]


{-| Render crew tab
-}
crewTab : Model -> List (Html Msg)
crewTab model =
    crewTabTop model
        ++ currentCrewInfoPanel model


{-| -}
crewTabTop : Model -> List (Html Msg)
crewTabTop =
    twinPanels EqualPanels crewSpaceStatsInfoPanel crewMessagesInfoPanel


crewSpaceStatsInfoPanel : Model -> List (Html Msg)
crewSpaceStatsInfoPanel model =
    infoPanel
        { title = "Crew quarters"
        , currentStatus = model.unitR.crewSpaceInfoPanelStatus
        , openingMessage = UnitMessage <| CrewSpaceInfoPanelStatusChanged InfoPanelOpen
        , closingMessage = UnitMessage <| CrewSpaceInfoPanelStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        crewSpacePanelContent
        model


crewSpacePanelContent : Model -> List (Html Msg)
crewSpacePanelContent model =
    let
        header =
            thead []
                [ tr []
                    [ th [] [ text "Type" ]
                    , th [] [ text "Space" ]
                    ]
                ]
    in
    case RemoteData.map (.crewSpace << unitStats) model.unitR.unit of
        NotAsked ->
            [ table []
                [ header
                , tbody []
                    [ tr []
                        [ td [ class "noData", Html.Attributes.colspan 2 ] [ text "Data has not been requested" ] ]
                    ]
                ]
            ]

        Loading ->
            [ table []
                [ header
                , tbody []
                    [ tr []
                        [ td [ class "noData", Html.Attributes.colspan 2 ] [ text "Loading..." ] ]
                    ]
                ]
            ]

        Success Nothing ->
            [ table []
                [ header
                , tbody []
                    [ tr []
                        [ td [ class "noData", Html.Attributes.colspan 2 ] [ text "Crew space data is unavailable" ] ]
                    ]
                ]
            ]

        Success (Just cSpace) ->
            [ table []
                [ header
                , tbody []
                    [ tr []
                        [ td [] [ text "Steerage" ]
                        , td []
                            [ unCrewSpace cSpace.steerageSpace
                                |> String.fromInt
                                |> text
                            ]
                        ]
                    , tr []
                        [ td [] [ text "Standard" ]
                        , td []
                            [ unCrewSpace cSpace.standardSpace
                                |> String.fromInt
                                |> text
                            ]
                        ]
                    , tr
                        []
                        [ td [] [ text "Luxury" ]
                        , td []
                            [ unCrewSpace cSpace.luxurySpace
                                |> String.fromInt
                                |> text
                            ]
                        ]
                    ]
                , tfoot []
                    [ tr
                        []
                        [ th [] [ text "Total" ]
                        , td [] [ text "79" ]
                        ]
                    , tr
                        []
                        [ th [] [ text "Used" ]
                        , td [] [ text "50" ]
                        ]
                    , tr
                        []
                        [ th [] [ text "Free" ]
                        , td [] [ text "29" ]
                        ]
                    ]
                ]
            ]

        Failure _ ->
            [ table []
                [ header
                , tbody []
                    [ tr []
                        [ td [ class "noData", Html.Attributes.colspan 2 ] [ text "Failure to retrieve data" ] ]
                    ]
                ]
            ]


crewMessagesInfoPanel : Model -> List (Html Msg)
crewMessagesInfoPanel model =
    infoPanel
        { title = "Messages"
        , currentStatus = model.unitR.crewMessagesInfoPanelStatus
        , openingMessage = UnitMessage <| CrewMessagesInfoPanelStatusChanged InfoPanelOpen
        , closingMessage = UnitMessage <| CrewMessagesInfoPanelStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        crewMessagesPanelContent
        model


crewMessagesPanelContent : Model -> List (Html Msg)
crewMessagesPanelContent _ =
    [ text "Here are messages about current crew situation" ]


{-| Render info panel for showing current crew of the ship
-}
currentCrewInfoPanel : Model -> List (Html Msg)
currentCrewInfoPanel model =
    let
        crew =
            case model.unitR.unit of
                Success (Ship details) ->
                    details.crew

                Success (Vehicle details) ->
                    details.crew

                _ ->
                    []
    in
    infoPanel
        { title = "Crew"
        , currentStatus = model.unitR.crewTabStatus
        , openingMessage = UnitMessage <| CrewTabStatusChanged InfoPanelOpen
        , closingMessage = UnitMessage <| CrewTabStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        (Just
            { pageSize = model.unitR.crewTabPageSize
            , currentPage = model.unitR.crewTabCurrentPage
            , maxPage = maxPage model.unitR.crewTabPageSize crew
            , pageChangedMessage = UnitMessage << CrewPageChanged
            }
        )
        crewTabContent
        model


crewTabContent : Model -> List (Html Msg)
crewTabContent model =
    [ div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ table []
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Position" ]
                        , th [] [ text "Rank" ]
                        ]
                    ]
                , case model.unitR.unit of
                    Success unit ->
                        case unit of
                            Ship details ->
                                crewTable model.unitR details.crew

                            Vehicle details ->
                                crewTable model.unitR details.crew

                    _ ->
                        tbody []
                            [ tr []
                                [ td [ class "noData", Html.Attributes.colspan 3 ] [ text "Nothing to show" ] ]
                            ]
                ]
            ]
        ]
    ]


crewTable : UnitViewModel -> List CrewReport -> Html Msg
crewTable vm crew =
    List.sortWith crewOrdering crew
        |> List.drop (vm.crewTabCurrentPage * vm.crewTabPageSize)
        |> List.take vm.crewTabPageSize
        |> List.map crewRow
        |> tbody []


crewRow : CrewReport -> Html Msg
crewRow crew =
    tr []
        [ td [] [ a [ href (PersonR crew.personId) ] [ text <| displayName crew.name ] ]
        , td [] [ text <| crewPositionToString crew.position ]
        , td [] [ text <| crewRankToString crew.rank ]
        ]


crewOrdering : Ordering CrewReport
crewOrdering a b =
    positionOrdering a.position b.position
        |> Ordering.ifStillTiedThen (rankOrdering a.rank b.rank)


ordersTab : Model -> Html Msg
ordersTab _ =
    div [ class "row" ]
        [ div [ class "col-lg-12" ] [ text "orders tab" ] ]


logTab : Model -> Html Msg
logTab _ =
    div [ class "row" ]
        [ div [ class "col-lg-12" ] [ text "log tab" ] ]


damageControlTab : Model -> Html Msg
damageControlTab _ =
    div [ class "row" ]
        [ div [ class "col-lg-12" ] [ text "damage control tab" ] ]


statsTab : Model -> Html Msg
statsTab _ =
    div [ class "row" ]
        [ div [ class "col-lg-12" ] [ text "stats tab" ] ]


{-| Handle incoming messages
-}
update : UnitRMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UnitDetailsReceived NotAsked ->
            ( model
            , Cmd.none
            )

        UnitDetailsReceived Loading ->
            ( model
            , Cmd.none
            )

        UnitDetailsReceived (Success unit) ->
            ( set (unitRA << unitA) (Success unit) model
            , getLocationDetails unit
            )

        UnitDetailsReceived (Failure err) ->
            ( set (unitRA << unitA) (Failure err) model
                |> over errorsA (\errors -> error err "Failed to load unit details" :: errors)
            , Cmd.none
            )

        PlanetDetailsReceived NotAsked ->
            ( model
            , Cmd.none
            )

        PlanetDetailsReceived Loading ->
            ( model
            , Cmd.none
            )

        PlanetDetailsReceived (Success planet) ->
            ( set (unitRA << planetA) (Success planet) model
            , Cmd.none
            )

        PlanetDetailsReceived (Failure err) ->
            ( set (unitRA << planetA) (Failure err) model
                |> over errorsA (\errors -> error err "Failed to load planet details" :: errors)
            , Cmd.none
            )

        StarSystemDetailsReceived NotAsked ->
            ( model
            , Cmd.none
            )

        StarSystemDetailsReceived Loading ->
            ( model
            , Cmd.none
            )

        StarSystemDetailsReceived (Success starSystem) ->
            ( set (unitRA << starSystemA) (Success starSystem) model
            , Cmd.none
            )

        StarSystemDetailsReceived (Failure err) ->
            ( set (unitRA << starSystemA) (Failure err) model
                |> over errorsA (\errors -> error err "Failed to load star system details" :: errors)
            , Cmd.none
            )

        TabActivated tab ->
            ( set (unitRA << activeTabA) tab model
            , Cmd.none
            )

        CrewTabStatusChanged status ->
            ( set (unitRA << crewTabStatusA) status model
            , Cmd.none
            )

        CrewPageChanged n ->
            ( set (unitRA << crewTabCurrentPageA) n model
            , Cmd.none
            )

        CrewSpaceInfoPanelStatusChanged status ->
            ( set (unitRA << crewSpaceInfoPanelStatusA) status model
            , Cmd.none
            )

        CrewMessagesInfoPanelStatusChanged status ->
            ( set (unitRA << crewMessagesInfoPanelStatusA) status model
            , Cmd.none
            )


{-| Load location of current unit
-}
getLocationDetails : Unit -> Cmd Msg
getLocationDetails unit =
    case unit of
        Ship details ->
            case details.location of
                Just (PlanetarySpace pId _) ->
                    getPlanet (UnitMessage << PlanetDetailsReceived) pId

                Just (SystemSpace sId _) ->
                    getStarSystem (UnitMessage << StarSystemDetailsReceived) sId

                Nothing ->
                    Cmd.none

        Vehicle details ->
            case details.location of
                Just (VehicleOnPlanet pId) ->
                    getPlanet (UnitMessage << PlanetDetailsReceived) pId

                Nothing ->
                    Cmd.none


{-| Initialize view model
-}
init : UnitId -> Model -> Cmd Msg
init uId _ =
    Cmd.batch
        [ getUnitDetails (UnitMessage << UnitDetailsReceived) uId
        ]


isLoading : Model -> Bool
isLoading model =
    let
        vm =
            model.unitR
    in
    RemoteData.isLoading vm.starSystem
        || RemoteData.isLoading vm.planet
        || RemoteData.isLoading vm.unit
