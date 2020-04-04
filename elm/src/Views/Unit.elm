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
        , unDesignName
        , unShipName
        )
import Html
    exposing
        ( Html
        , a
        , div
        , hr
        , span
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Ordering exposing (Ordering)
import RemoteData exposing (RemoteData(..))
import ViewModels.Unit
    exposing
        ( Tab(..)
        , UnitRMsg(..)
        , UnitViewModel
        )
import Views.Helpers exposing (href, infoPanel)


page : Model -> Html Msg
page model =
    div []
        (commonInfo model.unitR
            ++ [ div [ class "row" ]
                    [ div [ class "col-lg-12" ] [ tabs model ]
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
        Just (Ship details) ->
            shipCommonInfo vm details

        Just (Vehicle details) ->
            vehicleCommonInfo vm details

        Nothing ->
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
            Maybe.map (unPlanetName << .name) vm.planet
                |> Maybe.withDefault "unknown planet"

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


tabs : Model -> Html Msg
tabs model =
    let
        tab =
            tabHeader model
    in
    div [ class "space-bottom" ]
        [ tab GeneralInfo
        , tab Crew
        , tab Orders
        , tab DamageControl
        , tab Stats
        , tab Log
        ]


tabHeader : Model -> Tab -> Html Msg
tabHeader model tab =
    let
        attributes =
            if model.unitR.activeTab == tab then
                [ class "btn btn-primary btn-sm command-button" ]

            else
                [ class "btn btn-default btn-sm command-button"
                , onClick <| UnitMessage (TabActivated tab)
                ]
    in
    div attributes [ text <| tabToString tab ]


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


crewTab : Model -> List (Html Msg)
crewTab model =
    let
        crew =
            case model.unitR.unit of
                Just (Ship details) ->
                    details.crew

                Just (Vehicle details) ->
                    details.crew

                Nothing ->
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
                        , th [] [ text "Rank" ]
                        , th [] [ text "Position" ]
                        ]
                    ]
                , case model.unitR.unit of
                    Just unit ->
                        case unit of
                            Ship details ->
                                crewTable model.unitR details.crew

                            Vehicle details ->
                                crewTable model.unitR details.crew

                    Nothing ->
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
        , td [] [ text <| crewRankToString crew.rank ]
        , td [] [ text <| crewPositionToString crew.position ]
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
        UnitDetailsReceived (Ok unit) ->
            ( set (unitRA << unitA) (Just unit) model
            , getLocationDetails unit
            )

        UnitDetailsReceived (Err err) ->
            ( set (unitRA << unitA) Nothing model
                |> over errorsA (\errors -> error err "Failed to load unit details" :: errors)
            , Cmd.none
            )

        PlanetDetailsReceived (Ok planet) ->
            ( set (unitRA << planetA) (Just planet) model
            , Cmd.none
            )

        PlanetDetailsReceived (Err err) ->
            ( set (unitRA << planetA) Nothing model
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
