module Views.Layout exposing (view)

import Browser
import Data.Common
    exposing
        ( ErrorMessage(..)
        , Route(..)
        , unPlanetName
        , unStarSystemName
        )
import Data.Model exposing (Model, Msg(..))
import Data.PersonNames exposing (displayName)
import Data.User exposing (Role(..))
import Data.Vehicles exposing (Unit(..), unShipName, unVehicleName)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , div
        , i
        , li
        , nav
        , text
        , ul
        )
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Navigation exposing (parseLocation)
import RemoteData exposing (RemoteData(..))
import Url exposing (Url)
import Views.Admin.Main
import Views.Admin.People.Add
import Views.Admin.People.Edit
import Views.Admin.People.List
import Views.Bases
import Views.Construction
import Views.Designer
import Views.Fleet
import Views.Helpers
    exposing
        ( biologicalsToText
        , chemicalsToText
        , href
        , mechanicalsToText
        , starDateToText
        )
import Views.Home
import Views.Messages
import Views.Person
import Views.Planet
import Views.Profile
import Views.Research
import Views.StarSystem
import Views.StarSystems
import Views.Unit


menuBar : Model -> List Role -> Html Msg
menuBar model _ =
    let
        currentLocation =
            parseLocation model.url

        mClass =
            menuClass currentLocation

        menuButton route msg =
            li [ mClass route ] [ a [ href route ] [ text msg ] ]
    in
    nav [ class "navbar navbar-default navbar-static-top" ]
        [ div [ class "container" ]
            [ div [ class "collapse navbar-collapse" ]
                [ ul [ class "nav navbar-nav" ]
                    [ menuButton HomeR "Home"
                    , menuButton ProfileR "Profile"
                    , menuButton StarSystemsR "Star Systems"
                    , menuButton BasesR "Bases"
                    , menuButton FleetR "Fleet"
                    , menuButton DesignerR "Designer"
                    , menuButton ResearchR "Research"
                    , menuButton ConstructionR "Construction"
                    , menuButton MessagesR "Messages"
                    ]
                , ul [ class "nav navbar-nav navbar-right" ]
                    [ menuButton AdminR "Admin"
                    , menuButton LogoutR "Logout"
                    ]
                ]
            ]
        ]


infoBar : Model -> Html Msg
infoBar model =
    div [ class "container" ]
        [ ul [ class "infoitem" ]
            ([ li []
                [ i [ class "fas fa-clock" ] []
                , starDateToText <| RemoteData.toMaybe model.currentTime
                ]
             , li [] <|
                biologicalsToText <|
                    RemoteData.toMaybe model.resources
             , li [] <|
                mechanicalsToText <|
                    RemoteData.toMaybe model.resources
             , li [] <|
                chemicalsToText <|
                    RemoteData.toMaybe model.resources
             ]
                ++ (if isLoading model then
                        [ li [ id "loading-indicator", class "pull-right" ] [ text "loading..." ] ]

                    else
                        []
                   )
            )
        ]


isLoading : Model -> Bool
isLoading model =
    isModuleLoading model
        || RemoteData.isLoading model.resources
        || RemoteData.isLoading model.currentTime


isModuleLoading : Model -> Bool
isModuleLoading model =
    case parseLocation model.url of
        AdminR ->
            Views.Admin.Main.isLoading model

        AdminListPeopleR ->
            Views.Admin.People.List.isLoading model

        AdminPersonR _ ->
            Views.Admin.People.Edit.isLoading model

        AdminNewPersonR ->
            Views.Admin.People.Add.isLoading model

        BasesR ->
            False

        ConstructionR ->
            False

        DesignerR ->
            Views.Designer.isLoading model

        FleetR ->
            False

        HomeR ->
            False

        MessagesR ->
            False

        ProfileR ->
            False

        ResearchR ->
            False

        StarSystemR _ ->
            Views.StarSystem.isLoading model

        StarSystemsR ->
            Views.StarSystems.isLoading model

        PlanetR _ ->
            False

        PersonR _ ->
            False

        UnitR _ ->
            Views.Unit.isLoading model

        LogoutR ->
            False


errorBar : Model -> Html Msg
errorBar model =
    div [ class "container", onClick ClearErrors ] <|
        List.map errorRow model.errors


errorRow : ErrorMessage -> Html Msg
errorRow (ErrorMessage error) =
    div [ class "row error-bar" ]
        [ div [ class "col-lg-12" ] [ text error ] ]


menuClass : Route -> Route -> Attribute Msg
menuClass current checked =
    if similarRoutes current checked then
        class "active"

    else
        class ""


{-| Compare routes and deduce if they should be considired similar for purposes
of active menu item selection. First route is currently active route
(ie. the route that browser is currently displaying). Second route should be
one of the top level routes (ie. ones that are displayed at the top menu bar).
-}
similarRoutes : Route -> Route -> Bool
similarRoutes current checked =
    case checked of
        StarSystemsR ->
            case current of
                StarSystemsR ->
                    True

                StarSystemR _ ->
                    True

                PlanetR _ ->
                    True

                _ ->
                    False

        AdminR ->
            case current of
                AdminR ->
                    True

                AdminListPeopleR ->
                    True

                AdminPersonR _ ->
                    True

                AdminNewPersonR ->
                    True

                _ ->
                    False

        FleetR ->
            case current of
                FleetR ->
                    True

                UnitR _ ->
                    True

                _ ->
                    False

        _ ->
            current == checked


breadcrumbPath : Model -> Html Msg
breadcrumbPath model =
    div [ class "container" ]
        [ ul [ class "breadcrumb" ]
            (breadcrumb model True <|
                parseLocation model.url
            )
        ]


{-| Given model and route, build segment of breadcrumb path as a triple.
First element of tuple is text that should be displayed in the breadcrumb
path. Second element is possible id for Html. Third element is possible
parent element of the route. For example HomeR is parent of StarSystemsR,
which in turn is parent of StarSystemR 1. Model can be used to compute
dynamic text to be displayed in breadcrumb path, for example a planet or
person name.
-}
segment : Model -> Route -> ( String, Maybe (Attribute Msg), Maybe Route )
segment model route =
    case route of
        AdminR ->
            ( "Admin", Nothing, Just HomeR )

        AdminListPeopleR ->
            ( "People", Nothing, Just AdminR )

        AdminPersonR _ ->
            let
                name =
                    RemoteData.map (\x -> displayName x.name) model.adminR.adminEditPersonR.person
                        |> RemoteData.withDefault "-"
            in
            ( name, Just (id "breadcrumb-person-name"), Just AdminListPeopleR )

        AdminNewPersonR ->
            ( "Add person", Nothing, Just AdminListPeopleR )

        BasesR ->
            ( "Bases", Nothing, Just HomeR )

        ConstructionR ->
            ( "Constructions", Nothing, Just HomeR )

        DesignerR ->
            ( "Designs", Nothing, Just HomeR )

        FleetR ->
            ( "Fleet", Nothing, Just HomeR )

        HomeR ->
            ( "Home", Nothing, Nothing )

        MessagesR ->
            ( "Messages", Nothing, Just HomeR )

        ProfileR ->
            ( "Profile", Nothing, Just HomeR )

        ResearchR ->
            ( "Research", Nothing, Just HomeR )

        StarSystemR _ ->
            let
                starSystemName =
                    model.starSystemR.starSystem
                        |> RemoteData.map (\x -> unStarSystemName x.name)
                        |> RemoteData.withDefault "Unknown star system"
            in
            ( starSystemName, Just (id "breadcrumb-system-name"), Just StarSystemsR )

        StarSystemsR ->
            ( "Star systems", Nothing, Just HomeR )

        PlanetR _ ->
            case model.planetR.planet of
                Nothing ->
                    ( "-", Just (id "breadcrumb-planet-name"), Just HomeR )

                Just planet ->
                    ( unPlanetName planet.name
                    , Just (id "breadcrumb-planet-name")
                    , Just (StarSystemR planet.systemId)
                    )

        PersonR _ ->
            let
                personName =
                    case model.personR.person of
                        Nothing ->
                            "-"

                        Just person ->
                            displayName person.name
            in
            ( personName, Just (id "breadcrumb-person-name"), Just HomeR )

        UnitR _ ->
            let
                unitName =
                    case model.unitR.unit of
                        Just (Ship details) ->
                            unShipName details.name

                        Just (Vehicle details) ->
                            unVehicleName details.name

                        Nothing ->
                            "-"
            in
            ( unitName, Just (id "breadcrumb-unit-name"), Just FleetR )

        LogoutR ->
            ( "Logout", Nothing, Just HomeR )


breadcrumb : Model -> Bool -> Route -> List (Html Msg)
breadcrumb model topLevel route =
    let
        ( linkText, linkId, linkParent ) =
            segment model route

        textEntry =
            if topLevel then
                text <| linkText

            else
                a [ href route ] [ text linkText ]

        entryClass =
            if topLevel then
                case linkId of
                    Nothing ->
                        [ class "active" ]

                    Just x ->
                        [ class "active", x ]

            else
                case linkId of
                    Nothing ->
                        []

                    Just x ->
                        [ x ]
    in
    case linkParent of
        Nothing ->
            [ li entryClass [ textEntry ] ]

        Just x ->
            breadcrumb model False x ++ [ li entryClass [ textEntry ] ]


view : Model -> Browser.Document Msg
view model =
    { title = "Deep Sky"
    , body =
        [ menuBar model [ AdminRole, PlayerRole ]
        , breadcrumbPath model
        , infoBar model
        , errorBar model
        , div [ class "container" ]
            [ currentPage model.url model ]
        ]
    }


currentPage : Url -> (Model -> Html Msg)
currentPage url =
    case parseLocation url of
        AdminR ->
            Views.Admin.Main.page

        AdminListPeopleR ->
            Views.Admin.People.List.page

        AdminPersonR _ ->
            Views.Admin.People.Edit.page

        AdminNewPersonR ->
            Views.Admin.People.Add.page

        BasesR ->
            Views.Bases.page

        ConstructionR ->
            Views.Construction.page

        DesignerR ->
            Views.Designer.page

        FleetR ->
            Views.Fleet.page

        HomeR ->
            Views.Home.page

        MessagesR ->
            Views.Messages.page

        ProfileR ->
            Views.Profile.page

        ResearchR ->
            Views.Research.page

        StarSystemR systemId ->
            Views.StarSystem.page systemId

        StarSystemsR ->
            Views.StarSystems.page

        PlanetR planetId ->
            Views.Planet.page planetId

        PersonR _ ->
            Views.Person.page

        UnitR _ ->
            Views.Unit.page

        LogoutR ->
            Views.Home.page
