module Views.Layout exposing (view)

import Accessors exposing (get)
import Browser
import Browser.Navigation as Nav
import Data.Accessors exposing (planetsA, starSystemsA)
import Data.Common
    exposing
        ( ErrorMessage(..)
        , Route(..)
        , routeToString
        , unPlanetId
        , unPlanetName
        , unStarSystemId
        , unStarSystemName
        )
import Data.Model exposing (Model, Msg(..))
import Data.People exposing (displayName)
import Data.User exposing (Role(..))
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (member)
import Maybe exposing (andThen, withDefault)
import Navigation exposing (parseLocation, routes)
import Url exposing (Url)
import Url.Parser exposing (parse)
import Views.Admin.Main
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


menuBar : Model -> List Role -> Html Msg
menuBar model roles =
    let
        isAdmin =
            member AdminRole roles

        isplayer =
            member PlayerRole roles

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
            [ li []
                [ i [ class "fas fa-clock" ] []
                , starDateToText model.currentTime
                ]
            , li [] <|
                biologicalsToText model.resources
            , li [] <|
                mechanicalsToText model.resources
            , li [] <|
                chemicalsToText model.resources
            ]
        ]


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

                PlanetR _ _ ->
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


{-| Given model and route, build segment of breadcrumb path as a tuple.
First element of tuple is text that should be displayed in the breadcrumb
path. Second element is possible parent element of the route. For example
HomeR is parent of StarSystemsR, which in turn is parent of StarSystemR 1.
Model can be used to compute dynamic text to be displayed in breadcrumb path,
for example a planet or person name.
-}
segment : Model -> Route -> ( String, Maybe Route )
segment model route =
    case route of
        AdminR ->
            ( "Admin", Just HomeR )

        AdminListPeopleR ->
            ( "People", Just AdminR )

        AdminPersonR pId ->
            ( "Name Here", Just AdminListPeopleR )

        --TODO: person name
        BasesR ->
            ( "Bases", Just HomeR )

        ConstructionR ->
            ( "Constructions", Just HomeR )

        DesignerR ->
            ( "Designs", Just HomeR )

        FleetR ->
            ( "Fleet", Just HomeR )

        HomeR ->
            ( "Home", Nothing )

        MessagesR ->
            ( "Messages", Just HomeR )

        ProfileR ->
            ( "Profile", Just HomeR )

        ResearchR ->
            ( "Research", Just HomeR )

        StarSystemR systemId ->
            let
                starSystemName =
                    model.starSystems
                        |> andThen (Dict.get (unStarSystemId systemId))
                        |> andThen (\x -> Just (unStarSystemName x.name))
                        |> withDefault "Unknown star system"
            in
            ( starSystemName, Just StarSystemsR )

        StarSystemsR ->
            ( "Star systems", Just HomeR )

        PlanetR systemId planetId ->
            let
                planetName =
                    model.planets
                        |> andThen (Dict.get (unStarSystemId systemId))
                        |> withDefault []
                        |> List.filter (\planet -> unPlanetId planet.id == unPlanetId planetId)
                        |> List.head
                        |> andThen (\x -> Just (unPlanetName x.name))
                        |> withDefault "Unknown planet"
            in
            ( planetName, Just (StarSystemR systemId) )

        PersonR personId ->
            let
                personName =
                    case model.personR.person of
                        Nothing ->
                            "-"

                        Just person ->
                            displayName person.name
            in
            ( personName, Just HomeR )

        LogoutR ->
            ( "Logout", Just HomeR )


breadcrumb : Model -> Bool -> Route -> List (Html Msg)
breadcrumb model topLevel route =
    let
        segmentPair =
            segment model route

        textEntry =
            case topLevel of
                True ->
                    text <| Tuple.first segmentPair

                False ->
                    a [ href route ] [ text <| Tuple.first segmentPair ]

        entryClass =
            case topLevel of
                True ->
                    [ class "active" ]

                False ->
                    []
    in
    case Tuple.second segmentPair of
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
            [ currentPage model.url <| model ]
        ]
    }


currentPage : Url -> (Model -> Html Msg)
currentPage url =
    case parseLocation url of
        AdminR ->
            Views.Admin.Main.page

        AdminListPeopleR ->
            Views.Admin.People.List.page

        AdminPersonR pId ->
            Views.Admin.People.Edit.page

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

        PlanetR systemId planetId ->
            Views.Planet.page systemId planetId

        PersonR personId ->
            Views.Person.page

        LogoutR ->
            Views.Home.page
