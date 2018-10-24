module Views.Layout exposing (view)

import Accessors exposing (get)
import Browser
import Browser.Navigation as Nav
import Data.Accessors exposing (planetsA, starSystemsA)
import Data.Common
    exposing
        ( Route(..)
        , unPlanetId
        , unStarSystemId
        )
import Data.Model exposing (ErrorMessage(..), Model, Msg(..))
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
import Views.Admin
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
                    [ li [ mClass AdminR ] [ a [ href AdminR ] [ text "Admin" ] ]
                    , li [ mClass LogoutR ] [ a [ href LogoutR ] [ text "Logout" ] ]
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


similarRoutes : Route -> Route -> Bool
similarRoutes current checked =
    case current of
        StarSystemR _ ->
            case checked of
                StarSystemR _ ->
                    True

                StarSystemsR ->
                    True

                _ ->
                    False

        PlanetR _ _ ->
            case checked of
                PlanetR _ _ ->
                    True

                StarSystemR _ ->
                    True

                StarSystemsR ->
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


segment : Model -> Route -> ( String, Maybe Route )
segment model route =
    case route of
        AdminR ->
            ( "Admin", Just HomeR )

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
                    get starSystemsA model
                        |> andThen (Dict.get (unStarSystemId systemId))
                        |> andThen (\x -> Just x.name)
                        |> withDefault "Unknown star system"
            in
            ( starSystemName, Just StarSystemsR )

        StarSystemsR ->
            ( "Star systems", Just HomeR )

        PlanetR systemId planetId ->
            let
                planetName =
                    get planetsA model
                        |> andThen (Dict.get (unStarSystemId systemId))
                        |> withDefault []
                        |> List.filter (\planet -> unPlanetId planet.id == unPlanetId planetId)
                        |> List.head
                        |> andThen (\x -> Just x.name)
                        |> withDefault "Unknown planet"
            in
            ( planetName, Just (StarSystemR systemId) )

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
            Views.Admin.page

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

        LogoutR ->
            Views.Home.page
