module Main exposing (handleApiMsg, init, main, subscriptions, update)

{-| Main module of the application
-}

import Accessors exposing (get, over, set)
import Api.Common exposing (resourcesCmd, starDateCmd)
import Browser
import Browser.Navigation as Nav
import Data.Accessors
    exposing
        ( availableBuildingsA
        , buildingsA
        , constructionsA
        , errorsA
        , iconsA
        , newsA
        , planetStatusA
        , planetsA
        , populationsA
        , starSystemsA
        , starsA
        )
import Data.Common
    exposing
        ( InfoPanelStatus(..)
        , Route(..)
        , unPlanetId
        , unStarId
        , unStarSystemId
        )
import Data.Construction exposing (constructionPlanet)
import Data.Model
    exposing
        ( ApiMsg(..)
        , ErrorMessage(..)
        , Model
        , Msg(..)
        )
import Data.StarSystem exposing (Star, StarSystem)
import Data.User exposing (Role(..))
import Dict exposing (Dict)
import Dict.Extra exposing (groupBy)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error(..))
import List
import Maybe.Extra exposing (isJust)
import Navigation exposing (parseLocation)
import Url exposing (Url)
import ViewModels.Messages exposing (MessagesRMsg(..))
import ViewModels.Planet exposing (PlanetRMsg(..))
import ViewModels.StarSystem exposing (StarSystemRMsg(..))
import Views.Admin
import Views.Bases
import Views.Construction
import Views.Designer
import Views.Fleet
import Views.Home
import Views.Layout exposing (view)
import Views.Messages
import Views.Planet
import Views.Profile
import Views.Research
import Views.StarSystem
import Views.StarSystems



-- MAIN


{-| Main function
-}
main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


{-| Initialize the application
-}
init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { key = key
            , url = url
            , currentTime = Nothing
            , resources = Nothing
            , starSystems = Nothing
            , stars = Nothing
            , planets = Nothing
            , planetStatus = Nothing
            , populations = Nothing
            , buildings = Nothing
            , constructions = Nothing
            , availableBuildings = Nothing
            , news = Nothing
            , icons = Nothing
            , starSystemsR = ViewModels.StarSystem.init
            , planetR = ViewModels.Planet.init
            , messagesR = ViewModels.Messages.init
            , errors = []
            }
    in
    ( model
    , Cmd.batch
        [ starDateCmd
        , resourcesCmd
        , currentInit url <| model
        ]
    )


{-| Handle update messages
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , currentInit url <| model
            )

        ClearErrors ->
            ( set errorsA [] model
            , Cmd.none
            )

        ApiMsgCompleted message ->
            handleApiMsg message model

        StarSystemMessage message ->
            Views.StarSystem.update message model

        PlanetMessage message ->
            Views.Planet.update message model

        NewsMessage message ->
            Views.Messages.update message model


{-| Handle messages related to API calls
-}
handleApiMsg : ApiMsg -> Model -> ( Model, Cmd Msg )
handleApiMsg msg model =
    case msg of
        StarDateReceived (Ok starDate) ->
            ( { model | currentTime = Just starDate }
            , Cmd.none
            )

        StarDateReceived (Err err) ->
            ( { model | currentTime = Nothing }
                |> over errorsA (\errors -> error err "Failed to load star date" :: errors)
            , Cmd.none
            )

        ResourcesReceived (Ok resources) ->
            ( { model | resources = Just resources }
            , Cmd.none
            )

        ResourcesReceived (Err err) ->
            ( { model | resources = Nothing }
                |> over errorsA (\errors -> error err "Failed to load resources" :: errors)
            , Cmd.none
            )

        StarSystemsReceived (Ok starSystems) ->
            ( set starSystemsA (Just <| starSystemsToDict starSystems) model
            , Cmd.none
            )

        StarSystemsReceived (Err err) ->
            ( set starSystemsA Nothing model
                |> over errorsA (\errors -> error err "Failed to load star systems" :: errors)
            , Cmd.none
            )

        StarsReceived (Ok stars) ->
            ( set starsA (Just <| groupBy (\entry -> unStarSystemId entry.systemId) stars) model
            , Cmd.none
            )

        StarsReceived (Err err) ->
            ( set starSystemsA Nothing model
                |> over errorsA (\errors -> error err "Failed to load stars" :: errors)
            , Cmd.none
            )

        PlanetsReceived (Ok planets) ->
            ( set planetsA (Just <| groupBy (\entry -> unStarSystemId entry.systemId) planets) model
            , Cmd.none
            )

        PlanetsReceived (Err err) ->
            ( set planetsA Nothing model
                |> over errorsA (\errors -> error err "Failed to load planets" :: errors)
            , Cmd.none
            )

        PopulationReceived (Ok populations) ->
            ( set populationsA (Just <| groupBy (\entry -> unPlanetId entry.planetId) populations) model
            , Cmd.none
            )

        PopulationReceived (Err err) ->
            ( set populationsA Nothing model
                |> over errorsA (\errors -> error err "Failed to load populations" :: errors)
            , Cmd.none
            )

        BuildingsReceived (Ok buildings) ->
            ( set buildingsA (Just <| groupBy (\entry -> unPlanetId entry.planetId) buildings) model
            , Cmd.none
            )

        BuildingsReceived (Err err) ->
            ( set buildingsA Nothing model
                |> over errorsA (\errors -> error err "Failed to load buildings" :: errors)
            , Cmd.none
            )

        ConstructionsReceived (Ok constructions) ->
            ( set constructionsA
                (Just <|
                    groupBy (\entry -> unPlanetId <| just (constructionPlanet entry))
                        (List.filter (\entry -> isJust (constructionPlanet entry)) constructions)
                )
                model
            , Cmd.none
            )

        ConstructionsReceived (Err err) ->
            ( set constructionsA Nothing model
                |> over errorsA (\errors -> error err "Failed to load constructions" :: errors)
            , Cmd.none
            )

        AvailableBuildingsReceived (Ok buildings) ->
            ( set availableBuildingsA (Just buildings) model
            , Cmd.none
            )

        AvailableBuildingsReceived (Err err) ->
            ( set availableBuildingsA Nothing model
                |> over errorsA (\errors -> error err "Failed to load available buildings" :: errors)
            , Cmd.none
            )

        NewsReceived (Ok news) ->
            ( set newsA (Just news) model
            , Cmd.none
            )

        NewsReceived (Err err) ->
            ( set newsA Nothing model
                |> over errorsA (\errors -> error err "Failed to load recent news" :: errors)
            , Cmd.none
            )

        IconsReceived (Ok icons) ->
            ( set iconsA (Just icons) model
            , Cmd.none
            )

        IconsReceived (Err err) ->
            ( set iconsA Nothing model
                |> over errorsA (\errors -> error err "Failed to load user icons" :: errors)
            , Cmd.none
            )

        PlanetStatusReceived (Ok status) ->
            ( set planetStatusA (Just status) model
            , Cmd.none
            )

        PlanetStatusReceived (Err err) ->
            ( set planetStatusA Nothing model
                |> over errorsA (\errors -> error err "Failed to planet status" :: errors)
            , Cmd.none
            )


{-| Unsafe method to get x from Just x
-}
just : Maybe b -> b
just b =
    case b of
        Just value ->
            value

        Nothing ->
            Debug.todo "Partial function"


{-| Turn given error with descriptive text into error message
-}
error : Error -> String -> ErrorMessage
error err msg =
    ErrorMessage (msg ++ " - " ++ errorToString err)


{-| Turn Http error message into user readable string
-}
errorToString : Error -> String
errorToString err =
    case err of
        BadUrl url ->
            "Bad URL: " ++ url

        Timeout ->
            "timeout"

        NetworkError ->
            "network error"

        BadStatus _ ->
            "bad status"

        BadPayload msg _ ->
            "Bad payload: " ++ msg


{-| Given list of star systems, turn them into dictionary with star system id as key
-}
starSystemsToDict : List StarSystem -> Dict Int StarSystem
starSystemsToDict systems =
    Dict.fromList (List.map (\entry -> ( unStarSystemId entry.id, entry )) systems)



-- SUBSCRIPTIONS


{-| Create subscriptions at the start of program
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


{-| Give current page's init function
-}
currentInit : Url -> (Model -> Cmd Msg)
currentInit url =
    case parseLocation url of
        AdminR ->
            Views.Admin.init

        BasesR ->
            Views.Bases.init

        ConstructionR ->
            Views.Construction.init

        DesignerR ->
            Views.Designer.init

        FleetR ->
            Views.Fleet.init

        HomeR ->
            Views.Home.init

        MessagesR ->
            Views.Messages.init

        ProfileR ->
            Views.Profile.init

        ResearchR ->
            Views.Research.init

        StarSystemR systemId ->
            Views.StarSystem.init systemId

        StarSystemsR ->
            Views.StarSystems.init

        PlanetR starSystemId planetId ->
            Views.Planet.init starSystemId planetId

        LogoutR ->
            Views.Home.init
