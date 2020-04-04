module Main exposing (handleApiMsg, init, main, subscriptions, update)

{-| Main module of the application
-}

import Accessors exposing (over, set)
import Api.Common exposing (getResources, getStarDate)
import Browser
import Browser.Navigation as Nav
import Data.Accessors
    exposing
        ( adminAddPersonRA
        , adminEditPersonRA
        , adminListPeopleRA
        , adminRA
        , availableBuildingsA
        , availableResearchA
        , buildingsA
        , constructionsA
        , currentResearchA
        , designerRA
        , errorsA
        , iconsA
        , messagesRA
        , newsA
        , personRA
        , planetRA
        , planetStatusA
        , planetsA
        , populationsA
        , researchProductionA
        , researchRA
        , starSystemRA
        , unitRA
        )
import Data.Common
    exposing
        ( ErrorMessage(..)
        , InfoPanelStatus(..)
        , Route(..)
        , error
        , unPlanetId
        , unStarSystemId
        )
import Data.Construction exposing (constructionPlanet)
import Data.Model
    exposing
        ( ApiMsg(..)
        , Model
        , Msg(..)
        )
import Data.User exposing (Role(..))
import Dict.Extra exposing (groupBy)
import Http exposing (Error(..))
import List
import Maybe.Extra exposing (isJust)
import Navigation exposing (parseLocation)
import RemoteData exposing (RemoteData(..))
import Url exposing (Url)
import ViewModels.Admin.Main
import ViewModels.Admin.People.Add
import ViewModels.Admin.People.Edit
import ViewModels.Admin.People.List
import ViewModels.Designer exposing (DesignerRMsg(..))
import ViewModels.Messages exposing (MessagesRMsg(..))
import ViewModels.Person
import ViewModels.Planet exposing (PlanetRMsg(..))
import ViewModels.Research exposing (ResearchRMsg(..))
import ViewModels.StarSystem exposing (StarSystemRMsg(..))
import ViewModels.StarSystems
import ViewModels.Unit
import Views.Admin.Main
import Views.Admin.People.Add
import Views.Admin.People.Edit
import Views.Admin.People.List
import Views.Bases
import Views.Construction
import Views.Designer
import Views.Fleet
import Views.Home
import Views.Layout exposing (view)
import Views.Messages
import Views.Person
import Views.Planet
import Views.Profile
import Views.Research
import Views.StarSystem
import Views.StarSystems
import Views.Unit



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
init _ url key =
    let
        model =
            { key = key
            , url = url
            , currentTime = NotAsked
            , resources = NotAsked
            , planets = Nothing
            , planetStatus = Nothing
            , populations = Nothing
            , buildings = Nothing
            , constructions = Nothing
            , availableBuildings = Nothing
            , news = Nothing
            , icons = Nothing
            , starSystemR = ViewModels.StarSystem.init
            , starSystemsR = ViewModels.StarSystems.init
            , planetR = ViewModels.Planet.init
            , messagesR = ViewModels.Messages.init
            , availableResearch = Nothing
            , currentResearch = Nothing
            , researchProduction = Nothing
            , errors = []
            , researchR = ViewModels.Research.init
            , designerR = ViewModels.Designer.init
            , personR = ViewModels.Person.init
            , adminR = ViewModels.Admin.Main.init
            , unitR = ViewModels.Unit.init
            }
    in
    ( model
    , Cmd.batch
        [ getStarDate (ApiMsgCompleted << StarDateReceived)
        , getResources (ApiMsgCompleted << ResourcesReceived)
        , currentInit url <| model
        ]
    )


initViewModel : Url -> Model -> Model
initViewModel url model =
    case parseLocation url of
        AdminR ->
            set adminRA ViewModels.Admin.Main.init model

        HomeR ->
            model

        ProfileR ->
            model

        StarSystemsR ->
            model

        StarSystemR _ ->
            set starSystemRA ViewModels.StarSystem.init model

        PlanetR _ ->
            set planetRA ViewModels.Planet.init model

        BasesR ->
            model

        FleetR ->
            model

        DesignerR ->
            set designerRA ViewModels.Designer.init model

        ConstructionR ->
            model

        MessagesR ->
            set messagesRA ViewModels.Messages.init model

        PersonR _ ->
            set personRA ViewModels.Person.init model

        UnitR _ ->
            set unitRA ViewModels.Unit.init model

        AdminListPeopleR ->
            set (adminRA << adminListPeopleRA) ViewModels.Admin.People.List.init model

        AdminPersonR _ ->
            set (adminRA << adminEditPersonRA) ViewModels.Admin.People.Edit.init model

        AdminNewPersonR ->
            set (adminRA << adminAddPersonRA) ViewModels.Admin.People.Add.init model

        LogoutR ->
            model

        ResearchR ->
            set researchRA ViewModels.Research.init model


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
            ( initViewModel url { model | url = url }
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

        StarSystemsMessage message ->
            Views.StarSystems.update message model

        PlanetMessage message ->
            Views.Planet.update message model

        NewsMessage message ->
            Views.Messages.update message model

        ResearchMessage message ->
            Views.Research.update message model

        DesignerMessage message ->
            Views.Designer.update message model

        PersonMessage message ->
            Views.Person.update message model

        UnitMessage message ->
            Views.Unit.update message model

        AdminMessage message ->
            Views.Admin.Main.update message model

        AdminListPeopleMessage message ->
            Views.Admin.People.List.update message model

        AdminEditPersonMessage message ->
            Views.Admin.People.Edit.update message model

        AdminAddPersonMessage message ->
            Views.Admin.People.Add.update message model


{-| Handle messages related to API calls
-}
handleApiMsg : ApiMsg -> Model -> ( Model, Cmd Msg )
handleApiMsg msg model =
    case msg of
        StarDateReceived NotAsked ->
            ( model
            , Cmd.none
            )

        StarDateReceived Loading ->
            ( model
            , Cmd.none
            )

        StarDateReceived (Success starDate) ->
            ( { model | currentTime = Success starDate }
            , Cmd.none
            )

        StarDateReceived (Failure err) ->
            ( { model | currentTime = Failure err }
                |> over errorsA (\errors -> error err "Failed to load star date" :: errors)
            , Cmd.none
            )

        ResourcesReceived NotAsked ->
            ( model
            , Cmd.none
            )

        ResourcesReceived Loading ->
            ( model
            , Cmd.none
            )

        ResourcesReceived (Success resources) ->
            ( { model | resources = Success resources }
            , Cmd.none
            )

        ResourcesReceived (Failure err) ->
            ( { model | resources = Failure err }
                |> over errorsA (\errors -> error err "Failed to load resources" :: errors)
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
                |> over errorsA (\errors -> error err "Failed to load planet statuses" :: errors)
            , Cmd.none
            )

        AvailableResearchReceived (Ok status) ->
            ( set availableResearchA (Just status) model
            , Cmd.none
            )

        AvailableResearchReceived (Err err) ->
            ( set availableResearchA Nothing model
                |> over errorsA (\errors -> error err "Failed to load available research" :: errors)
            , Cmd.none
            )

        CurrentResearchReceived (Ok status) ->
            ( set currentResearchA (Just status) model
            , Cmd.none
            )

        CurrentResearchReceived (Err err) ->
            ( set currentResearchA Nothing model
                |> over errorsA (\errors -> error err "Failed to load current research" :: errors)
            , Cmd.none
            )

        ResearchProductionReceived (Ok status) ->
            ( set researchProductionA (Just status) model
            , Cmd.none
            )

        ResearchProductionReceived (Err err) ->
            ( set researchProductionA Nothing model
                |> over errorsA (\errors -> error err "Failed to load research production" :: errors)
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
            Views.Admin.Main.init

        AdminListPeopleR ->
            Views.Admin.People.List.init

        AdminPersonR pId ->
            Views.Admin.People.Edit.init pId

        AdminNewPersonR ->
            Views.Admin.People.Add.init

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

        PlanetR planetId ->
            Views.Planet.init planetId

        PersonR personId ->
            Views.Person.init personId

        UnitR unitId ->
            Views.Unit.init unitId

        LogoutR ->
            Views.Home.init
