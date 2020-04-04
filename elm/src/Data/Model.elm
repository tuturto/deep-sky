module Data.Model exposing
    ( ApiMsg(..)
    , Model
    , Msg(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Data.Common exposing (ErrorMessage, Resources, StarDate)
import Data.Construction exposing (Building, BuildingInfo, Construction)
import Data.Messages exposing (NewsArticle, UserIcon)
import Data.Research exposing (CurrentResearch, Research, TotalResearchScore)
import Data.StarSystem
    exposing
        ( Planet
        , PlanetStatus
        , Population
        , Star
        , StarSystem
        )
import Dict exposing (Dict)
import Http
import RemoteData exposing (WebData)
import Url exposing (Url)
import ViewModels.Admin.Main exposing (AdminRMsg, AdminViewModel)
import ViewModels.Admin.People.Add exposing (AdminAddPersonRMsg)
import ViewModels.Admin.People.Edit exposing (AdminEditPersonRMsg)
import ViewModels.Admin.People.List exposing (AdminListPeopleRMsg)
import ViewModels.Designer exposing (DesignerRMsg, DesignerViewModel)
import ViewModels.Messages exposing (MessagesRMsg, MessagesViewModel)
import ViewModels.Person exposing (PersonRMsg, PersonViewModel)
import ViewModels.Planet exposing (PlanetRMsg(..), PlanetViewModel)
import ViewModels.Research exposing (ResearchRMsg(..), ResearchViewModel)
import ViewModels.StarSystem exposing (StarSystemRMsg, StarSystemViewModel)
import ViewModels.StarSystems exposing (StarSystemsRMsg, StarSystemsViewModel)
import ViewModels.Unit exposing (UnitRMsg, UnitViewModel)


type alias Model =
    { key : Key
    , url : Url
    , currentTime : WebData StarDate
    , resources : WebData Resources
    , planets : Maybe (Dict Int (List Planet))
    , planetStatus : Maybe PlanetStatus
    , populations : Maybe (Dict Int (List Population))
    , buildings : Maybe (Dict Int (List Building))
    , constructions : Maybe (Dict Int (List Construction))
    , availableBuildings : Maybe (List BuildingInfo)
    , news : Maybe (List NewsArticle)
    , starSystemR : StarSystemViewModel
    , starSystemsR : StarSystemsViewModel
    , planetR : PlanetViewModel
    , messagesR : MessagesViewModel
    , icons : Maybe (List ( UserIcon, String ))
    , availableResearch : Maybe (List Research)
    , currentResearch : Maybe (List CurrentResearch)
    , researchProduction : Maybe TotalResearchScore
    , errors : List ErrorMessage
    , researchR : ResearchViewModel
    , designerR : DesignerViewModel
    , personR : PersonViewModel
    , adminR : AdminViewModel
    , unitR : UnitViewModel
    }


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | ClearErrors
    | ApiMsgCompleted ApiMsg
    | StarSystemMessage StarSystemRMsg
    | StarSystemsMessage StarSystemsRMsg
    | PlanetMessage PlanetRMsg
    | NewsMessage MessagesRMsg
    | ResearchMessage ResearchRMsg
    | DesignerMessage DesignerRMsg
    | PersonMessage PersonRMsg
    | UnitMessage UnitRMsg
    | AdminMessage AdminRMsg
    | AdminListPeopleMessage AdminListPeopleRMsg
    | AdminEditPersonMessage AdminEditPersonRMsg
    | AdminAddPersonMessage AdminAddPersonRMsg


type ApiMsg
    = StarDateReceived (WebData StarDate)
    | ResourcesReceived (WebData Resources)
    | PlanetsReceived (Result Http.Error (List Planet))
    | PopulationReceived (Result Http.Error (List Population))
    | BuildingsReceived (Result Http.Error (List Building))
    | ConstructionsReceived (Result Http.Error (List Construction))
    | AvailableBuildingsReceived (Result Http.Error (List BuildingInfo))
    | NewsReceived (Result Http.Error (List NewsArticle))
    | IconsReceived (Result Http.Error (List ( UserIcon, String )))
    | PlanetStatusReceived (Result Http.Error PlanetStatus)
    | AvailableResearchReceived (Result Http.Error (List Research))
    | CurrentResearchReceived (Result Http.Error (List CurrentResearch))
    | ResearchProductionReceived (Result Http.Error TotalResearchScore)
