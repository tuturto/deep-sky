module Data.Model exposing
    ( ApiMsg(..)
    , ErrorMessage(..)
    , Model
    , Msg(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Data.Common exposing (InfoPanelStatus, Resources, StarDate)
import Data.Construction exposing (Building, BuildingInfo, Construction)
import Data.Messages exposing (NewsArticle, UserIcon)
import Data.StarSystem exposing (Planet, PlanetStatus, Population, Star, StarSystem)
import Dict exposing (Dict)
import Http
import Url exposing (Url)
import ViewModels.Messages exposing (MessagesRMsg, MessagesViewModel)
import ViewModels.Planet exposing (PlanetRMsg(..), PlanetViewModel)
import ViewModels.StarSystem exposing (StarSystemRMsg, StarSystemViewModel)


type alias Model =
    { key : Key
    , url : Url
    , currentTime : Maybe StarDate
    , resources : Maybe Resources
    , starSystems : Maybe (Dict Int StarSystem)
    , planets : Maybe (Dict Int (List Planet))
    , planetStatus : Maybe PlanetStatus
    , stars : Maybe (Dict Int (List Star))
    , populations : Maybe (Dict Int (List Population))
    , buildings : Maybe (Dict Int (List Building))
    , constructions : Maybe (Dict Int (List Construction))
    , availableBuildings : Maybe (List BuildingInfo)
    , news : Maybe (List NewsArticle)
    , starSystemsR : StarSystemViewModel
    , planetR : PlanetViewModel
    , messagesR : MessagesViewModel
    , icons : Maybe (List ( UserIcon, String ))
    , errors : List ErrorMessage
    }


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | ClearErrors
    | ApiMsgCompleted ApiMsg
    | StarSystemMessage StarSystemRMsg
    | PlanetMessage PlanetRMsg
    | NewsMessage MessagesRMsg


type ApiMsg
    = StarDateReceived (Result Http.Error StarDate)
    | ResourcesReceived (Result Http.Error Resources)
    | StarSystemsReceived (Result Http.Error (List StarSystem))
    | StarsReceived (Result Http.Error (List Star))
    | PlanetsReceived (Result Http.Error (List Planet))
    | PopulationReceived (Result Http.Error (List Population))
    | BuildingsReceived (Result Http.Error (List Building))
    | ConstructionsReceived (Result Http.Error (List Construction))
    | AvailableBuildingsReceived (Result Http.Error (List BuildingInfo))
    | NewsReceived (Result Http.Error (List NewsArticle))
    | IconsReceived (Result Http.Error (List ( UserIcon, String )))
    | PlanetStatusReceived (Result Http.Error PlanetStatus)


type ErrorMessage
    = ErrorMessage String
