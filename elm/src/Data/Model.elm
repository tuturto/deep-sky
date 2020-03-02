module Data.Model exposing
    ( ApiMsg(..)
    , Model
    , Msg(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Data.Common exposing (ErrorMessage, Resources, StarDate)
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
    , starSystemR : StarSystemViewModel
    , starSystemsR : StarSystemsViewModel
    , planetR : PlanetViewModel
    , messagesR : MessagesViewModel
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
