module ViewModels.Person exposing
    ( PersonRMsg(..)
    , PersonViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.People exposing (DemesneShortInfo, Person)
import RemoteData exposing (RemoteData(..), WebData)


type PersonRMsg
    = PersonDetailsReceived (WebData Person)
    | DemesneReceived (WebData (List DemesneShortInfo))
    | PersonDetailsStatusChanged InfoPanelStatus
    | PersonDetailsRefreshRequested
    | StatsStatusChanged InfoPanelStatus
    | DemesneStatusChanged InfoPanelStatus
    | DemesneRefreshRequested
    | DemesnePageChanged Int
    | RelationsStatusChanged InfoPanelStatus
    | RelationsPageChanged Int
    | TraitsStatusChanged InfoPanelStatus
    | TraitsPageChanged Int


type alias PersonViewModel =
    { person : WebData Person
    , demesne : WebData (List DemesneShortInfo)
    , personDetailsStatus : InfoPanelStatus
    , statsStatus : InfoPanelStatus
    , demesneStatus : InfoPanelStatus
    , demesnePageSize : Int
    , demesneCurrentPage : Int
    , relationsStatus : InfoPanelStatus
    , relationsPageSize : Int
    , relationsCurrentPage : Int
    , traitsStatus : InfoPanelStatus
    , traitsPageSize : Int
    , traitsCurrentPage : Int
    }


init : PersonViewModel
init =
    { person = Loading
    , demesne = Loading
    , personDetailsStatus = InfoPanelOpen
    , statsStatus = InfoPanelOpen
    , demesneStatus = InfoPanelOpen
    , demesnePageSize = 10
    , demesneCurrentPage = 0
    , relationsStatus = InfoPanelOpen
    , relationsPageSize = 10
    , relationsCurrentPage = 0
    , traitsStatus = InfoPanelOpen
    , traitsPageSize = 10
    , traitsCurrentPage = 0
    }
