module ViewModels.Person exposing
    ( PersonRMsg(..)
    , PersonViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.People exposing (DemesneShortInfo, Person)
import Http


type PersonRMsg
    = PersonDetailsReceived (Result Http.Error Person)
    | DemesneReceived (Result Http.Error (List DemesneShortInfo))
    | PersonDetailsStatusChanged InfoPanelStatus
    | PersonDetailsRefreshRequested
    | StatsStatusChanged InfoPanelStatus
    | DemesneStatusChanged InfoPanelStatus
    | DemesneRefreshRequested
    | DemesnePageChanged Int


type alias PersonViewModel =
    { person : Maybe Person
    , demesne : Maybe (List DemesneShortInfo)
    , personDetailsStatus : InfoPanelStatus
    , statsStatus : InfoPanelStatus
    , demesneStatus : InfoPanelStatus
    , demesnePageSize : Int
    , demesneCurrentPage : Int
    }


init : PersonViewModel
init =
    { person = Nothing
    , demesne = Nothing
    , personDetailsStatus = InfoPanelOpen
    , statsStatus = InfoPanelOpen
    , demesneStatus = InfoPanelOpen
    , demesnePageSize = 10
    , demesneCurrentPage = 0
    }