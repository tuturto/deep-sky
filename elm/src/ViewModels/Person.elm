module ViewModels.Person exposing
    ( PersonRMsg(..)
    , PersonViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.People exposing (Person)
import Http


type PersonRMsg
    = PersonDetailsReceived (Result Http.Error Person)
    | PersonDetailsStatusChanged InfoPanelStatus
    | PersonDetailsRefreshRequested
    | StatsStatusChanged InfoPanelStatus


type alias PersonViewModel =
    { person : Maybe Person
    , personDetailsStatus : InfoPanelStatus
    , statsStatus : InfoPanelStatus
    }


init : PersonViewModel
init =
    { person = Nothing
    , personDetailsStatus = InfoPanelOpen
    , statsStatus = InfoPanelOpen
    }
