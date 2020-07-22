module ViewModels.Unit exposing
    ( Tab(..)
    , UnitRMsg(..)
    , UnitViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.StarSystem exposing (Planet, StarSystem)
import Data.Vehicles exposing (Unit(..))
import RemoteData exposing (RemoteData(..), WebData)


type UnitRMsg
    = UnitDetailsReceived (WebData Unit)
    | PlanetDetailsReceived (WebData Planet)
    | StarSystemDetailsReceived (WebData StarSystem)
    | TabActivated Tab
    | CrewTabStatusChanged InfoPanelStatus
    | CrewSpaceInfoPanelStatusChanged InfoPanelStatus
    | CrewMessagesInfoPanelStatusChanged InfoPanelStatus
    | CrewPageChanged Int


type alias UnitViewModel =
    { activeTab : Tab
    , unit : WebData Unit
    , planet : WebData Planet
    , starSystem : WebData StarSystem
    , crewTabPageSize : Int
    , crewTabCurrentPage : Int
    , crewTabStatus : InfoPanelStatus
    , crewSpaceInfoPanelStatus : InfoPanelStatus
    , crewMessagesInfoPanelStatus : InfoPanelStatus
    }


init : UnitViewModel
init =
    { activeTab = GeneralInfo
    , unit = Loading
    , planet = NotAsked
    , starSystem = NotAsked
    , crewTabPageSize = 20
    , crewTabCurrentPage = 0
    , crewTabStatus = InfoPanelOpen
    , crewSpaceInfoPanelStatus = InfoPanelOpen
    , crewMessagesInfoPanelStatus = InfoPanelOpen
    }


type Tab
    = GeneralInfo
    | Crew
    | Orders
    | Log
    | DamageControl
    | Stats
