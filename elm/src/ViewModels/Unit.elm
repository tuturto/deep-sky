module ViewModels.Unit exposing
    ( Tab(..)
    , UnitRMsg(..)
    , UnitViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.StarSystem exposing (Planet, StarSystem)
import Data.Vehicles exposing (Unit(..))
import Http


type UnitRMsg
    = UnitDetailsReceived (Result Http.Error Unit)
    | PlanetDetailsReceived (Result Http.Error Planet)
    | StarSystemDetailsReceived (Result Http.Error StarSystem)
    | TabActivated Tab
    | CrewTabStatusChanged InfoPanelStatus
    | CrewPageChanged Int


type alias UnitViewModel =
    { activeTab : Tab
    , unit : Maybe Unit
    , planet : Maybe Planet
    , starSystem : Maybe StarSystem
    , crewTabPageSize : Int
    , crewTabCurrentPage : Int
    , crewTabStatus : InfoPanelStatus
    }


init : UnitViewModel
init =
    { activeTab = GeneralInfo
    , unit = Nothing
    , planet = Nothing
    , starSystem = Nothing
    , crewTabPageSize = 20
    , crewTabCurrentPage = 0
    , crewTabStatus = InfoPanelOpen
    }


type Tab
    = GeneralInfo
    | Crew
    | Orders
    | Log
    | DamageControl
    | Stats
