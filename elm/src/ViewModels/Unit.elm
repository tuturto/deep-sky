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
import RemoteData exposing (RemoteData(..), WebData)


type UnitRMsg
    = UnitDetailsReceived (Result Http.Error Unit)
    | PlanetDetailsReceived (Result Http.Error Planet)
    | StarSystemDetailsReceived (WebData StarSystem)
    | TabActivated Tab
    | CrewTabStatusChanged InfoPanelStatus
    | CrewPageChanged Int


type alias UnitViewModel =
    { activeTab : Tab
    , unit : Maybe Unit
    , planet : Maybe Planet
    , starSystem : WebData StarSystem
    , crewTabPageSize : Int
    , crewTabCurrentPage : Int
    , crewTabStatus : InfoPanelStatus
    }


init : UnitViewModel
init =
    { activeTab = GeneralInfo
    , unit = Nothing
    , planet = Nothing
    , starSystem = Loading
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
