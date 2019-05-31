module ViewModels.StarSystem exposing
    ( StarSystemRMsg(..)
    , StarSystemViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.StarSystem exposing (StarSystem)
import Http


type StarSystemRMsg
    = SystemDetailsStatusChanged InfoPanelStatus
    | StarListStatusChanged InfoPanelStatus
    | StarLaneListStatusChanged InfoPanelStatus
    | PlanetListStatusChanged InfoPanelStatus
    | StarSystemReceived (Result Http.Error StarSystem)


type alias StarSystemViewModel =
    { systemDetailsStatus : InfoPanelStatus
    , starListStatus : InfoPanelStatus
    , starLanesStatus : InfoPanelStatus
    , planetsStatus : InfoPanelStatus
    , starSystem : Maybe StarSystem
    }


init : StarSystemViewModel
init =
    { systemDetailsStatus = InfoPanelOpen
    , starListStatus = InfoPanelOpen
    , starLanesStatus = InfoPanelOpen
    , planetsStatus = InfoPanelOpen
    , starSystem = Nothing
    }
