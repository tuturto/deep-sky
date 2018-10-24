module ViewModels.StarSystem exposing
    ( StarSystemRMsg(..)
    , StarSystemViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))


type StarSystemRMsg
    = SystemDetailsStatusChanged InfoPanelStatus
    | StarListStatusChanged InfoPanelStatus
    | StarLaneListStatusChanged InfoPanelStatus
    | PlanetListStatusChanged InfoPanelStatus


type alias StarSystemViewModel =
    { systemDetailsStatus : InfoPanelStatus
    , starListStatus : InfoPanelStatus
    , starLanesStatus : InfoPanelStatus
    , planetsStatus : InfoPanelStatus
    }


init : StarSystemViewModel
init =
    { systemDetailsStatus = InfoPanelOpen
    , starListStatus = InfoPanelOpen
    , starLanesStatus = InfoPanelOpen
    , planetsStatus = InfoPanelOpen
    }
