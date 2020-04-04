module ViewModels.StarSystem exposing
    ( StarSystemRMsg(..)
    , StarSystemViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.StarSystem exposing (Star, StarSystem)
import RemoteData exposing (RemoteData(..), WebData)


type StarSystemRMsg
    = SystemDetailsStatusChanged InfoPanelStatus
    | StarListStatusChanged InfoPanelStatus
    | StarLaneListStatusChanged InfoPanelStatus
    | PlanetListStatusChanged InfoPanelStatus
    | StarSystemReceived (WebData StarSystem)
    | StarsReceived (WebData (List Star))


type alias StarSystemViewModel =
    { systemDetailsStatus : InfoPanelStatus
    , starListStatus : InfoPanelStatus
    , starLanesStatus : InfoPanelStatus
    , planetsStatus : InfoPanelStatus
    , starSystem : WebData StarSystem
    , stars : WebData (List Star)
    }


init : StarSystemViewModel
init =
    { systemDetailsStatus = InfoPanelOpen
    , starListStatus = InfoPanelOpen
    , starLanesStatus = InfoPanelOpen
    , planetsStatus = InfoPanelOpen
    , starSystem = Loading
    , stars = Loading
    }
