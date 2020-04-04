module ViewModels.StarSystems exposing
    ( StarSystemsRMsg(..)
    , StarSystemsViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..), StarSystemId)
import Data.StarSystem exposing (StarSystem)
import RemoteData exposing (RemoteData(..), WebData)


type StarSystemsRMsg
    = SystemsStatusChanged InfoPanelStatus
    | SystemsPageChanged Int
    | ViewSystemRequested StarSystemId
    | StarSystemsReceived (WebData (List StarSystem))


type alias StarSystemsViewModel =
    { systemsStatus : InfoPanelStatus
    , systemsPageSize : Int
    , systemsCurrentPage : Int
    , systems : WebData (List StarSystem)
    }


init : StarSystemsViewModel
init =
    { systemsStatus = InfoPanelOpen
    , systemsPageSize = 50
    , systemsCurrentPage = 0
    , systems = Loading
    }
