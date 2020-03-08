module ViewModels.Designer exposing
    ( DesignerRMsg(..)
    , DesignerViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.Vehicles
    exposing
        ( Chassis
        , Component
        , Design
        , PlannedComponent
        , UnitStats
        )
import RemoteData exposing (RemoteData(..), WebData)
import SaveData exposing (SaveData(..))


{-| Messages sent by designer view
-}
type DesignerRMsg
    = ChassisListStatusChanged InfoPanelStatus
    | ChassisListUpdatedRequested
    | ChassisListPageChanged Int
    | ComponentListStatusChanged InfoPanelStatus
    | ComponentListUpdatedRequested
    | ComponentListPageChanged Int
    | DesignsPanelStatusChanged InfoPanelStatus
    | DesignsPanelUpdatedRequested
    | CommandsStatusChanged InfoPanelStatus
    | MessagesStatusChanged InfoPanelStatus
    | ChassisSelected Chassis
    | ComponentAdded Component
    | ComponentRemoved PlannedComponent
    | DesignPanelStatusChanged InfoPanelStatus
    | DesignsPanelPageChanged Int
    | ShipNameChanged String
    | NewDesignStarted
    | SaveDesignRequested Design
    | DesignSelected Design
    | DesignDeleted Design
    | DesignCopied Design
    | StatsStatusChanged InfoPanelStatus
    | ComponentsReceived (WebData (List Component))
    | ChassisReceived (WebData (List Chassis))
    | DesignsReceived (WebData (List Design))
    | DesignEstimated (WebData UnitStats)
    | DesignSaved (WebData Design)


{-| State of the designer view
-}
type alias DesignerViewModel =
    { chassisListStatus : InfoPanelStatus
    , chassisPageSize : Int
    , chassisCurrentPage : Int
    , componentListStatus : InfoPanelStatus
    , componentsPageSize : Int
    , componentsCurrentPage : Int
    , designsPanelStatus : InfoPanelStatus
    , designsPageSize : Int
    , designsCurrentPage : Int
    , commandsStatus : InfoPanelStatus
    , messagesStatus : InfoPanelStatus
    , designPanelStatus : InfoPanelStatus
    , currentDesign : Maybe Design
    , designStats : WebData UnitStats
    , statsStatus : InfoPanelStatus
    , availableComponents : WebData (List Component)
    , availableChassis : WebData (List Chassis)
    , designs : WebData (List Design)
    }


{-| Create initial state for designer view
-}
init : DesignerViewModel
init =
    { chassisListStatus = InfoPanelOpen
    , chassisPageSize = 6
    , chassisCurrentPage = 0
    , componentListStatus = InfoPanelOpen
    , componentsPageSize = 6
    , componentsCurrentPage = 0
    , designsPanelStatus = InfoPanelOpen
    , designsPageSize = 15
    , designsCurrentPage = 0
    , commandsStatus = InfoPanelOpen
    , messagesStatus = InfoPanelOpen
    , designPanelStatus = InfoPanelOpen
    , currentDesign = Nothing
    , designStats = NotAsked
    , statsStatus = InfoPanelOpen
    , availableComponents = Loading
    , availableChassis = Loading
    , designs = Loading
    }
