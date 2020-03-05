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
import RemoteData exposing (WebData)


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
    , designStats : Maybe UnitStats
    , statsStatus : InfoPanelStatus
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
    , designStats = Nothing
    , statsStatus = InfoPanelOpen
    }
