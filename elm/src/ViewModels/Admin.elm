module ViewModels.Admin exposing
    ( AdminRMsg(..)
    , AdminViewModel
    , DisplayMode(..)
    , init
    )

import Data.Admin exposing (Simulation, SystemStatus)
import Http


{-| Messages admin view model may emit
-}
type AdminRMsg
    = SimulationStatusReceived (Result Http.Error Simulation)
    | ChangeStatusRequested SystemStatus
    | DisplayPage DisplayMode


{-| Current state of admin view model
-}
type alias AdminViewModel =
    { simulation : Maybe Simulation
    , currentPage : DisplayMode
    }


{-| Currently displayed page on admin view
-}
type DisplayMode
    = AdminMenu
    | SimulationStatusMenu
    | PeopleMenu


{-| Create initial view model
-}
init : AdminViewModel
init =
    { simulation = Nothing
    , currentPage = AdminMenu
    }
