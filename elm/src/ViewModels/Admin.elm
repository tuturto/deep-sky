module ViewModels.Admin exposing
    ( ActionStatus(..)
    , AdminRMsg(..)
    , AdminViewModel
    , DisplayMode(..)
    , init
    )

import Data.Admin exposing (Simulation)
import Http


{-| Messages admin view model may emit
-}
type AdminRMsg
    = SimulationStatusReceived (Result Http.Error Simulation)
    | ProcessTurn ActionStatus
    | DisplayPage DisplayMode


{-| Current state of admin view model
-}
type alias AdminViewModel =
    { simulation : Maybe Simulation
    , currentPage : DisplayMode
    , processTurn : ActionStatus
    }


{-| Currently displayed page on admin view
-}
type DisplayMode
    = AdminMenu
    | SimulationStatusMenu
    | PeopleMenu


{-| Track state of UI with confirmation action
-}
type ActionStatus
    = Inactive
    | Requested
    | Confirmed


{-| Create initial view model
-}
init : AdminViewModel
init =
    { simulation = Nothing
    , currentPage = AdminMenu
    , processTurn = Inactive
    }
