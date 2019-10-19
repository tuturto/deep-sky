module Data.Admin exposing (Simulation, SimulationStatus(..))

import Data.Common exposing (StarDate(..))


type alias Simulation =
    { time : StarDate }


type SimulationStatus
    = Offline
    | Maintenance
    | Online
    | ProcessingTurn
