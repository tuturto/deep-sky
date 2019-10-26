module Data.Admin exposing (Simulation, SystemStatus(..))

import Data.Common exposing (StarDate(..))


type alias Simulation =
    { time : StarDate
    , status : SystemStatus }


type SystemStatus
    = Offline
    | Maintenance
    | Online
    | ProcessingTurn
