module ViewModels.Admin.Main exposing
    ( AdminRMsg(..)
    , AdminViewModel
    , init
    )

import Data.Admin exposing (Simulation, SystemStatus)
import Http
import ViewModels.Admin.People.Add exposing (AdminAddPersonViewModel)
import ViewModels.Admin.People.Edit exposing (AdminEditPersonViewModel)
import ViewModels.Admin.People.List exposing (AdminListPeopleViewModel)


{-| Messages admin view model may emit
-}
type AdminRMsg
    = SimulationStatusReceived (Result Http.Error Simulation)
    | ChangeStatusRequested SystemStatus


{-| Current state of admin view model
-}
type alias AdminViewModel =
    { simulation : Maybe Simulation
    , adminListPeopleR : AdminListPeopleViewModel
    , adminEditPersonR : AdminEditPersonViewModel
    , adminAddPersonR : AdminAddPersonViewModel
    }


{-| Create initial view model
-}
init : AdminViewModel
init =
    { simulation = Nothing
    , adminListPeopleR = ViewModels.Admin.People.List.init
    , adminEditPersonR = ViewModels.Admin.People.Edit.init
    , adminAddPersonR = ViewModels.Admin.People.Add.init
    }
