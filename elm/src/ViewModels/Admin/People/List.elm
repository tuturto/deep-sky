module ViewModels.Admin.People.List exposing
    ( AdminListPeopleRMsg(..)
    , AdminListPeopleViewModel
    , init
    )

import Data.Admin exposing (Person)
import Data.Common exposing (PagedResult, PersonId)
import Dict exposing (Dict)
import Http


{-| Messages view model may emit
-}
type AdminListPeopleRMsg
    = PeopleReceived (Result Http.Error (PagedResult Person))
    | PersonSelected PersonId


{-| Current state of view model
-}
type alias AdminListPeopleViewModel =
    { currentPage : Int
    , pageSize : Int
    , people : Dict Int (Maybe (List Person))
    }


{-| Create initial view model
-}
init : AdminListPeopleViewModel
init =
    { currentPage = 0
    , pageSize = 50
    , people = Dict.empty
    }
