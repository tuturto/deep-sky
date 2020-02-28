module ViewModels.Admin.People.List exposing
    ( AdminListPeopleRMsg(..)
    , AdminListPeopleViewModel
    , init
    )

import Data.Admin exposing (Person)
import Data.Common exposing (PagedResult, PersonId)
import Dict exposing (Dict)
import RemoteData exposing (RemoteData(..), WebData)


{-| Messages view model may emit
-}
type AdminListPeopleRMsg
    = PeopleReceived (WebData (PagedResult Person))
    | PersonSelected PersonId
    | PageRequested Int


{-| Current state of view model
-}
type alias AdminListPeopleViewModel =
    { currentPage : Int
    , pageSize : Int
    , people : Dict Int (WebData (List Person))
    }


{-| Create initial view model
-}
init : AdminListPeopleViewModel
init =
    { currentPage = 0
    , pageSize = 25
    , people =
        Dict.fromList
            [ ( 0, Loading )
            , ( 1, Loading )
            ]
    }
