module ViewModels.Person exposing
    ( PersonRMsg(..)
    , PersonViewModel
    , init
    )

import Data.People exposing (Person)
import Http


type PersonRMsg
    = PersonDetailsReceived (Result Http.Error Person)


type alias PersonViewModel =
    { person : Maybe Person }


init : PersonViewModel
init =
    { person = Nothing }
