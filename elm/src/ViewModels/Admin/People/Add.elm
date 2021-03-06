module ViewModels.Admin.People.Add exposing
    ( AdminAddPersonRMsg(..)
    , AdminAddPersonViewModel
    , AgeFields
    , Fields
    , emptyFields
    , init
    )

import Data.Admin exposing (Person)
import RemoteData exposing (RemoteData(..), WebData)


{-| Messages view model may emit
-}
type AdminAddPersonRMsg
    = ExactAgeChanged String
    | AgeOptionChanged String
    | StartAgeChanged String
    | EndAgeChanged String
    | CreationRequested
    | PersonCreated (WebData Person)


{-| Current state of view model
-}
type alias AdminAddPersonViewModel =
    { fields : Fields
    , person : WebData Person
    }


type alias Fields =
    { ageOption : String
    , ageFields : AgeFields
    }


type alias AgeFields =
    { startAge : String
    , endAge : String
    , exactAge : String
    }


emptyAgeFields : AgeFields
emptyAgeFields =
    { startAge = ""
    , endAge = ""
    , exactAge = ""
    }


emptyFields : Fields
emptyFields =
    { ageOption = ""
    , ageFields = emptyAgeFields
    }


{-| Create initial view model
-}
init : AdminAddPersonViewModel
init =
    { fields = emptyFields
    , person = NotAsked
    }
