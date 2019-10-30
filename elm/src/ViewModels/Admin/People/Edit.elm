module ViewModels.Admin.People.Edit exposing
    ( AdminEditPersonRMsg(..)
    , AdminEditPersonViewModel
    , Fields
    , emptyFields
    , init
    )

import Data.Admin exposing (Person)
import Data.Common exposing (PagedResult, PersonId)
import Dict exposing (Dict)
import Http


{-| Messages view model may emit
-}
type AdminEditPersonRMsg
    = PersonReceived (Result Http.Error Person)
    | DiplomacyChanged String
    | MartialChanged String
    | StewardshipChanged String
    | IntriqueChanged String
    | LearningChanged String
    | SexChanged String
    | GenderChanged String
    | NameTypeChanged String
    | FirstNameChanged String
    | CognomenChanged String
    | FamilyNameChanged String
    | RegnalNumberChanged String
    | DateOfBirthChanged String
    | UndoRequested
    | SaveRequested


{-| Current state of view model
-}
type alias AdminEditPersonViewModel =
    { person : Maybe Person
    , fields : Fields
    }


type alias Fields =
    { firstName : String
    , familyName : String
    , cognomen : String
    , regnalNumber : String
    , sex : String
    , gender : String
    , dateOfBirth : String
    , diplomacy : String
    , martial : String
    , stewardship : String
    , intrique : String
    , learning : String
    , nameType : String
    }


emptyFields : Fields
emptyFields =
    { firstName = ""
    , familyName = ""
    , cognomen = ""
    , regnalNumber = ""
    , sex = ""
    , gender = ""
    , dateOfBirth = ""
    , diplomacy = ""
    , martial = ""
    , stewardship = ""
    , intrique = ""
    , learning = ""
    , nameType = ""
    }


{-| Create initial view model
-}
init : AdminEditPersonViewModel
init =
    { person = Nothing
    , fields = emptyFields
    }
