module Data.Admin exposing (Person, Simulation, SystemStatus(..))

import Data.Common
    exposing
        ( DynastyId(..)
        , FactionId(..)
        , PersonId(..)
        , PlanetId(..)
        , StarDate(..)
        , StarSystemId(..)
        )
import Data.People
    exposing
        ( Gender(..)
        , PersonName(..)
        , Sex(..)
        , StatValue(..)
        )


type alias Simulation =
    { time : StarDate
    , status : SystemStatus
    }


type SystemStatus
    = Offline
    | Maintenance
    | Online
    | ProcessingTurn


type alias Person =
    { id : PersonId
    , name : PersonName
    , sex : Sex
    , gender : Gender
    , dateOfBirth : StarDate
    , diplomacy : StatValue
    , learning : StatValue
    , martial : StatValue
    , intrique : StatValue
    , stewardship : StatValue
    , factionId : Maybe FactionId
    , planetTitle : Maybe PlanetId
    , starSystemTitle : Maybe StarSystemId
    , dynastyId : Maybe DynastyId
    }
