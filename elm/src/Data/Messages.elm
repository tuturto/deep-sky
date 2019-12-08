module Data.Messages exposing
    ( BuildingFinishedNews
    , DesignCreatedNews
    , EventResolveType(..)
    , KragiiResolution
    , KragiiSpecialEvent
    , NamingPetResolution
    , NamingPetSpecialEvent
    , NewsArticle
    , NewsContent(..)
    , PlanetFoundNews
    , ProductionChangeNews
    , ResearchCompletedNews
    , ScurryingSoundsResolution
    , ScurryingSoundsSpecialEvent
    , ShipFinishedNews
    , SpecialEventChoice(..)
    , SpecialEventOption
    , StarFoundNews
    , UserIcon(..)
    , UserWrittenNews
    )

import Data.Common
    exposing
        ( BuildingId
        , DesignId
        , FactionId
        , MessageId
        , PersonId
        , PetId
        , PlanetId
        , PlanetName
        , ResourceType
        , ShipId
        , StarDate
        , StarName
        , StarSystemId
        , StarSystemName
        , UserId
        )
import Data.People exposing (PetType)
import Data.PersonNames exposing (PersonName)
import Data.User exposing (UserName)
import Data.Vehicles exposing (DesignName)


type alias NewsArticle =
    { messageId : MessageId
    , starDate : StarDate
    , icon : String
    , content : NewsContent
    , options : List SpecialEventOption
    , choice : Maybe SpecialEventChoice
    , resolveType : Maybe EventResolveType
    }


type NewsContent
    = StarFound StarFoundNews
    | PlanetFound PlanetFoundNews
    | UserWritten UserWrittenNews
    | DesignCreated DesignCreatedNews
    | BuildingFinished BuildingFinishedNews
    | ShipFinished ShipFinishedNews
    | ProductionBoostStarted ProductionChangeNews
    | ProductionSlowdownStarted ProductionChangeNews
    | ProductionBoostEnded ProductionChangeNews
    | ProductionSlowdownEnded ProductionChangeNews
    | ResearchCompleted ResearchCompletedNews
    | KragiiEvent KragiiSpecialEvent
    | KragiiResolved KragiiResolution
    | ScurryingSoundsEvent ScurryingSoundsSpecialEvent
    | ScurryingSoundsResolved ScurryingSoundsResolution
    | NamingPetEvent NamingPetSpecialEvent
    | PetNamingResolved NamingPetResolution


type alias StarFoundNews =
    { starName : StarName
    , systemName : StarSystemName
    , systemId : StarSystemId
    }


type alias PlanetFoundNews =
    { planetName : PlanetName
    , systemName : StarSystemName
    , systemId : StarSystemId
    , planetId : PlanetId
    }


type alias UserWrittenNews =
    { message : String
    , author : PersonName
    , icon : UserIcon
    }


type alias DesignCreatedNews =
    { designId : DesignId
    , name : DesignName
    }


type alias BuildingFinishedNews =
    { planetName : PlanetName
    , planetId : PlanetId
    , systemName : StarSystemName
    , systemId : StarSystemId
    , name : String
    , buildingId : BuildingId
    }


type alias ShipFinishedNews =
    { planetName : Maybe PlanetName
    , planetId : Maybe PlanetId
    , systemName : StarSystemName
    , systemId : StarSystemId
    , name : String
    , shipId : ShipId
    }


type alias ProductionChangeNews =
    { planetName : PlanetName
    , planetId : PlanetId
    , systemName : StarSystemName
    , systemId : StarSystemId
    , resourceType : ResourceType
    }


type alias ResearchCompletedNews =
    { name : String
    }


type alias KragiiSpecialEvent =
    { planetName : PlanetName
    , planetId : PlanetId
    , systemName : StarSystemName
    , systemId : StarSystemId
    , factionId : FactionId
    }


type alias KragiiResolution =
    { planetName : PlanetName
    , planetId : PlanetId
    , systemName : StarSystemName
    , systemId : StarSystemId
    , report : String
    }


type alias ScurryingSoundsSpecialEvent =
    { personId : PersonId
    }


type alias ScurryingSoundsResolution =
    { petId : Maybe PetId
    , petType : Maybe PetType
    , report : String
    }


type alias NamingPetSpecialEvent =
    { personId : PersonId
    , petId : PetId
    , petType : PetType
    }


type alias NamingPetResolution =
    { petId : PetId
    , petType : PetType
    , report : String
    }


type alias SpecialEventOption =
    { title : String
    , explanation : List String
    , choice : SpecialEventChoice
    }


{-| There are three different structures of data for special event choice
Simplest one is just string that is used when data in server is enumeration
Tag only is used when there's value constructors present with parameters, but this one doesn't
Tag and contents is for value constructor with parameters
-}
type SpecialEventChoice
    = EnumOnly String
    | TagOnly String
    | TagAndContents String String


type UserIcon
    = GenericUserNewsIcon
    | JubilationUserNewsIcon
    | CatUserNewsIcon


{-| How soon after making choice a special event is resolved.
Immediate events are resolved as soon as person has made a choice. In case
person never makes a choice, event is resolved during turn simulation. Deferred
events are always resolved during turn simulation.
-}
type EventResolveType
    = ImmediateEvent
    | DelayedEvent
