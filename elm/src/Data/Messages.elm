module Data.Messages exposing
    ( BuildingFinishedNews
    , DesignCreatedNews
    , KragiiResolution
    , KragiiSpecialEvent
    , NewsArticle
    , NewsContent(..)
    , PlanetFoundNews
    , ProductionChangeNews
    , ResearchCompletedNews
    , ShipFinishedNews
    , SpecialEventChoice(..)
    , SpecialEventOption
    , StarFoundNews
    , UserIcon(..)
    , UserWrittenNews
    , unSpecialEventChoice
    )

import Data.Common
    exposing
        ( BuildingId
        , DesignId
        , FactionId
        , MessageId
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
import Data.People exposing (PersonName)
import Data.User exposing (UserName)
import Data.Vehicles exposing (DesignName)


type alias NewsArticle =
    { messageId : MessageId
    , starDate : StarDate
    , icon : String
    , content : NewsContent
    , options : List SpecialEventOption
    , choice : Maybe SpecialEventChoice
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
    }


type alias KragiiResolution =
    { planetName : PlanetName
    , planetId : PlanetId
    , systemName : StarSystemName
    , systemId : StarSystemId
    , report : String
    }


type alias SpecialEventOption =
    { title : String
    , explanation : List String
    , choice : SpecialEventChoice
    }


type SpecialEventChoice
    = SpecialEventChoice String


unSpecialEventChoice : SpecialEventChoice -> String
unSpecialEventChoice (SpecialEventChoice x) =
    x


type UserIcon
    = GenericUserNewsIcon
    | JubilationUserNewsIcon
    | CatUserNewsIcon
