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
        , ResourceType
        , ShipId
        , StarDate
        , StarSystemId
        , UserId
        )
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
    { starName : String
    , systemName : String
    , systemId : StarSystemId
    }


type alias PlanetFoundNews =
    { planetName : String
    , systemName : String
    , systemId : StarSystemId
    , planetId : PlanetId
    }


type alias UserWrittenNews =
    { message : String
    , author : UserName
    , icon : UserIcon
    }


type alias DesignCreatedNews =
    { designId : DesignId
    , name : DesignName
    }


type alias BuildingFinishedNews =
    { planetName : String
    , planetId : PlanetId
    , systemName : String
    , systemId : StarSystemId
    , name : String
    , buildingId : BuildingId
    }


type alias ShipFinishedNews =
    { planetName : Maybe String
    , planetId : Maybe PlanetId
    , systemName : String
    , systemId : StarSystemId
    , name : String
    , shipId : ShipId
    }


type alias ProductionChangeNews =
    { planetName : String
    , planetId : PlanetId
    , systemName : String
    , systemId : StarSystemId
    , resourceType : ResourceType
    }


type alias ResearchCompletedNews =
    { name : String
    }


type alias KragiiSpecialEvent =
    { planetName : String
    , planetId : PlanetId
    , systemName : String
    , systemId : StarSystemId
    }


type alias KragiiResolution =
    { planetName : String
    , planetId : PlanetId
    , systemName : String
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
