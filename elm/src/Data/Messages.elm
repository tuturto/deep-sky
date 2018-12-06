module Data.Messages exposing
    ( BuildingFinishedNews
    , DesignCreatedNews
    , NewsArticle
    , NewsContent(..)
    , PlanetFoundNews
    , ShipFinishedNews
    , StarFoundNews
    , UserIcon(..)
    , UserWrittenNews
    )

import Data.Common
    exposing
        ( BuildingId
        , DesignId
        , MessageId
        , PlanetId
        , ShipId
        , StarDate
        , StarSystemId
        , UserId
        )
import Data.User exposing (UserName)


type alias NewsArticle =
    { messageId : MessageId
    , starDate : StarDate
    , icon : String
    , content : NewsContent
    }


type NewsContent
    = StarFound StarFoundNews
    | PlanetFound PlanetFoundNews
    | UserWritten UserWrittenNews
    | DesignCreated DesignCreatedNews
    | BuildingFinished BuildingFinishedNews
    | ShipFinished ShipFinishedNews


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
    , name : String
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


type UserIcon
    = GenericUserNewsIcon
    | JubilationUserNewsIcon
    | CatUserNewsIcon
