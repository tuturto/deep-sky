{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module News.Data ( NewsArticle(..), UserNewsIcon(..), StarFoundNews(..)
                 , PlanetFoundNews(..), UserWrittenNews(..), DesignCreatedNews(..)
                 , ConstructionFinishedNews(..), SpecialNews(..), mkNews, mkSpecialNews
                 )
    where

import Import
import Data.Aeson.TH
import Data.Aeson.Text ( encodeToLazyText )
import Common ( ToDto(..), FromDto(..) )
import CustomTypes ( SpecialEventStatus(..) )
import Dto.News ( NewsDto(..), NewsArticleDto(..), StarFoundNewsDto(..), PlanetFoundNewsDto(..)
                , UserWrittenNewsDto(..), DesignCreatedNewsDto(..), ConstructionFinishedNewsDto(..)
                , IconMapper(..), UserNewsIconDto(..), SpecialNewsDto(..), KragiiWormsEventDto(..)
                )
import Events.Import ( UserOption(..) )
import Events.Kragii ( KragiiWormsEvent(..), KragiiWormsChoice(..), KragiiNews(..) )


-- | All possible news articles
data NewsArticle =
    StarFound StarFoundNews
    | PlanetFound PlanetFoundNews
    | UserWritten UserWrittenNews
    | DesignCreated DesignCreatedNews
    | ConstructionFinished ConstructionFinishedNews
    | KragiiResolution KragiiNews
    | Special SpecialNews


-- | News announcing discovery of a new star
data StarFoundNews = StarFoundNews
    { starFoundNewsStarName :: Text
    , starFoundNewsSystemName :: Text
    , starFoundNewsSystemId :: Key StarSystem
    , starFoundNewsDate :: Int
    }


instance ToDto StarFoundNews StarFoundNewsDto where
    toDto news =
        StarFoundNewsDto { starFoundNewsDtoStarName = starFoundNewsStarName news
                         , starFoundNewsDtoSystemName = starFoundNewsSystemName news
                         , starFoundNewsDtoSystemId = starFoundNewsSystemId news
                         , starFoundNewsDtoDate = starFoundNewsDate news
                         }


instance FromDto StarFoundNews StarFoundNewsDto where
    fromDto dto =
        StarFoundNews { starFoundNewsStarName = starFoundNewsDtoStarName dto
                      , starFoundNewsSystemName = starFoundNewsDtoSystemName dto
                      , starFoundNewsSystemId = starFoundNewsDtoSystemId dto
                      , starFoundNewsDate = starFoundNewsDtoDate dto
                      }


-- | News announcing discovery of a new planet
data PlanetFoundNews = PlanetFoundNews
    { planetFoundNewsPlanetName :: Text
    , planetFoundNewsSystemName :: Text
    , planetFoundNewsSystemId :: Key StarSystem
    , planetFoundNewsPlanetId :: Key Planet
    , planetFoundNewsDate :: Int
    }


instance ToDto PlanetFoundNews PlanetFoundNewsDto where
    toDto news =
        PlanetFoundNewsDto { planetFoundNewsDtoPlanetName = planetFoundNewsPlanetName news
                           , planetFoundNewsDtoSystemName = planetFoundNewsSystemName news
                           , planetFoundNewsDtoSystemId = planetFoundNewsSystemId news
                           , planetFoundNewsDtoPlanetId = planetFoundNewsPlanetId news
                           , planetFoundNewsDtoDate = planetFoundNewsDate news
                           }


instance FromDto PlanetFoundNews PlanetFoundNewsDto where
    fromDto dto =
        PlanetFoundNews { planetFoundNewsPlanetName = planetFoundNewsDtoPlanetName dto
                        , planetFoundNewsSystemName = planetFoundNewsDtoSystemName dto
                        , planetFoundNewsSystemId = planetFoundNewsDtoSystemId dto
                        , planetFoundNewsPlanetId = planetFoundNewsDtoPlanetId dto
                        , planetFoundNewsDate = planetFoundNewsDtoDate dto
                        }


-- | User supplied news
data UserWrittenNews = UserWrittenNews
    { userWrittenNewsContent :: Text
    , userWrittenNewsIcon :: UserNewsIcon
    , userWrittenNewsDate :: Int
    , userWrittenNewsUser :: Text
    }


instance ToDto UserWrittenNews UserWrittenNewsDto where
    toDto news =
        UserWrittenNewsDto { userWrittenNewsDtoContent = userWrittenNewsContent news
                           , userWrittenNewsDtoDate = userWrittenNewsDate  news
                           , userWrittenNewsDtoUser = userWrittenNewsUser news
                           , userWrittenNewsDtoIcon = toDto $ userWrittenNewsIcon news
                           }


instance FromDto UserWrittenNews UserWrittenNewsDto where
    fromDto dto =
        UserWrittenNews { userWrittenNewsContent = userWrittenNewsDtoContent dto
                        , userWrittenNewsIcon = fromDto $ userWrittenNewsDtoIcon dto
                        , userWrittenNewsDate = userWrittenNewsDtoDate dto
                        , userWrittenNewsUser = userWrittenNewsDtoUser dto
                        }


-- | News announcing creation of a new design
data DesignCreatedNews = DesignCreatedNews
    { designCreatedNewsDesignId :: Key Design
    , designCreatedNewsName :: Text
    , designCreatedDate :: Int
    }


instance ToDto DesignCreatedNews DesignCreatedNewsDto where
    toDto news =
        DesignCreatedNewsDto { designCreatedNewsDtoDesignId = designCreatedNewsDesignId news
                             , designCreatedNewsDtoName = designCreatedNewsName news
                             , designCreatedNewsDtoDate = designCreatedDate news
                             }


instance FromDto DesignCreatedNews DesignCreatedNewsDto where
    fromDto dto =
        DesignCreatedNews { designCreatedNewsDesignId = designCreatedNewsDtoDesignId dto
                          , designCreatedNewsName = designCreatedNewsDtoName dto
                          , designCreatedDate = designCreatedNewsDtoDate dto
                          }


data ConstructionFinishedNews = ConstructionFinishedNews
    { constructionFinishedNewsPlanetName :: Maybe Text
    , constructionFinishedNewsPlanetId :: Maybe (Key Planet)
    , constructionFinishedNewsSystemName :: Text
    , constructionFinishedNewsSystemId :: Key StarSystem
    , constructionFinishedConstructionName :: Text
    , constructionFinishedBuildingId :: Maybe (Key Building)
    , constructionFinishedShipId :: Maybe (Key Ship)
    , constructionFinishedDate :: Int
    }


instance ToDto ConstructionFinishedNews ConstructionFinishedNewsDto where
    toDto news =
        ConstructionFinishedNewsDto { constructionFinishedNewsDtoPlanetName = constructionFinishedNewsPlanetName news
                                    , constructionFinishedNewsDtoPlanetId = constructionFinishedNewsPlanetId news
                                    , constructionFinishedNewsDtoSystemName = constructionFinishedNewsSystemName news
                                    , constructionFinishedNewsDtoSystemId = constructionFinishedNewsSystemId news
                                    , constructionFinishedNewsDtoConstructionName = constructionFinishedConstructionName news
                                    , constructionFinishedNewsDtoBuildingId = constructionFinishedBuildingId news
                                    , constructionFinishedNewsDtoShipId = constructionFinishedShipId news
                                    , constructionFinishedNewsDtoDate = constructionFinishedDate news
                                    }


instance FromDto ConstructionFinishedNews ConstructionFinishedNewsDto where
    fromDto dto =
        ConstructionFinishedNews { constructionFinishedNewsPlanetName = constructionFinishedNewsDtoPlanetName dto
                                 , constructionFinishedNewsPlanetId = constructionFinishedNewsDtoPlanetId dto
                                 , constructionFinishedNewsSystemName = constructionFinishedNewsDtoSystemName dto
                                 , constructionFinishedNewsSystemId = constructionFinishedNewsDtoSystemId dto
                                 , constructionFinishedConstructionName = constructionFinishedNewsDtoConstructionName dto
                                 , constructionFinishedBuildingId = constructionFinishedNewsDtoBuildingId dto
                                 , constructionFinishedShipId = constructionFinishedNewsDtoShipId dto
                                 , constructionFinishedDate = constructionFinishedNewsDtoDate dto
                                 }


instance ToDto ((Key News, NewsArticle), (IconMapper NewsArticleDto)) NewsDto where
    toDto ((nId, article), icons) =
        let
            content = toDto article
        in
        NewsDto { newsDtoId = nId
                , newsContents = content
                , newsIcon = runIconMapper icons content
                }


instance FromDto NewsArticle NewsArticleDto where
    fromDto dto =
        case dto of
            StarFoundDto content ->
                StarFound $ fromDto content

            PlanetFoundDto content ->
                PlanetFound $ fromDto content

            UserWrittenDto content ->
                UserWritten $ fromDto content

            DesignCreatedDto content ->
                DesignCreated $ fromDto content

            ConstructionFinishedDto content ->
                ConstructionFinished $ fromDto content

            KragiiDto content ->
                KragiiResolution $ fromDto content

            SpecialDto content ->
                Special $ fromDto content


instance ToDto NewsArticle NewsArticleDto where
    toDto news =
        case news of
            (StarFound x) -> StarFoundDto $ toDto x
            (PlanetFound x) -> PlanetFoundDto $ toDto x
            (UserWritten x) -> UserWrittenDto $ toDto x
            (DesignCreated x) -> DesignCreatedDto $ toDto x
            (ConstructionFinished x) -> ConstructionFinishedDto $ toDto x
            (KragiiResolution x) -> KragiiDto $ toDto x
            (Special x) -> SpecialDto $ toDto x


-- | Special news that require player interaction
data SpecialNews = KragiiWorms KragiiWormsEvent [UserOption KragiiWormsChoice] (Maybe KragiiWormsChoice)
    deriving (Show, Read, Eq)


instance ToDto SpecialNews SpecialNewsDto where
    toDto (KragiiWorms event options choice) =
        KragiiEventDto $ KragiiWormsEventDto
            { kragiiWormsDtoPlanetId = kragiiWormsPlanetId event
            , kragiiWormsDtoPlanetName = kragiiWormsPlanetName event
            , kragiiWormsDtoSystemId = kragiiWormsSystemId event
            , kragiiWormsDtoSystemName = kragiiWormsSystemName event
            , kragiiWormsDtoOptions = fmap toDto options
            , kragiiWormsDtoChoice = fmap toDto choice
            , kragiiWormsDtoDate = kragiiWormsDate event
            }


instance FromDto SpecialNews SpecialNewsDto where
    fromDto (KragiiEventDto dto) =
        KragiiWorms ( KragiiWormsEvent
                        { kragiiWormsPlanetId = kragiiWormsDtoPlanetId dto
                        , kragiiWormsPlanetName = kragiiWormsDtoPlanetName dto
                        , kragiiWormsSystemId = kragiiWormsDtoSystemId dto
                        , kragiiWormsSystemName = kragiiWormsDtoSystemName dto
                        , kragiiWormsDate = kragiiWormsDtoDate dto
                        } )
                    []
                    (fmap fromDto $ kragiiWormsDtoChoice dto)


-- | Icon for user created news
data UserNewsIcon =
    GenericUserNews
    | JubilationUserNews
    | CatUserNews
    deriving (Show, Read, Eq)


instance ToDto UserNewsIcon UserNewsIconDto where
    toDto icon =
        case icon of
            GenericUserNews ->
                GenericUserNewsDto

            JubilationUserNews ->
                JubilationUserNewsDto

            CatUserNews ->
                CatUserNewsDto


instance FromDto UserNewsIcon UserNewsIconDto where
    fromDto icon =
        case icon of
            GenericUserNewsDto ->
                GenericUserNews

            JubilationUserNewsDto ->
                JubilationUserNews

            CatUserNewsDto ->
                CatUserNews


-- | Helper function for creating News that aren't special events and haven't been dismissed
mkNews :: Key Faction -> Time -> NewsArticle -> News
mkNews fId date content =
    News { newsContent = toStrict $ encodeToLazyText content
         , newsFactionId = fId
         , newsDate = timeCurrentTime date
         , newsDismissed = False
         , newsSpecialEvent = NoSpecialEvent
         }


-- | Helper function for creating News that are special events and haven't been handled
mkSpecialNews :: Time -> Key Faction -> SpecialNews -> News
mkSpecialNews date fId content =
    News { newsContent = toStrict $ encodeToLazyText $ Special content
         , newsFactionId = fId
         , newsDate = timeCurrentTime date
         , newsDismissed = False
         , newsSpecialEvent = UnhandledSpecialEvent
         }


$(deriveJSON defaultOptions ''StarFoundNews)
$(deriveJSON defaultOptions ''PlanetFoundNews)
$(deriveJSON defaultOptions ''UserWrittenNews)
$(deriveJSON defaultOptions ''DesignCreatedNews)
$(deriveJSON defaultOptions ''ConstructionFinishedNews)
$(deriveJSON defaultOptions ''UserNewsIcon)
$(deriveJSON defaultOptions ''SpecialNews)
$(deriveJSON defaultOptions ''NewsArticle)
