{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}

module News ( NewsArticle(..)
            , parseNews, parseNewsEntity, parseNewsEntities
            , userWrittenNews, planetFoundNews, starFoundNews
            , designCreatedNews, buildingConstructionFinishedNews
            , UserNewsIcon(..)
            , StarFoundNews(..), PlanetFoundNews(..), UserWrittenNews(..)
            , DesignCreatedNews(..), ConstructionFinishedNews(..), iconMapper
            , iconInfo )
    where

import Import
import Data.Aeson.TH
import Data.Aeson (decode)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder(toLazyByteString)
import Data.Maybe (isJust, fromJust)
import Data.Aeson.Text (encodeToLazyText)
import Buildings ( building, BLevel(..), BuildingInfo(..) )
import Events ( KragiiWormsEvent(..) )
import Common ( ToDto(..), FromDto(..) )
import Dto.News ( NewsDto(..), NewsArticleDto(..), StarFoundNewsDto(..), PlanetFoundNewsDto(..)
                , UserWrittenNewsDto(..), DesignCreatedNewsDto(..), ConstructionFinishedNewsDto(..)
                , IconMapper(..), UserNewsIconDto(..) )


-- | Use passed url render function to return link to news article's icon
-- This function is useful for example when returning JSON data to client
-- and supplying link to icon that should be displayed for it.
iconMapper :: (Route App -> Text) -> IconMapper
iconMapper render =
    IconMapper $ \article ->
        case article of
            StarFoundDto _ ->
                render $ StaticR images_news_sun_png

            PlanetFoundDto _->
                render $ StaticR images_news_planet_png

            UserWrittenDto details ->
                userNewsIcon render $ userWrittenNewsDtoIcon details

            DesignCreatedDto _ ->
                render $ StaticR images_news_blueprint_png

            ConstructionFinishedDto _ ->
                render $ StaticR images_news_crane_png


-- | Get url to image corresponding to icon selection in user news
userNewsIcon :: (Route App -> Text) -> UserNewsIconDto -> Text
userNewsIcon render icon =
    case icon of
        GenericUserNewsDto ->
            render $ StaticR images_news_question_png

        JubilationUserNewsDto ->
            render $ StaticR images_news_jubileum_png

        CatUserNewsDto ->
            render $ StaticR images_news_cat_png


-- | List of tuples for all user news icon dtos, containing dto and link to
-- resource that can be used to retrieve image corresponding to dto
iconInfo :: (Route App -> Text) -> [(UserNewsIconDto, Text)]
iconInfo render =
    map (\x -> (x, userNewsIcon render x)) $ enumFrom minBound


-- | All possible news articles
data NewsArticle =
    StarFound StarFoundNews
    | PlanetFound PlanetFoundNews
    | UserWritten UserWrittenNews
    | DesignCreated DesignCreatedNews
    | ConstructionFinished ConstructionFinishedNews


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


instance ToDto ((Key News, NewsArticle), IconMapper) NewsDto where
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


instance ToDto NewsArticle NewsArticleDto where
    toDto news =
        case news of
            (StarFound x) -> StarFoundDto $ toDto x
            (PlanetFound x) -> PlanetFoundDto $ toDto x
            (UserWritten x) -> UserWrittenDto $ toDto x
            (DesignCreated x) -> DesignCreatedDto $ toDto x
            (ConstructionFinished x) -> ConstructionFinishedDto $ toDto x


-- | Special news that require player interaction
data SpecialNews = KragiiWorms KragiiWormsEvent
    deriving (Show, Read, Eq)


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


-- | Parse database entry of news and construct a possible news article
parseNews :: News -> Maybe NewsArticle
parseNews =
    decode . toLazyByteString . encodeUtf8Builder . newsContent


-- | Given a news entity, parse that into a tuple of key and possible news article
parseNewsEntity :: Entity News -> (Key News, Maybe NewsArticle)
parseNewsEntity entity =
    let
        nId = entityKey entity
        news = entityVal entity
    in
        (nId, parseNews news)


-- | Given a list of news entities, parse them into a list of tuples of key and possible news article
-- Entries that failed to parse are removed from the end result
parseNewsEntities :: [Entity News] -> [(Key News, NewsArticle)]
parseNewsEntities entities =
    let
        parsed = map parseNewsEntity entities
        removeFailed (_ , article) = isJust article
        simplify (key, article) = (key, fromJust article)
    in
        map simplify $ filter removeFailed parsed


-- | Construct news entry for user submitted news
userWrittenNews :: Text -> UserNewsIcon -> Time -> User -> News
userWrittenNews msg icon date user =
    let
        content = UserWritten $ UserWrittenNews msg icon (timeCurrentTime date) (userIdent user)
    in
        News (toStrict $ encodeToLazyText content) (fromJust $ userFactionId user) (timeCurrentTime date) False


-- | Construct news entry for discovery of new planet
planetFoundNews :: Entity Planet -> StarSystem -> Time -> Key Faction -> News
planetFoundNews planetEnt system date fId =
    let
        planet = entityVal planetEnt
        planetKey = entityKey planetEnt
        content = PlanetFound $ PlanetFoundNews (planetName planet) (starSystemName system) (planetStarSystemId planet) planetKey (timeCurrentTime date)
    in
        News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False


-- | Construct news entry for discovery of new star
starFoundNews :: Star -> Entity StarSystem -> Time -> Key Faction -> News
starFoundNews star systemEnt date fId =
    let
        system = entityVal systemEnt
        systemId = entityKey systemEnt
        content = StarFound $ StarFoundNews (starName star) (starSystemName system) systemId (timeCurrentTime date)
    in
        News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False


-- | Construct news entry for creation of new space ship desgin
designCreatedNews :: Entity Design -> Time -> Key Faction -> News
designCreatedNews design date fId =
    let
        dId = entityKey design
        name = designName $ entityVal design
        content = DesignCreated $ DesignCreatedNews dId name (timeCurrentTime date)
    in
        News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False


-- | Construct news entry for a finished building construction
buildingConstructionFinishedNews :: Entity Planet -> Entity StarSystem -> Entity Building -> Time -> Key Faction -> News
buildingConstructionFinishedNews planetE starSystemE buildingE date fId =
    let
        modelBuilding = building (buildingType $ entityVal buildingE) (BLevel $ buildingLevel $ entityVal buildingE)
        content = ConstructionFinished $ ConstructionFinishedNews
                    { constructionFinishedNewsPlanetName = (Just . planetName . entityVal) planetE
                    , constructionFinishedNewsPlanetId = Just $ entityKey planetE
                    , constructionFinishedNewsSystemName = (starSystemName . entityVal) starSystemE
                    , constructionFinishedNewsSystemId = entityKey starSystemE
                    , constructionFinishedConstructionName = buildingInfoName modelBuilding
                    , constructionFinishedBuildingId = Just $ entityKey buildingE
                    , constructionFinishedShipId = Nothing
                    , constructionFinishedDate = timeCurrentTime date
                    }
    in
        News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False


$(deriveJSON defaultOptions ''StarFoundNews)
$(deriveJSON defaultOptions ''PlanetFoundNews)
$(deriveJSON defaultOptions ''UserWrittenNews)
$(deriveJSON defaultOptions ''DesignCreatedNews)
$(deriveJSON defaultOptions ''ConstructionFinishedNews)
$(deriveJSON defaultOptions ''UserNewsIcon)
$(deriveJSON defaultOptions ''SpecialNews)
$(deriveJSON defaultOptions ''NewsArticle)
