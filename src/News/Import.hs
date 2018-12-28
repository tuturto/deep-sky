{-# LANGUAGE NoImplicitPrelude     #-}

module News.Import ( parseNewsEntities, userWrittenNews
                   , planetFoundNews, starFoundNews, designCreatedNews
                   , buildingConstructionFinishedNews, iconMapper, userNewsIconMapper
                   , iconInfo )
    where

import Import
import Data.Aeson ( decode )
import Data.ByteString.Builder( toLazyByteString )
import Data.Maybe ( isJust, fromJust )
import Data.Text.Encoding ( encodeUtf8Builder )
import Buildings ( building, BLevel(..), BuildingInfo(..) )
import Dto.News ( NewsArticleDto(..), UserWrittenNewsDto(..), IconMapper(..), UserNewsIconDto(..)
                , SpecialNewsDto(..)
                )
import Events.Import ( eventOptions )
import News.Data ( NewsArticle(..), ConstructionFinishedNews(..), DesignCreatedNews(..)
                 , PlanetFoundNews(..), StarFoundNews(..), UserWrittenNews(..)
                 , UserNewsIcon(..), SpecialNews(..), mkNews
                 )


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
        addOptions (key, article) = case article of
                                        Special news ->
                                            (key, Special $ availableOptions news)
                                        _ ->
                                            (key, article)
    in
        fmap (addOptions . simplify) $ filter removeFailed parsed


-- | Evaluation current situation in respect to an special event and add available options
availableOptions :: SpecialNews -> SpecialNews
availableOptions x =
    case x of
        KragiiWorms event _ choice ->
            KragiiWorms event (eventOptions event) choice


-- | Use passed url render function to return link to news article's icon
-- This function is useful for example when returning JSON data to client
-- and supplying link to icon that should be displayed for it.
iconMapper :: (Route App -> Text) -> IconMapper UserNewsIconDto -> IconMapper NewsArticleDto
iconMapper render userIconMapper =
    IconMapper $ \article ->
        case article of
            StarFoundDto _ ->
                render $ StaticR images_news_sun_png

            PlanetFoundDto _->
                render $ StaticR images_news_planet_png

            UserWrittenDto details ->
                runIconMapper userIconMapper $ userWrittenNewsDtoIcon details

            DesignCreatedDto _ ->
                render $ StaticR images_news_blueprint_png

            ConstructionFinishedDto _ ->
                render $ StaticR images_news_crane_png

            SpecialDto (KragiiEventDto _) ->
                render $ StaticR images_news_hydra_png

            KragiiDto _ ->
                render $ StaticR images_news_hydra_png


-- | Get url to image corresponding to icon selection in user news
userNewsIconMapper :: (Route App -> Text) -> IconMapper UserNewsIconDto
userNewsIconMapper render =
    IconMapper $ \icon ->
        case icon of
            GenericUserNewsDto ->
                render $ StaticR images_news_question_png

            JubilationUserNewsDto ->
                render $ StaticR images_news_jubileum_png

            CatUserNewsDto ->
                render $ StaticR images_news_cat_png


-- | List of tuples for all user news icon dtos, containing dto and link to
-- resource that can be used to retrieve image corresponding to dto
iconInfo :: IconMapper UserNewsIconDto -> [(UserNewsIconDto, Text)]
iconInfo mapper =
    map (\x -> (x, runIconMapper mapper x)) $ enumFrom minBound


-- | Construct news entry for user submitted news
userWrittenNews :: Text -> UserNewsIcon -> Time -> User -> News
userWrittenNews msg icon date user =
    let
        content = UserWritten $ UserWrittenNews msg icon (timeCurrentTime date) (userIdent user)
    in
        mkNews (fromJust $ userFactionId user) date content


-- | Construct news entry for discovery of new planet
planetFoundNews :: Entity Planet -> StarSystem -> Time -> Key Faction -> News
planetFoundNews planetEnt system date fId =
    let
        planet = entityVal planetEnt
        planetKey = entityKey planetEnt
        content = PlanetFound $ PlanetFoundNews (planetName planet) (starSystemName system) (planetStarSystemId planet) planetKey (timeCurrentTime date)
    in
        mkNews fId date content


-- | Construct news entry for discovery of new star
starFoundNews :: Star -> Entity StarSystem -> Time -> Key Faction -> News
starFoundNews star systemEnt date fId =
    let
        system = entityVal systemEnt
        systemId = entityKey systemEnt
        content = StarFound $ StarFoundNews (starName star) (starSystemName system) systemId (timeCurrentTime date)
    in
        mkNews fId date content


-- | Construct news entry for creation of new space ship desgin
designCreatedNews :: Entity Design -> Time -> Key Faction -> News
designCreatedNews design date fId =
    let
        dId = entityKey design
        name = designName $ entityVal design
        content = DesignCreated $ DesignCreatedNews dId name (timeCurrentTime date)
    in
        mkNews fId date content


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
        mkNews fId date content
