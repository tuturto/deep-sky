{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE LambdaCase            #-}

module News.Import ( parseNewsEntities, userWrittenNews
                   , planetFoundNews, starFoundNews, designCreatedNews
                   , buildingConstructionFinishedNews, iconMapper, userNewsIconMapper
                   , productionChangeEndedIconMapper, iconInfo, productionBoostStartedNews
                   , productionSlowdownStartedNews, researchCompleted )
    where

import Import
import Data.Aeson ( decode )
import Data.ByteString.Builder( toLazyByteString )
import Data.Maybe ( isJust, fromJust )
import Data.Text.Encoding ( encodeUtf8Builder )
import Buildings ( building, BLevel(..), BuildingInfo(..) )
import CustomTypes ( StarDate )
import Dto.Icons ( IconMapper(..) )
import Dto.News ( NewsArticleDto(..), UserWrittenNewsDto(..), UserNewsIconDto(..)
                , SpecialNewsDto(..), ProductionChangedNewsDto(..)
                )
import Events.Import ( eventOptions )
import News.Data ( NewsArticle(..), ConstructionFinishedNews(..), DesignCreatedNews(..)
                 , PlanetFoundNews(..), StarFoundNews(..), UserWrittenNews(..)
                 , UserNewsIcon(..), SpecialNews(..), ProductionChangedNews(..)
                 , ResearchCompletedNews(..), mkNews
                 )
import Research.Data ( Technology )
import Resources ( ResourceType(..) )


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
        addOptions . simplify <$> filter removeFailed parsed


-- | Evaluation current situation in respect to an special event and add available options
availableOptions :: SpecialNews -> SpecialNews
availableOptions x =
    case x of
        KragiiWorms event _ choice ->
            KragiiWorms event (eventOptions event) choice


-- | Use passed url render function to return link to news article's icon
-- This function is useful for example when returning JSON data to client
-- and supplying link to icon that should be displayed for it.
iconMapper :: (Route App -> Text) -> IconMapper UserNewsIconDto
    -> IconMapper ProductionChangedNewsDto
    -> IconMapper NewsArticleDto
iconMapper render userIconMapper changeIconMapper =
    IconMapper $ \case
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

            ProductionBoostStartedDto details ->
                case productionChangedNewsDtoType details of
                    BiologicalResource ->
                        render $ StaticR images_statuses_wheat_up_png

                    MechanicalResource ->
                        render $ StaticR images_statuses_cog_up_png

                    ChemicalResource ->
                        render $ StaticR images_statuses_droplets_up_png

            ProductionSlowdownStartedDto details ->
                case productionChangedNewsDtoType details of
                    BiologicalResource ->
                        render $ StaticR images_statuses_wheat_down_png

                    MechanicalResource ->
                        render $ StaticR images_statuses_cog_down_png

                    ChemicalResource ->
                        render $ StaticR images_statuses_droplets_down_png

            ProductionBoostEndedDto details ->
                runIconMapper changeIconMapper details

            ProductionSlowdownEndedDto details ->
                runIconMapper changeIconMapper details

            ResearchCompletedDto _ ->
                render $ StaticR images_news_microscope_png

            SpecialDto (KragiiEventDto _) ->
                render $ StaticR images_news_hydra_png

            KragiiDto _ ->
                render $ StaticR images_news_hydra_png


-- | Get url to image corresponding to icon selection in user news
userNewsIconMapper :: (Route App -> Text) -> IconMapper UserNewsIconDto
userNewsIconMapper render =
    IconMapper $ \case
            GenericUserNewsDto ->
                render $ StaticR images_news_question_png

            JubilationUserNewsDto ->
                render $ StaticR images_news_jubileum_png

            CatUserNewsDto ->
                render $ StaticR images_news_cat_png


productionChangeEndedIconMapper :: (Route App -> Text) -> IconMapper ProductionChangedNewsDto
productionChangeEndedIconMapper render =
    IconMapper $ \dto ->
        case productionChangedNewsDtoType dto of
            BiologicalResource ->
                render $ StaticR images_statuses_wheat_right_png

            MechanicalResource ->
                render $ StaticR images_statuses_cog_right_png

            ChemicalResource ->
                render $ StaticR images_statuses_droplets_right_png


-- | List of tuples for all user news icon dtos, containing dto and link to
-- resource that can be used to retrieve image corresponding to dto
iconInfo :: IconMapper UserNewsIconDto -> [(UserNewsIconDto, Text)]
iconInfo mapper =
    map (\x -> (x, runIconMapper mapper x)) $ enumFrom minBound


-- | Construct news entry for user submitted news
userWrittenNews :: Text -> UserNewsIcon -> StarDate -> User -> News
userWrittenNews msg icon date user =
    let
        content = UserWritten $ UserWrittenNews msg icon date (userIdent user)
    in
        mkNews (fromJust $ userFactionId user) date content


-- | Construct news entry for discovery of new planet
planetFoundNews :: Entity Planet -> StarSystem -> StarDate -> Key Faction -> News
planetFoundNews planetEnt system date fId =
    let
        planet = entityVal planetEnt
        planetKey = entityKey planetEnt
        content = PlanetFound $ PlanetFoundNews (planetName planet) (starSystemName system) (planetStarSystemId planet) planetKey date
    in
        mkNews fId date content


-- | Construct news entry for discovery of new star
starFoundNews :: Star -> Entity StarSystem -> StarDate -> Key Faction -> News
starFoundNews star systemEnt date fId =
    let
        system = entityVal systemEnt
        systemId = entityKey systemEnt
        content = StarFound $ StarFoundNews (starName star) (starSystemName system) systemId date
    in
        mkNews fId date content


-- | Construct news entry for creation of new space ship desgin
designCreatedNews :: Entity Design -> StarDate -> Key Faction -> News
designCreatedNews design date fId =
    let
        dId = entityKey design
        name = designName $ entityVal design
        content = DesignCreated $ DesignCreatedNews dId name date
    in
        mkNews fId date content


-- | Construct news entry for a finished building construction
buildingConstructionFinishedNews :: Entity Planet -> Entity StarSystem -> Entity Building -> StarDate -> Key Faction -> News
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
                    , constructionFinishedDate = date
                    }
    in
        mkNews fId date content


productionBoostStartedNews :: Entity Planet -> Entity StarSystem -> ResourceType -> StarDate -> Key Faction -> News
productionBoostStartedNews planet system rType date fId =
    let
        content = ProductionBoostStarted $ productionChanged planet system rType date
    in
        mkNews fId date content


productionSlowdownStartedNews :: Entity Planet -> Entity StarSystem -> ResourceType -> StarDate -> Key Faction -> News
productionSlowdownStartedNews planet system rType date fId =
    let
        content = ProductionSlowdownStarted $ productionChanged planet system rType date
    in
        mkNews fId date content


productionChanged :: Entity Planet -> Entity StarSystem -> ResourceType -> StarDate -> ProductionChangedNews
productionChanged planet system rType date =
    ProductionChangedNews
        { productionChangedNewsPlanetId = entityKey planet
        , productionChangedNewsPlanetName = (planetName . entityVal) planet
        , productionChangedNewsSystemId = entityKey system
        , productionChangedNewsSystemName = (starSystemName . entityVal) system
        , productionChangedNewsType = rType
        , productionChangedNewsDate = date
        }

researchCompleted :: StarDate -> Key Faction -> Technology -> News
researchCompleted date fId tech =
    let
        content = ResearchCompleted $ ResearchCompletedNews
                    { researchCompletedNewsTechnology = tech
                    , researchCompletedNewsData = date
                    }
    in
        mkNews fId date content
