{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module News ( NewsArticle(..)
            , parseNews, parseNewsEntity, parseNewsEntities
            , userWrittenNews, planetFoundNews, starFoundNews
            , designCreatedNews, buildingConstructionFinishedNews
            , UserNewsIcon(..) )
    where

import Import
import Data.Aeson.TH 
import Data.Aeson (decode)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder(toLazyByteString)
import Data.Maybe (isJust, fromJust)
import Data.Aeson.Text (encodeToLazyText)
import Buildings ( building, BLevel(..), BuildingInfo(..) )
import Yesod.Form.Bootstrap3

data NewsArticle = 
    StarFoundNews
        { starFoundNewsStarName :: Text
        , starFoundNewsSystemName :: Text
        , starFoundNewsSystemId :: Key StarSystem
        , starFoundNewsDate :: Int
        }
    | PlanetFoundNews
        { planetFoundNewsPlanetName :: Text
        , planetFoundNewsSystemName :: Text
        , planetFoundNewsSystemId :: Key StarSystem
        , planetFoundNewsPlanetId :: Key Planet
        , planetFoundNewsDate :: Int
        }
    | UserWrittenNews
        { userWrittenNewsContent :: Text
        , userWrittenNewsIcon :: UserNewsIcon
        , userWrittenNewsDate :: Int
        , userWrittenNewsUser :: Text
        }
    | DesignCreatedNews
        { designCreatedNewsDesignId :: Key Design
        , designCreatedNewsName :: Text
        , designCreatedDate :: Int
        }
    | ConstructionFinishedNews
        { constructionFinishedNewsPlanetName :: Maybe Text
        , constructionFinishedNewsPlanetId :: Maybe (Key Planet)
        , constructionFinishedNewsSystemName :: Text
        , constructionFinishedNewsSystemId :: Key StarSystem
        , constructionFinishedConstructionName :: Text
        , constructionFinishedBuildingId :: Maybe (Key Building)
        , constructionFinishedShipId :: Maybe (Key Ship)
        , constructionFinishedDate :: Int
        }
    | KragiiWormsNews
        { kragiiWormsNewsPlanetName :: Maybe Text
        , kragiiWormsNewsPlanetId :: Maybe (Key Planet)
        , kragiiWormsDate :: Int
        }
    | CropsNews
        { cropsNewsPlanetName :: Maybe Text
        , cropsDate :: Int
        }

data NewsForm = KragiiWormsNewsForm KragiiWormsNewsOption
    | CropsNewsForm CropsNewsOption
    deriving (Show, Read, Eq)

data KragiiWormsNewsOption = AvoidKragiiWorms
    | AttackKragiiWorms
    deriving (Show, Read, Eq)

data CropsNewsOption = HarvestCrops
    | WaitCropsToGrow
    deriving (Show, Read, Eq)

newsAForm :: NewsArticle -> Maybe (AForm Handler NewsForm)
newsAForm KragiiWormsNews {} = Just $ KragiiWormsNewsForm
    <$> areq (radioFieldList entries) "Choice: " Nothing
    where
        entries :: [(Text, KragiiWormsNewsOption)]
        entries = [ ("Avoid kragii worms", AvoidKragiiWorms)
                  , ("Attack kragii worms", AttackKragiiWorms)
                  ]
newsAForm CropsNews {} = Just $ CropsNewsForm
    <$> areq (radioFieldList entries) "Choice: " Nothing
    where
        entries :: [(Text, CropsNewsOption)]
        entries = [ ("Harvest crops", HarvestCrops)
                  , ("Wait crops to grow more", WaitCropsToGrow)
                  ]
newsAForm _ = Nothing

data UserNewsIcon =
    GenericUserNews
    | JubilationUserNews
    | CatUserNews
    deriving (Show, Read, Eq)

parseNews :: News -> Maybe NewsArticle
parseNews =
    decode . toLazyByteString . encodeUtf8Builder . newsContent

parseNewsEntity :: Entity News -> (Key News, Maybe NewsArticle)
parseNewsEntity entity =
    let
        nId = entityKey entity
        news = entityVal entity
    in
        (nId, parseNews news)

parseNewsEntities :: [Entity News] -> [(Key News, NewsArticle)]
parseNewsEntities entities =
    let
        parsed = map parseNewsEntity entities
        removeFailed (_ , article) = isJust article
        simplify (key, article) = (key, fromJust article)
    in
        map simplify $ filter removeFailed parsed

$(deriveJSON defaultOptions ''UserNewsIcon)
$(deriveJSON defaultOptions ''NewsArticle)

userWrittenNews :: Text -> UserNewsIcon -> Time -> User -> News
userWrittenNews msg icon date user =
    let
        content = UserWrittenNews msg icon (timeCurrentTime date) (userIdent user) 
    in
        News (toStrict $ encodeToLazyText content) (fromJust $ userFactionId user) (timeCurrentTime date) False

planetFoundNews :: Entity Planet -> StarSystem -> Time -> Key Faction -> News
planetFoundNews planetEnt system date fId =
    let
        planet = entityVal planetEnt
        planetKey = entityKey planetEnt
        content = PlanetFoundNews (planetName planet) (starSystemName system) (planetStarSystemId planet) planetKey (timeCurrentTime date)
    in
        News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False

starFoundNews :: Star -> Entity StarSystem -> Time -> Key Faction -> News
starFoundNews star systemEnt date fId =
    let
        system = entityVal systemEnt
        systemId = entityKey systemEnt
        content = StarFoundNews (starName star) (starSystemName system) systemId (timeCurrentTime date)
    in
        News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False

designCreatedNews :: Entity Design -> Time -> Key Faction -> News
designCreatedNews design date fId =
    let
        dId = entityKey design
        name = designName $ entityVal design
        content = DesignCreatedNews dId name (timeCurrentTime date)
    in
        News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False

-- | Construct news entry for a finished building construction
buildingConstructionFinishedNews :: Entity Planet -> Entity StarSystem -> Entity Building -> Time -> Key Faction -> News
buildingConstructionFinishedNews planetE starSystemE buildingE date fId =
    let
        modelBuilding = building (buildingType $ entityVal buildingE) (BLevel $ buildingLevel $ entityVal buildingE)
        content = ConstructionFinishedNews
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
