{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module News ( NewsArticle(..), parseNews, parseNewsEntity, parseNewsEntities
            , makeUserWrittenNews, makePlanetFoundNews, makeStarFoundNews
            , makeDesignCreatedNews, buildingConstructionFinishedNews
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

makeUserWrittenNews :: Text -> UserNewsIcon -> Time -> User -> News
makeUserWrittenNews msg icon date user =
    let
        content = UserWrittenNews msg icon (timeCurrentTime date) (userIdent user) 
    in
        News (toStrict $ encodeToLazyText content) (fromJust $ userFactionId user) (timeCurrentTime date) False

makePlanetFoundNews :: Entity Planet -> StarSystem -> Time -> Key Faction -> News
makePlanetFoundNews planetEnt system date fId =
    let
        planet = entityVal planetEnt
        planetKey = entityKey planetEnt
        content = PlanetFoundNews (planetName planet) (starSystemName system) (planetStarSystemId planet) planetKey (timeCurrentTime date)
    in
        News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False

makeStarFoundNews :: Star -> Entity StarSystem -> Time -> Key Faction -> News
makeStarFoundNews star systemEnt date fId =
    let
        system = entityVal systemEnt
        systemId = entityKey systemEnt
        content = StarFoundNews (starName star) (starSystemName system) systemId (timeCurrentTime date)
    in
        News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False

makeDesignCreatedNews :: Entity Design -> Time -> Key Faction -> News
makeDesignCreatedNews design date fId =
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
