{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module News ( NewsArticle(..), parseNews, parseNewsEntity, parseNewsEntities
            , makeUserWrittenNews, makePlanetFoundNews, makeStarFoundNews,
            UserNewsIcon(..) )
    where

import Import
import Data.Aeson.TH 
import Data.Aeson (decode)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder(toLazyByteString)
import Data.Maybe (isJust, fromJust)
import Data.Aeson.Text (encodeToLazyText)

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

data UserNewsIcon =
    GenericUserNews
    | JubilationUserNews
    | CatUserNews
    deriving (Show, Read, Eq)

parseNews :: News -> Maybe NewsArticle
parseNews entry =
    (decode . toLazyByteString . encodeUtf8Builder . newsContent) entry

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

makePlanetFoundNews :: (Entity Planet) -> StarSystem -> Time -> (Entity Faction) -> News
makePlanetFoundNews planetEnt system date facEnt =
    let
        planet = entityVal planetEnt
        planetKey = entityKey planetEnt
        fId = entityKey facEnt
        content = PlanetFoundNews (planetName planet) (starSystemName system) (planetStarSystemId planet) planetKey (timeCurrentTime date)
    in
        News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False

makeStarFoundNews :: (Entity Star) -> (Entity StarSystem) -> Time -> (Entity Faction) -> News
makeStarFoundNews starEnt systemEnt date facEnt =
    let
        star = entityVal starEnt
        system = entityVal systemEnt
        systemId = entityKey systemEnt
        fId = entityKey facEnt
        content = StarFoundNews (starName star) (starSystemName system) systemId (timeCurrentTime date)
    in
        News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False
