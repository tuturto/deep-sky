{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module News ( NewsArticle(..), parseNews, parseNewsEntity, parseNewsEntities
            , UserNewsIcon(..) )
    where

import Import
import Data.Aeson.TH 
import Data.Aeson (decode)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder(toLazyByteString)
import Data.Maybe (isJust, fromJust)

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
