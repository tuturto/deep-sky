{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module News ( NewsArticle(..), parseNews )
    where

import Import
import Database.Persist.Sql (toSqlKey)
import Data.Aeson.TH 
import Data.Aeson (decode)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder(toLazyByteString)

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

parseNews :: News -> Maybe NewsArticle
parseNews entry =
    (decode . toLazyByteString . encodeUtf8Builder . newsContent) entry

$(deriveJSON defaultOptions ''NewsArticle)
