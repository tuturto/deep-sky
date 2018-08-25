{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module News ( NewsArticle(..), parseNews )
    where

import Import
import Database.Persist.Sql (toSqlKey)

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

parseNews :: News -> NewsArticle
parseNews entry =
    if (newsDismissed entry)
    then
        StarFoundNews "Sun" "Sol" (toSqlKey 1) 20199
    else
        PlanetFoundNews "Mercury" "Sol" (toSqlKey 1) (toSqlKey 1) 20199
