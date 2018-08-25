{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Messages where

import Import
import Database.Persist.Sql (toSqlKey)
import Widgets (newsArticleWidget)
import News (parseNews)
import Data.Maybe (isJust, fromJust)

getMessageListR :: Handler Html
getMessageListR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    let loadedNews = loadNewsEntries
    let entries = map fromJust $ filter isJust $ map parseNews loadedNews    
    defaultLayout $ do
        addStylesheet $ StaticR css_site_css
        setTitle "Deep Sky - Messages"
        $(widgetFile "messages")

loadNewsEntries :: [News]
loadNewsEntries =
    [ News "{\"tag\":\"StarFoundNews\",\"starFoundNewsSystemName\":\"Sol\",\"starFoundNewsSystemId\":1,\"starFoundNewsStarName\":\"Sun\",\"starFoundNewsDate\":20199}" (toSqlKey 1) 20199 True
    , News "{\"tag\":\"PlanetFoundNews\",\"planetFoundNewsPlanetName\":\"Mercury\",\"planetFoundNewsSystemName\":\"Sol\",\"planetFoundNewsSystemId\":1,\"planetFoundNewsPlanetId\":1,\"planetFoundNewsDate\":20199}" (toSqlKey 1) 20199 True
    ]
