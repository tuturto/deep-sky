{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Messages where

import Import
import Database.Persist.Sql (toSqlKey)
import Widgets (newsArticleWidget)
import News

getMessageListR :: Handler Html
getMessageListR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    let loadedNews = loadNewsEntries
    let entries = map parseNews loadedNews
    defaultLayout $ do
        setTitle "Deep Sky - Messages"
        $(widgetFile "messages")

loadNewsEntries :: [News]
loadNewsEntries =
    [ News "{ type: \"star found\", starName: \"Sun\", systemName: \"Sol\", systemId: 1, date: 20199}" (toSqlKey 1) 20199 True
    , News "{ type: \"planet found\", planetName: \"Mercury\", systemName: \"Sol\", systemId: 1, planetId: 1, date: 20199}" (toSqlKey 1) 20199 False
    ]
 