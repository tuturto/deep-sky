{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Messages where

import Import
import Widgets (newsArticleWidget)
import News (parseNewsEntities)

getMessageListR :: Handler Html
getMessageListR = do
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    loadedNews <- runDB $ loadNewsEntries fId
    let entries = parseNewsEntities loadedNews
    defaultLayout $ do
        addStylesheet $ StaticR css_site_css
        setTitle "Deep Sky - Messages"
        $(widgetFile "messages")
 
getMessageDeleteR :: Key News -> Handler Html
getMessageDeleteR nId = do
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    _ <- runDB $ update nId [ NewsDismissed =. True ]
    loadedNews <- runDB $ loadNewsEntries fId
    let entries = parseNewsEntities loadedNews
    defaultLayout $ do
        addStylesheet $ StaticR css_site_css
        setTitle "Deep Sky - Messages"
        $(widgetFile "messages")    

loadNewsEntries :: (PersistQueryRead backend, MonadIO m,
    BaseBackend backend ~ SqlBackend) =>
    Key Faction -> ReaderT backend m [Entity News]
loadNewsEntries fId = do
    selectList [ NewsFactionId ==. fId 
               , NewsDismissed ==. False ] [ Desc NewsDate ]
