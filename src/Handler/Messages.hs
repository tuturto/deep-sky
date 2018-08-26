{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Messages where

import Import
import Widgets (newsArticleWidget)
import News (parseNews)
import Common (filterMap)

getMessageListR :: Handler Html
getMessageListR = do
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    loadedNews <- runDB $ loadNewsEntries fId
    let entries = filterMap parseNews $ map entityVal loadedNews    
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
