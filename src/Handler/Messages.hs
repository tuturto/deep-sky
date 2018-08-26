{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Messages where

import Import
import Widgets (newsArticleWidget)
import News (parseNewsEntities, UserNewsIcon(..), NewsArticle(UserWrittenNews))
import Yesod.Form.Bootstrap3
import MenuHelpers (starDate)
import Data.Aeson.Text (encodeToLazyText)

getMessageListR :: Handler Html
getMessageListR = do
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    loadedNews <- runDB $ loadNewsEntries fId
    let entries = parseNewsEntities loadedNews
    let currentPage = (1 :: Int)
    let totalPages = (1 :: Int)
    let totalUnread = (2 :: Int)
    (userNewsForm, _) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm newsAForm
    defaultLayout $ do
        addStylesheet $ StaticR css_site_css
        setTitle "Deep Sky - Messages"
        $(widgetFile "messages")
 
postMessageListR :: Handler Html
postMessageListR = do
    (uId, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    
    ((formRes, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ newsAForm
    res <- case formRes of
                FormSuccess x -> return x
                _ -> redirect FactionR
    date <- runDB $ starDate
    let content = UserWrittenNews (nfContent res) (nfIcon res) (timeCurrentTime date) (userIdent user)
    let news = News (toStrict $ encodeToLazyText content) fId (timeCurrentTime date) False
    _ <- runDB $ insert news
    redirect MessageListR    

getMessageDeleteR :: Key News -> Handler Html
getMessageDeleteR nId = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
            Just x -> return x
            Nothing -> redirect ProfileR
    _ <- runDB $ update nId [ NewsDismissed =. True ]
    redirect MessageListR
 
loadNewsEntries :: (PersistQueryRead backend, MonadIO m,
    BaseBackend backend ~ SqlBackend) =>
    Key Faction -> ReaderT backend m [Entity News]
loadNewsEntries fId = do
    selectList [ NewsFactionId ==. fId 
               , NewsDismissed ==. False ] [ Desc NewsDate ]

newsAForm :: AForm Handler NewsPostingForm
newsAForm = NewsPostingForm
        <$> areq (selectFieldList entries) "Icon: " Nothing
        <*> areq textField "Message: " Nothing
        <*  bootstrapSubmit (BootstrapSubmit ("Submit" :: Text) "btn-default" [])
    where
        entries :: [(Text, UserNewsIcon)]
        entries = [ ("Generic", GenericUserNews)
                  , ("Jubilation", JubilationUserNews)
                  , ("Cat", CatUserNews)
                  ]

data NewsPostingForm = NewsPostingForm
    { nfIcon :: UserNewsIcon
    , nfContent :: Text
    }
