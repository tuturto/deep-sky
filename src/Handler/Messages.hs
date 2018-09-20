{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Messages ( getMessageListR, postNewMessageR, getMessageDeleteR )
    where

import Import
import Widgets (newsArticleWidget)
import News (parseNewsEntities, UserNewsIcon(..), makeUserWrittenNews)
import Yesod.Form.Bootstrap3
import MenuHelpers (starDate)

getMessageListR :: Int -> Handler Html
getMessageListR currentPage = do
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    let pageSize = 6
    (totalUnread, totalPages, loadedNews) <- runDB $ loadNewsEntries pageSize currentPage fId
    let entries = parseNewsEntities loadedNews
    let doubleLeftLnk = if currentPage > 1
                            then Just 1
                            else Nothing
    let doubleRightLnk = if currentPage < totalPages
                            then Just totalPages
                            else Nothing
    let leftLnk = if currentPage > 1
                    then Just $ currentPage - 1
                    else Nothing
    let rightLnk = if currentPage < totalPages
                    then Just $ currentPage + 1
                    else Nothing
    (userNewsForm, _) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm newsAForm
    defaultLayout $ do
        addStylesheet $ StaticR css_site_css
        setTitle "Deep Sky - Messages"
        $(widgetFile "messages")
 
postNewMessageR :: Handler Html
postNewMessageR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                Just x -> return x
                Nothing -> redirect ProfileR
    
    ((formRes, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ newsAForm
    res <- case formRes of
                FormSuccess x -> return x
                _ -> redirect FactionR
    date <- runDB $ starDate
    _ <- runDB $ insert $ makeUserWrittenNews (nfContent res) (nfIcon res) date user
    redirect $ MessageListR 1

getMessageDeleteR :: Key News -> Handler Html
getMessageDeleteR nId = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
            Just x -> return x
            Nothing -> redirect ProfileR
    _ <- runDB $ update nId [ NewsDismissed =. True ]
    redirect (MessageListR 1)

loadNewsEntries :: (PersistQueryRead backend, MonadIO m,
    BaseBackend backend ~ SqlBackend) =>
    Int -> Int -> Key Faction -> ReaderT backend m (Int, Int, [Entity News])
loadNewsEntries pageSize page fId = do
    results <- selectList [ NewsFactionId ==. fId 
                          , NewsDismissed ==. False ] [ Desc NewsDate 
                                                      , OffsetBy $ (page - 1) * pageSize
                                                      , LimitTo pageSize 
                                                      ]
    totalRecords <- count [ NewsFactionId ==. fId 
                          , NewsDismissed ==. False ]
    let totalPages = case totalRecords `mod` pageSize of
                        0 -> (totalRecords `div` pageSize)
                        _ -> (totalRecords `div` pageSize) + 1
    return (totalRecords, totalPages, results) 
  
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
