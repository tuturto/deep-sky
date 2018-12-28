{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Messages ( getApiMessageR, getMessageR, deleteApiMessageIdR
                        , postApiMessageR, getApiMessageIconsR )
    where

import Common ( apiRequireFaction, toDto, fromDto, apiNotFound, apiForbidden )
import Handler.Home ( getNewHomeR )
import Import
import News ( parseNewsEntities, iconMapper, NewsArticle(..), UserWrittenNews(..)
            , iconInfo, userNewsIconMapper )
import Data.Aeson.Text (encodeToLazyText)
import MenuHelpers (starDate)


-- | Api method to retrieve all pending messages
getApiMessageR :: Handler Value
getApiMessageR = do
    (_, _, fId) <- apiRequireFaction
    loadAllMessages fId


-- | Api method to mark message deleted. Marking already deleted message doesn't have
-- any effect.
deleteApiMessageIdR :: Key News -> Handler Value
deleteApiMessageIdR mId = do
    (_, _, fId) <- apiRequireFaction
    loadedMessages <- runDB $ selectList [ NewsId ==. mId
                                         , NewsFactionId ==. fId ] [ Asc NewsDate ]
    _ <- if length loadedMessages == 0
            then apiNotFound
            else runDB $ update mId [ NewsDismissed =. True ]
    loadAllMessages fId


-- | Api method to add new user submitted news article
-- Trying to submit any other type of news article will return
postApiMessageR :: Handler Value
postApiMessageR = do
    (_, user, fId) <- apiRequireFaction
    currentDate <- runDB starDate
    msg <- requireJsonBody
    let article = (setUser user . setStarDate currentDate . fromDto) msg
    _ <- if isUserSupplied article
            then runDB $ insert News { newsContent = toStrict $ encodeToLazyText article
                                     , newsFactionId = fId
                                     , newsDate = timeCurrentTime currentDate
                                     , newsDismissed = False
                                     }
            else apiForbidden "unsupported article type"
    loadAllMessages fId


-- | Api method for retrieving list of all icons used for user submitted news and
-- their respective image urls
-- This resource doesn't require any kind of authentication or authorization
getApiMessageIconsR :: Handler Value
getApiMessageIconsR = do
    render <- getUrlRender
    return $ (toJSON . iconInfo . userNewsIconMapper) render


getMessageR :: Handler Html
getMessageR = getNewHomeR


-- | Return true if article is user written
isUserSupplied :: NewsArticle -> Bool
isUserSupplied (UserWritten _) = True
isUserSupplied _ = False


-- | Update news article to have specific star date
-- This is specific to only user supplied news. For other news articles, no changes are made
setStarDate :: Time -> NewsArticle -> NewsArticle
setStarDate sDate (UserWritten details) =
    UserWritten $ details { userWrittenNewsDate = timeCurrentTime sDate }

setStarDate _ article =
    article


-- | Update news article to have specific user as article writer
-- This is specific to only user supplied news. For other news articles, no changes are made
setUser :: User -> NewsArticle -> NewsArticle
setUser user (UserWritten details) =
    UserWritten $ details { userWrittenNewsUser = userIdent user }

setUser _ article =
    article


-- | Load all messages of a faction that have not yet been dismissed and return them as JSON
-- Message icons are returned as links to respective server resources
loadAllMessages :: Key Faction -> HandlerFor App Value
loadAllMessages fId = do
    loadedMessages <- runDB $ selectList [ NewsFactionId ==. fId
                                         , NewsDismissed ==. False ] [ Desc NewsDate ]
    let parsedMessages = parseNewsEntities loadedMessages
    render <- getUrlRender
    let userIcons = userNewsIconMapper render
    return $ toJSON $ map (toDto . (flip (,) (iconMapper render userIcons))) parsedMessages
