{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TupleSections         #-}

module Handler.Messages ( getApiMessageR, getMessageR, deleteApiMessageIdR
                        , postApiMessageR, getApiMessageIconsR, putApiMessageIdR )
    where

import Import
import Data.Aeson.Text (encodeToLazyText)

import Common ( apiRequireFaction, toDto, fromDto, apiNotFound, apiForbidden )
import CustomTypes ( SpecialEventStatus(..), StarDate )
import Handler.Home ( getNewHomeR )
import MenuHelpers ( starDate )
import News.Data ( NewsArticle(..), UserWrittenNews(..) )
import News.Import ( parseNewsEntities, iconMapper, iconInfo, userNewsIconMapper
                   , productionChangeEndedIconMapper )


-- | Api method to retrieve all pending messages
getApiMessageR :: Handler Value
getApiMessageR = do
    (_, _, _, fId) <- apiRequireFaction
    loadAllMessages fId


-- | Api method to mark message deleted. Marking already deleted message doesn't have
-- any effect.
deleteApiMessageIdR :: Key News -> Handler Value
deleteApiMessageIdR mId = do
    (_, _, _, fId) <- apiRequireFaction
    loadedMessages <- runDB $ selectList [ NewsId ==. mId
                                         , NewsFactionId ==. fId ] [ Asc NewsDate ]
    _ <- if null loadedMessages
            then apiNotFound
            else runDB $ update mId [ NewsDismissed =. True ]
    loadAllMessages fId


-- | Api method for updated specific message. Used to make user choice for interactive
-- event
putApiMessageIdR :: Key News -> Handler Value
putApiMessageIdR mId = do
    (_, _, _, fId) <- apiRequireFaction
    msg <- requireJsonBody
    let article = fromDto msg
    _ <- if isSpecialEvent article
            then do
                loadedMessages <- runDB $ selectList [ NewsId ==. mId
                                                     , NewsFactionId ==. fId ] [ Asc NewsDate ]
                if null loadedMessages
                    then apiNotFound
                    else runDB $ update mId [ NewsContent =. toStrict (encodeToLazyText article) ]
            else apiForbidden "unsupported article type"
    loadAllMessages fId


-- | Api method to add new user submitted news article
-- Trying to submit any other type of news article will return
postApiMessageR :: Handler Value
postApiMessageR = do
    (_, user, _, fId) <- apiRequireFaction
    currentDate <- runDB starDate
    msg <- requireJsonBody
    let article = (setUser user . setStarDate currentDate . fromDto) msg
    _ <- if isUserSupplied article
            then runDB $ insert News { newsContent = toStrict $ encodeToLazyText article
                                     , newsFactionId = fId
                                     , newsDate = currentDate
                                     , newsDismissed = False
                                     , newsSpecialEvent = NoSpecialEvent
                                     }
            else apiForbidden "unsupported article type"
    loadAllMessages fId


-- | Api method for retrieving list of all icons used for user submitted news and
-- their respective image urls
-- This resource doesn't require any kind of authentication or authorization
getApiMessageIconsR :: Handler Value
getApiMessageIconsR =
    toJSON . iconInfo . userNewsIconMapper <$> getUrlRender


getMessageR :: Handler Html
getMessageR = getNewHomeR


-- | True if article is user written
isUserSupplied :: NewsArticle -> Bool
isUserSupplied (UserWritten _) = True
isUserSupplied _ = False


-- | True if article is special event
isSpecialEvent :: NewsArticle -> Bool
isSpecialEvent (Special _) = True
isSpecialEvent _ = False


-- | Update news article to have specific star date
-- This is specific to only user supplied news. For other news articles, no changes are made
setStarDate :: StarDate -> NewsArticle -> NewsArticle
setStarDate date (UserWritten details) =
    UserWritten $ details { userWrittenNewsDate = date }

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
    let changeIcons = productionChangeEndedIconMapper render
    let icons = iconMapper render userIcons changeIcons
    return $ toJSON $ map (toDto . (, icons)) parsedMessages
