{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TupleSections              #-}

module Handler.Messages
    ( getApiMessageR, getMessageR, deleteApiMessageIdR, postApiMessageR
    , getApiMessageIconsR, putApiMessageIdR )

    where

import Import
import Data.Aeson.Text ( encodeToLazyText )
import Data.Either.Validation ( Validation(..), _Failure, _Success )

import Common ( apiRequireFaction, toDto, fromDto, apiNotFound, apiForbidden
              , apiRequireViewSimulation, apiRequireOpenSimulation )
import Control.Lens ( (#) )
import Control.Monad.Trans.Writer ( WriterT, runWriterT, tell )
import CustomTypes ( SpecialEventStatus(..), StarDate )
import Events.Import ( EventResolveType(..) )
import Errors ( ErrorCode(..), raiseIfErrors )
import Handler.Home ( getNewHomeR )
import MenuHelpers ( starDate )
import News.Data ( NewsArticle(..), UserWrittenNews(..)
                 , resolveType )
import News.Import ( parseNewsEntities, iconMapper, iconInfo, userNewsIconMapper
                   , productionChangeEndedIconMapper, parseNews )
import Simulation.Events ( extractSpecialNews, handleSpecialEvent )


-- | Api method to retrieve all pending messages
getApiMessageR :: Handler Value
getApiMessageR = do
    (uId, _, avatar, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    loadAllMessages fId (entityKey avatar)


-- | Api method to mark message deleted. Marking already deleted message doesn't have
-- any effect.
deleteApiMessageIdR :: NewsId -> Handler Value
deleteApiMessageIdR mId = do
    (uId, _, avatar, fId) <- apiRequireFaction
    _ <- apiRequireOpenSimulation uId
    loadedMessages <- runDB $ selectList [ NewsId ==. mId
                                         , NewsFactionId ==. (Just fId) ] [ Asc NewsDate ]
    _ <- if null loadedMessages
            then apiNotFound
            else runDB $ update mId [ NewsDismissed =. True ]
    loadAllMessages fId (entityKey avatar)


-- | Api method for updated specific message. Used to make user choice for interactive
-- event
putApiMessageIdR :: NewsId -> Handler Value
putApiMessageIdR mId = do
    (uId, _, avatarE, fId) <- apiRequireFaction
    _ <- apiRequireOpenSimulation uId
    msg <- requireJsonBody
    (_, errs) <- runDB . runWriterT $ updateEventWithChoice mId avatarE $ fromDto msg
    raiseIfErrors errs
    loadAllMessages fId (entityKey avatarE)


-- | Handles case where user has made choice on interactive event
-- there are two major cases: simpler one is delayed event where event is just
-- updated with the choice. More complex case is immediate event where user
-- made choice immediately triggers resolving of the event
updateEventWithChoice :: (MonadIO m, PersistQueryWrite backend, PersistUniqueRead backend,
    BaseBackend backend ~ SqlBackend) =>
    NewsId
    -> Entity Person
    -> NewsArticle
    -> WriterT [ErrorCode] (ReaderT backend m) ()
updateEventWithChoice mId avatarE msgArticle = do
    loadedMessage <- lift $ get mId
    case loadedMessage of
        Nothing ->
            tell [ ResourceNotFound ]

        Just dbNews -> do
            case parseNews dbNews of
                Nothing ->
                    tell [ FailedToParseDataInDatabase ]

                Just dbArticle -> do
                    --TODO: validate possible faction id (in case of kragii for example)
                    let vRes = pure dbArticle <*
                                    newsIsUnresolved dbNews <*
                                    userHasRightToChoose avatarE dbNews :: Validation [ErrorCode] NewsArticle

                    case vRes of
                        Failure err ->
                            tell err

                        Success article ->
                            case resolveType article of
                                Just ImmediateEvent -> do
                                    _ <- handleWithChoice mId msgArticle
                                    return ()

                                Just DelayedEvent -> do
                                    lift $ updateWithChoice mId msgArticle

                                Nothing ->
                                    tell [ FailedToParseDataInDatabase ]


-- | Validation ensuring that the news article hasn't be already resolved
newsIsUnresolved :: News -> Validation [ErrorCode] News
newsIsUnresolved news =
    case newsSpecialEvent news of
        UnhandledSpecialEvent ->
            _Success # news

        HandledSpecialEvent ->
            _Failure # [ SpecialEventHasAlreadyBeenResolved ]

        NoSpecialEvent ->
            _Failure # [ TriedToMakeChoiceForRegularArticle ]


-- | Validation that user has rights to choose action for given article
userHasRightToChoose :: Entity Person -> News -> Validation [ErrorCode] News
userHasRightToChoose personE news =
    if ((personFactionId . entityVal) personE == newsFactionId news
        || (Just $ entityKey personE) == newsPersonId news)
        then _Success # news
        else _Failure # [ InsufficientRights ]


-- | Update news article content
updateWithChoice :: (MonadIO m, PersistStoreWrite backend,
    BaseBackend backend ~ SqlBackend) =>
    NewsId -> NewsArticle -> ReaderT backend m ()
updateWithChoice mId article =
    update mId [ NewsContent =. toStrict (encodeToLazyText article) ]


-- | Update news article content and in case of immediate events, trigger
-- resolution for special event
handleWithChoice :: (MonadIO m, PersistQueryWrite backend, PersistUniqueRead backend,
    BaseBackend backend ~ SqlBackend) =>
    NewsId
    -> NewsArticle
    -> WriterT [ErrorCode] (ReaderT backend m) (Maybe NewsId)
handleWithChoice mId article = do
    date <- lift starDate
    _ <- lift $ updateWithChoice mId article
    case extractSpecialNews (mId, article) of
        Nothing -> do
            tell [ SpecialNewsExtractionFailed ]
            return Nothing

        Just sNews -> do
            res <- lift $ handleSpecialEvent date sNews
            return $ Just res


-- | Api method to add new user submitted news article
-- Trying to submit any other type of news article will return
postApiMessageR :: Handler Value
postApiMessageR = do
    (uId, _, avatar, fId) <- apiRequireFaction
    _ <- apiRequireOpenSimulation uId
    currentDate <- runDB starDate
    msg <- requireJsonBody
    let article = (setUser (entityVal avatar) . setStarDate currentDate . fromDto) msg
    _ <- if isUserSupplied article
            then runDB $ insert News { newsContent = toStrict $ encodeToLazyText article
                                     , newsFactionId = Just fId
                                     , newsPersonId = Nothing
                                     , newsDate = currentDate
                                     , newsDismissed = False
                                     , newsSpecialEvent = NoSpecialEvent
                                     }
            else apiForbidden "unsupported article type"
    loadAllMessages fId (entityKey avatar)


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


-- | Update news article to have specific star date
-- This is specific to only user supplied news. For other news articles, no changes are made
setStarDate :: StarDate -> NewsArticle -> NewsArticle
setStarDate date (UserWritten details) =
    UserWritten $ details { userWrittenNewsDate = date }

setStarDate _ article =
    article


-- | Update news article to have specific person as article writer
-- This is specific to only user supplied news. For other news articles, no changes are made
setUser :: Person -> NewsArticle -> NewsArticle
setUser person (UserWritten details) =
    UserWritten $ details { userWrittenNewsUser = personName person }

setUser _ article =
    article


-- | Load all messages of a faction that have not yet been dismissed and return them as JSON
-- Message icons are returned as links to respective server resources
loadAllMessages :: FactionId -> PersonId -> HandlerFor App Value
loadAllMessages fId pId = do
    loadedMessages <- runDB $ selectList ( [ NewsFactionId ==. Just fId
                                           , NewsDismissed ==. False ]
                                           ||. [ NewsPersonId ==. Just pId
                                               , NewsDismissed ==. False ] )
                                         [ Desc NewsDate ]
    let parsedMessages = parseNewsEntities loadedMessages
    render <- getUrlRender
    let userIcons = userNewsIconMapper render
    let changeIcons = productionChangeEndedIconMapper render
    let icons = iconMapper render userIcons changeIcons
    return $ toJSON $ map (toDto . (, icons)) parsedMessages
