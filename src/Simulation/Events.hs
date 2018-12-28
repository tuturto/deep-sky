{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}


module Simulation.Events ( handleFactionEvents, addSpecialEvents )
    where

import Import
import System.Random
import Common ( maybeGet )
import CustomTypes ( SpecialEventStatus(..), PercentileChance(..), RollResult(..)
                   , roll )
import Events.Import ( resolveEvent, EventRemoval(..) )
import Events.News ( report, kragiiWormsEvent )
import News.Data ( SpecialNews(..), NewsArticle(..) )
import News.Import ( parseNewsEntities )
import Queries ( populatedFarmingPlanets )


-- | Handle all special events for given faction
-- After processing an event, it is marked as handled and dismissed
-- Returned list contains keys to news articles that were created as a result of processing
handleFactionEvents :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, PersistQueryWrite backend, MonadIO m) =>
    Time -> Entity Faction -> ReaderT backend m [Key News]
handleFactionEvents date faction = do
    -- doing it like this requires that all special events for the faction are to be processed
    -- in case where handling a special event for one faction might create special event for
    -- another faction, this method will fail
    -- if that ever comes to be, we can add NewsDate into query and only process special events
    -- that are old enough.
    loadedMessages <- selectList [ NewsFactionId ==. (entityKey faction)
                                 , NewsSpecialEvent ==. UnhandledSpecialEvent ] [ Desc NewsDate ]
    let specials = mapMaybe extractSpecialNews $ parseNewsEntities loadedMessages
    mapM (handleSpecialEvent (entityKey faction) date) specials


-- | Extract possible special event from news article
extractSpecialNews :: (Key News, NewsArticle) -> Maybe (Key News, SpecialNews)
extractSpecialNews (nId, (Special article)) = Just (nId, article)
extractSpecialNews _ = Nothing


-- | Handle special event, mark it processed and dismissed, create news article about results
handleSpecialEvent :: (PersistQueryWrite backend, MonadIO m
                      , BaseBackend backend ~ SqlBackend) =>
                      Key Faction -> Time -> (Key News, SpecialNews) -> ReaderT backend m (Key News)
handleSpecialEvent fId date (nId, (KragiiWorms event _ choice)) = do
    (removal, results) <- resolveEvent (nId, event) choice
    -- events that signal that they should be removed or that fail to signal anything are marked processed
    -- this is done to remove events that failed to process so they won't dangle around for eternity
    -- in future, it would be good idea to log such cases somewhere for further inspection
    _ <- when (removal /= Just KeepOriginalEvent) $
                    updateWhere [ NewsId ==. nId ]
                                [ NewsSpecialEvent =. HandledSpecialEvent
                                , NewsDismissed =. True ]
    insert $ report fId date event choice results


-- | Handle adding zero or more special events for a given faction
addSpecialEvents :: ( PersistQueryRead backend, MonadIO m, PersistStoreWrite backend
                    , BackendCompatible SqlBackend backend
                    , PersistUniqueRead backend, BaseBackend backend ~ SqlBackend) =>
                    Time -> Key Faction -> ReaderT backend m (Maybe (Key News))
addSpecialEvents date fId = do
    -- TODO: don't trigger on planets with kragii problems
    res <- liftIO $ roll $ PercentileChance 5
    case res of
        Success -> do
            planets <- populatedFarmingPlanets 10 5 fId
            if length planets == 0
                then return Nothing
                else do
                    n <- liftIO $ randomRIO (0, length planets - 1)
                    let planet = maybeGet n planets
                    starSystem <- mapM (getEntity . planetStarSystemId . entityVal) planet
                    let event = join $ kragiiWormsEvent <$> planet <*> join starSystem <*> Just date
                    mapM insert event

        Failure -> do
            return Nothing
