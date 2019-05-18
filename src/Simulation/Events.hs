{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}


module Simulation.Events ( handleFactionEvents, addSpecialEvents )
    where

import Import
import qualified Prelude as P
import System.Random
import Common ( maybeGet )
import CustomTypes ( SpecialEventStatus(..), PercentileChance(..), RollResult(..)
                   , PlanetaryStatus(..), StarDate, roll )
import Events.Import ( resolveEvent, EventRemoval(..) )
import Events.News ( report, kragiiWormsEvent )
import News.Data ( SpecialNews(..), NewsArticle(..) )
import News.Import ( parseNewsEntities, productionBoostStartedNews, productionSlowdownStartedNews )
import Queries ( kragiiTargetPlanets, farmingChangeTargetPlanets )
import Resources ( ResourceType(..) )


-- | Handle all special events for given faction
-- After processing an event, it is marked as handled and dismissed
handleFactionEvents :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, PersistQueryWrite backend, MonadIO m) =>
    StarDate -> Entity Faction -> ReaderT backend m ()
handleFactionEvents date faction = do
    -- doing it like this requires that all special events for the faction are to be processed
    -- in case where handling a special event for one faction might create special event for
    -- another faction, this method will fail
    -- if that ever comes to be, we can add NewsDate into query and only process special events
    -- that are old enough.
    loadedMessages <- selectList [ NewsFactionId ==. entityKey faction
                                 , NewsSpecialEvent ==. UnhandledSpecialEvent ] [ Desc NewsDate ]
    let specials = mapMaybe extractSpecialNews $ parseNewsEntities loadedMessages
    _ <- mapM (handleSpecialEvent (entityKey faction) date) specials
    return ()


-- | Extract possible special event from news article
extractSpecialNews :: (Key News, NewsArticle) -> Maybe (Key News, SpecialNews)
extractSpecialNews (nId, Special article) = Just (nId, article)
extractSpecialNews _ = Nothing


-- | Handle special event, mark it processed and dismissed, create news article about results
handleSpecialEvent :: (PersistQueryWrite backend, MonadIO m
    , BaseBackend backend ~ SqlBackend) =>
    Key Faction -> StarDate -> (Key News, SpecialNews) -> ReaderT backend m (Key News)
handleSpecialEvent fId date (nId, KragiiWorms event _ choice) = do
    (removal, results) <- resolveEvent (nId, event) choice
    -- events that signal that they should be removed or that fail to signal anything are marked processed
    -- this is done to remove events that failed to process so they won't dangle around for eternity
    -- in future, it would be good idea to log such cases somewhere for further inspection
    when (removal /= Just KeepOriginalEvent) $
                    updateWhere [ NewsId ==. nId ]
                                [ NewsSpecialEvent =. HandledSpecialEvent
                                , NewsDismissed =. True ]
    insert $ report fId date event choice results


-- | Handle adding zero or more special events for a given faction
addSpecialEvents :: ( PersistQueryRead backend, MonadIO m, PersistStoreWrite backend
    , BackendCompatible SqlBackend backend
    , PersistUniqueRead backend, BaseBackend backend ~ SqlBackend) =>
    StarDate -> Entity Faction -> ReaderT backend m ()
addSpecialEvents date faction = do
    -- TODO: dynamic length
    n <- liftIO $ randomRIO (0, 2)
    let (odds, eventCreator) = eventCreators P.!! n
    _ <- runEvent odds date faction eventCreator
    return ()


eventCreators :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    [ (PercentileChance, StarDate -> Entity Faction -> ReaderT backend m (Maybe (Key News))) ]
eventCreators =
    [ (PercentileChance 2, kragiiAttack)
    , (PercentileChance 5, biologicalsBoost)
    , (PercentileChance 5, biologicalsSlowdown)
    ]


runEvent :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    PercentileChance -> StarDate -> Entity Faction ->
    (StarDate -> Entity Faction -> ReaderT backend m (Maybe (Key News))) ->
    ReaderT backend m (Maybe (Key News))
runEvent odds date faction fn = do
    res <- liftIO $ roll odds
    case res of
        Success ->
            fn date faction

        Failure ->
            return Nothing


kragiiAttack :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    StarDate -> Entity Faction -> ReaderT backend m (Maybe (Key News))
kragiiAttack date faction = do
    planets <- kragiiTargetPlanets 10 5 $ entityKey faction
    if length planets == 0
        then return Nothing
        else do
            n <- liftIO $ randomRIO (0, length planets - 1)
            let planet = maybeGet n planets
            let statusRec = PlanetStatus <$> fmap entityKey planet
                                         <*> Just KragiiAttack
                                         <*> Just Nothing
            _ <- mapM insert statusRec
            starSystem <- mapM (getEntity . planetStarSystemId . entityVal) planet
            let event = join $ kragiiWormsEvent <$> planet <*> join starSystem <*> Just date
            mapM insert event


biologicalsBoost :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    StarDate -> Entity Faction -> ReaderT backend m (Maybe (Key News))
biologicalsBoost date faction = do
    planets <- farmingChangeTargetPlanets $ entityKey faction
    if length planets == 0
        then return Nothing
        else do
            n <- liftIO $ randomRIO (0, length planets - 1)
            let planet = maybeGet n planets
            let statusRec = PlanetStatus <$> fmap entityKey planet
                                         <*> Just GoodHarvest
                                         <*> Just (Just (date + 6))
            _ <- mapM insert statusRec
            starSystem <- mapM (getEntity . planetStarSystemId . entityVal) planet
            let event = productionBoostStartedNews <$> planet
                            <*> join starSystem
                            <*> Just BiologicalResource
                            <*> Just date
                            <*> Just (entityKey faction)
            mapM insert event


biologicalsSlowdown :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    StarDate -> Entity Faction -> ReaderT backend m (Maybe (Key News))
biologicalsSlowdown date faction = do
    planets <- farmingChangeTargetPlanets $ entityKey faction
    if length planets == 0
        then return Nothing
        else do
            n <- liftIO $ randomRIO (0, length planets - 1)
            let planet = maybeGet n planets
            let statusRec = PlanetStatus <$> fmap entityKey planet
                                         <*> Just PoorHarvest
                                         <*> Just (Just (date + 6))
            _ <- mapM insert statusRec
            starSystem <- mapM (getEntity . planetStarSystemId . entityVal) planet
            let event = productionSlowdownStartedNews <$> planet
                            <*> join starSystem
                            <*> Just BiologicalResource
                            <*> Just date
                            <*> Just (entityKey faction)
            mapM insert event
