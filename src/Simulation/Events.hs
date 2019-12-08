{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}


module Simulation.Events
    ( handleFactionEvents, addSpecialEvents, extractSpecialNews
    , handleSpecialEvent, handlePersonalEvent )

    where

import Import
import qualified Prelude as P
import System.Random
import Common ( maybeGet )
import CustomTypes ( SpecialEventStatus(..), PercentileChance(..), RollResult(..)
                   , StarDate, roll )
import qualified Events.Creation as EC
import Events.Import ( resolveEvent, EventRemoval(..) )
import Events.News ( report, kragiiWormsEvent, scurryingSoundsEvent, namingPetEvent )
import News.Data ( SpecialNews(..), NewsArticle(..) )
import News.Import ( parseNewsEntities, productionBoostStartedNews, productionSlowdownStartedNews )
import Queries ( kragiiTargetPlanets, farmingChangeTargetPlanets )
import Resources ( ResourceType(..) )
import Space.Data ( PlanetaryStatus(..) )


-- | Handle all special events for given faction
-- After processing an event, it is marked as handled and dismissed
handleFactionEvents :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, PersistQueryWrite backend
    , PersistUniqueRead backend, MonadIO m) =>
    StarDate -> Entity Faction -> ReaderT backend m ()
handleFactionEvents date faction = do
    loadedMessages <- selectList [ NewsFactionId ==. (Just $ entityKey faction)
                                 , NewsSpecialEvent ==. UnhandledSpecialEvent
                                 , NewsDate <. date ] [ Desc NewsDate ]
    let specials = mapMaybe extractSpecialNews $ parseNewsEntities loadedMessages
    _ <- mapM (handleSpecialEvent date) specials
    return ()


-- | Handle all special events that are specific to person
-- After processing an event, it is marked as handled and dismissed
handlePersonalEvent :: (MonadIO m, PersistQueryWrite backend, PersistUniqueRead backend,
    BaseBackend backend ~ SqlBackend) =>
    StarDate -> ReaderT backend m ()
handlePersonalEvent date = do
    loadedMessages <- selectList [ NewsFactionId ==. Nothing
                                 , NewsSpecialEvent ==. UnhandledSpecialEvent
                                 , NewsDate <. date ] [ Desc NewsDate ]
    let specials = mapMaybe extractSpecialNews $ parseNewsEntities loadedMessages
    _ <- mapM (handleSpecialEvent date) specials
    return ()


-- | Extract possible special event from news article
extractSpecialNews :: (NewsId, NewsArticle) -> Maybe (NewsId, SpecialNews)
extractSpecialNews (nId, Special article) = Just (nId, article)
extractSpecialNews _ = Nothing


-- TODO: deduplicate
-- | Handle special event, mark it processed and dismissed, create news article about results
handleSpecialEvent :: (PersistQueryWrite backend, PersistUniqueRead backend, MonadIO m
    , BaseBackend backend ~ SqlBackend) =>
    StarDate -> (NewsId, SpecialNews) -> ReaderT backend m NewsId
handleSpecialEvent date (nId, KragiiWorms event _ choice) = do
    (actions, results) <- resolveEvent (nId, event) choice
    _ <- updateEvent date nId actions
    insert $ report date event choice results

handleSpecialEvent date (nId, ScurryingSounds event _ choice) = do
    (actions, results) <- resolveEvent (nId, event) choice
    _ <- updateEvent date nId actions
    insert $ report date event choice results


handleSpecialEvent date (nId, NamingPet event _ choice) = do
    (actions, results) <- resolveEvent (nId, event) choice
    _ <- updateEvent date nId actions
    insert $ report date event choice results


updateEvent :: (PersistQueryWrite backend, MonadIO m,
   PersistUniqueRead backend, BaseBackend backend ~ SqlBackend) =>
   StarDate
   -> NewsId
   -> Maybe (EventRemoval, [EC.EventCreation])
   -> ReaderT backend m ()
updateEvent date nId actions = do
    -- events that signal that they should be removed or that fail to signal anything are marked processed
    -- this is done to remove events that failed to process so they won't dangle around for eternity
    -- in future, it would be good idea to log such cases somewhere for further inspection
    case actions of
        Just (RemoveOriginalEvent, creations) -> do
            _ <- mapM (createSpecialEvents date) creations
            updateWhere [ NewsId ==. nId ]
                        [ NewsSpecialEvent =. HandledSpecialEvent
                        , NewsDismissed =. True ]

        Just (KeepOriginalEvent, creations) -> do
            _ <- mapM (createSpecialEvents date) creations
            return ()

        Nothing ->
            updateWhere [ NewsId ==. nId ]
                        [ NewsSpecialEvent =. HandledSpecialEvent
                        , NewsDismissed =. True ]


-- | Insert new special event into database based on creation parameters
createSpecialEvents :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    StarDate
    -> EC.EventCreation
    -> ReaderT backend m (Maybe NewsId)
createSpecialEvents date event =
    case event of
        EC.NamingPet pId peId ->
            namingPet date pId peId


-- | Handle adding zero or more special events for a given faction
addSpecialEvents :: ( PersistQueryRead backend, MonadIO m, PersistStoreWrite backend
    , BackendCompatible SqlBackend backend
    , PersistUniqueRead backend, BaseBackend backend ~ SqlBackend) =>
    StarDate -> Entity Faction -> ReaderT backend m ()
addSpecialEvents date faction = do
    -- TODO: dynamic length
    -- TODO: these should be split into parts
       -- one possible faction wide event per turn
       -- one possbile planet event per planet
       -- one possible person event per person
    n <- liftIO $ randomRIO (0, 2)
    let (odds, eventCreator) = eventCreators P.!! n
    _ <- runEvent odds date faction eventCreator

    people <- selectList [ PersonFactionId ==. (Just $ entityKey faction) ] []
    _ <- mapM (addPersonalSpecialEvents date) people
    return ()


addPersonalSpecialEvents :: ( PersistQueryRead backend, MonadIO m, PersistStoreWrite backend
    , BackendCompatible SqlBackend backend
    , PersistUniqueRead backend, BaseBackend backend ~ SqlBackend) =>
    StarDate -> Entity Person -> ReaderT backend m ()
addPersonalSpecialEvents date person = do
    -- TODO: dynamic length
    n <- liftIO $ randomRIO (0, 0)
    let (odds, eventCreator) = personalEventCreators P.!! n
    _ <- runPersonalEvent odds date person eventCreator
    return ()

-- | List of faction wide event creators
eventCreators :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    [ (PercentileChance, StarDate -> Entity Faction -> ReaderT backend m (Maybe NewsId)) ]
eventCreators =
    -- TODO: kragii attack or biological boost / slowdown should really be planet specific
    [ (PercentileChance 2, kragiiAttack)
    , (PercentileChance 5, biologicalsBoost)
    , (PercentileChance 5, biologicalsSlowdown)
    ]


-- | List of person specific event creators
personalEventCreators :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    [ (PercentileChance, StarDate -> Entity Person -> ReaderT backend m (Maybe NewsId)) ]
personalEventCreators =
    [ (PercentileChance 1, scurryingNoises)
    ]


runEvent :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    PercentileChance -> StarDate -> Entity Faction ->
    (StarDate -> Entity Faction -> ReaderT backend m (Maybe (Key News))) ->
    ReaderT backend m (Maybe NewsId)
runEvent odds date faction fn = do
    res <- liftIO $ roll odds
    case res of
        Success ->
            fn date faction

        Failure ->
            return Nothing


-- | run personal event against a person
-- depending on the chance, event might or might not actually trigger
runPersonalEvent :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    PercentileChance
    -> StarDate
    -> Entity Person
    -> (StarDate -> Entity Person -> ReaderT backend m (Maybe NewsId)) ->
    ReaderT backend m (Maybe NewsId)
runPersonalEvent odds date person fn = do
    res <- liftIO $ roll odds
    case res of
        Success ->
            fn date person

        Failure ->
            return Nothing


-- | possible kragii infestation
kragiiAttack :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    StarDate -> Entity Faction -> ReaderT backend m (Maybe NewsId)
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
            let event = join $ kragiiWormsEvent <$> planet
                                                <*> join starSystem
                                                <*> Just date
                                                <*> Just (entityKey faction)
            mapM insert event


-- | possible boost in production of biological resources
biologicalsBoost :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    StarDate -> Entity Faction -> ReaderT backend m (Maybe NewsId)
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


-- | possible slowdown in production of biological resources
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


-- | possible rat infestation in person's living quarters
scurryingNoises :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend, BackendCompatible SqlBackend backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    StarDate
    -> Entity Person
    -> ReaderT backend m (Maybe NewsId)
scurryingNoises date person = do
    let info = scurryingSoundsEvent (entityKey person) date
    event <- insert info
    return $ Just event


-- | chance to name your new pet
namingPet :: (PersistStoreWrite backend, PersistUniqueRead backend,
    PersistQueryRead backend,
    MonadIO m, BaseBackend backend ~ SqlBackend) =>
    StarDate
    -> PersonId
    -> PetId
    -> ReaderT backend m (Maybe NewsId)
namingPet date pId peId = do
    personM <- getEntity pId
    petM <- getEntity peId
    --TODO: clean up
    case personM of
        Just person ->
            case petM of
                Just pet -> do
                    info <- namingPetEvent person pet date
                    nId <- insert info
                    return $ Just nId

                _ ->
                    return Nothing

        _ ->
            return Nothing
