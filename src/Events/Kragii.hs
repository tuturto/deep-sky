{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}

module Events.Kragii ( KragiiWormsEvent(..), KragiiWormsChoice(..), KragiiResults(..)
                     , KragiiNews(..) )
    where


import Import
import Control.Monad.Trans.Maybe ( MaybeT(..), runMaybeT )
import Control.Monad.Trans.Writer ( WriterT, runWriterT, tell )
import Data.Aeson.TH

import Common ( ToDto(..), FromDto(..) )
import CustomTypes ( PercentileChance(..), RollResult(..), PlanetaryStatus(..)
                   , StarDate, roll )
import Dto.News ( KragiiWormsChoiceDto(..), UserOptionDto(..), KragiiNewsDto(..) )
import Events.Import ( SpecialEvent(..), EventRemoval(..), UserOption(..) )
import Resources ( RawResource(..), Biological(..) )


-- | Data for kragii worms attack
data KragiiWormsEvent = KragiiWormsEvent
    { kragiiWormsPlanetId :: Key Planet
    , kragiiWormsPlanetName :: Text
    , kragiiWormsSystemId :: Key StarSystem
    , kragiiWormsSystemName :: Text
    , kragiiWormsDate :: StarDate
    }
    deriving (Show, Read, Eq)


-- | User choices for kragii worms attack
data KragiiWormsChoice = EvadeWorms
    | AttackWorms
    | TameWorms
    deriving (Show, Read, Eq)


-- | Results of resolving kragii attack
data KragiiResults =
    WormsStillPresent
    | WormsRemoved
    | WormsTamed
    | CropsDestroyed (RawResource Biological)
    | FarmersInjured
    deriving (Show, Read, Eq)


-- | data for kragii attack resolution
data KragiiNews = KragiiNews
    { kragiiNewsPlanetId :: Key Planet
    , kragiiNewsPlanetName :: Text
    , kragiiNewsSystemId :: Key StarSystem
    , kragiiNewsSystemName :: Text
    , kragiiNewsExplanation :: Text
    , kragiiNewsDate :: StarDate
    }
    deriving (Show, Read, Eq)


instance SpecialEvent KragiiWormsEvent KragiiWormsChoice KragiiResults where

    eventOptions _ = [ UserOption { userOptionTitle = "Avoid the worms"
                                  , userOptionExplanation = [ "Keep using fields, while avoiding the worms and hope they'll eventually leave."
                                                            , "50 units of biologicals lost"
                                                            , "25% chance of worms leaving"
                                                            ]
                                  , userOptionChoice = EvadeWorms
                                  }
                     , UserOption { userOptionTitle = "Attack worms and drive them away"
                                  , userOptionExplanation = [ "Agresively spray fields with chemicals and drive worms away while losing some harvest."
                                                            , "75% chance of driving worms away"
                                                            , "25% chance of serious injuries to farmers"
                                                            , "20 units of biologicals lost"]
                                  , userOptionChoice = AttackWorms
                                  }
                     , UserOption { userOptionTitle = "Try and tame them"
                                  , userOptionExplanation = [ "Try to use worms in your advantage in farming."
                                                            , "25% chance of success"
                                                            , "75% chance of serious injuries to farmers"
                                                            ]
                                  , userOptionChoice = TameWorms
                                  }
                     ]

    resolveEvent keyEventPair (Just choice) =
        runWriterT . runMaybeT $
            case choice of
                    EvadeWorms ->
                        chooseToAvoid keyEventPair

                    AttackWorms ->
                        chooseToAttack keyEventPair

                    TameWorms ->
                        chooseToTame keyEventPair

    resolveEvent keyEventPair Nothing =
        runWriterT . runMaybeT $ noChoice keyEventPair


-- | Avoiding kragii worms means of trying to work only part of the fields, while
-- the worms have free reign on other parts. It's least dangerous option to people
-- involved and will lead to somewhat smaller crop output while worms are present.
-- Given enough time, they should naturally move on.
chooseToAvoid :: (MonadIO m, PersistQueryWrite backend, BaseBackend backend ~ SqlBackend) =>
                 (Key News, KragiiWormsEvent) -> MaybeT (WriterT [KragiiResults] (ReaderT backend m)) EventRemoval
chooseToAvoid (_, event) = do
    faction <- getFaction event
    (cost, bioLeft) <- calculateNewBio (RawResource 50) (entityVal faction)
    _ <- destroyCrops faction cost bioLeft
    removeNews event $ PercentileChance 25


-- | Attacking and driving kragii worms away from the fields is unpleasant and
-- somewhat unsafe option. It's best option though, if one just wants to quickly get
-- rid of them. Some of the crops might be destroyed as a result of chemicals used
-- in the attack.
chooseToAttack :: (MonadIO m, PersistQueryWrite backend, BaseBackend backend ~ SqlBackend) =>
                  (Key News, KragiiWormsEvent) -> MaybeT (WriterT [KragiiResults] (ReaderT backend m)) EventRemoval
chooseToAttack (_, event) = do
    faction <- getFaction event
    (cost, bioLeft) <- calculateNewBio (RawResource 20) (entityVal faction)
    -- TODO: injured farmers
    _ <- destroyCrops faction cost bioLeft
    removeNews event $ PercentileChance 75


-- | Kragii worms can't be tamed in the usual sense. However, it is possible to
-- try to corral them and have them working on improving soil of the fields they
-- attacked. While this is potentially dangerous to people involved, it can
-- yield much more nutrient soil and thus higher crop output
chooseToTame :: (MonadIO m, PersistQueryWrite backend, BaseBackend backend ~ SqlBackend) =>
                (Key News, KragiiWormsEvent) -> MaybeT (WriterT [KragiiResults] (ReaderT backend m)) EventRemoval
chooseToTame (_, event) = do
    -- TODO: proper implementation
    faction <- getFaction event
    (cost, bioLeft) <- calculateNewBio (RawResource 50) (entityVal faction)
    _ <- destroyCrops faction cost bioLeft
    removeNews event $ PercentileChance 25


-- | If left unchecked, kragii worms will eat crops from fields
-- They prefer potatoe plants and will not touch beans or paprikas
-- Different crop types aren't currently modeled in the game, so we just
-- use up 100 units of biological resources. There's chance that worms
-- will eventually leave all by themselves.
noChoice :: (MonadIO m, PersistQueryWrite backend, BaseBackend backend ~ SqlBackend) =>
            (Key News, KragiiWormsEvent) -> MaybeT (WriterT [KragiiResults] (ReaderT backend m)) EventRemoval
noChoice (_, event) = do
    faction <- getFaction event
    (cost, bioLeft) <- calculateNewBio (RawResource 100) (entityVal faction)
    _ <- destroyCrops faction cost bioLeft
    removeNews event $ PercentileChance 10


-- | retrieve current owner (faction) of planet where kragii attack is in progress
getFaction :: ( MonadIO m, PersistStoreRead backend, BaseBackend backend ~ SqlBackend ) =>
              KragiiWormsEvent -> MaybeT (WriterT [KragiiResults] (ReaderT backend m)) (Entity Faction)
getFaction event = MaybeT $ do
    planet <- lift $ get $ kragiiWormsPlanetId event
    let owner = planet >>= planetOwnerId
    res <- lift $ mapM getEntity owner
    return $ join res


-- | Amount of biological resources left after consuming given amount
-- first element of the tuple is cost and second one amount left after applying cost
calculateNewBio :: Monad m =>
                   RawResource Biological -> Faction -> MaybeT (WriterT [KragiiResults] m) (RawResource Biological, RawResource Biological)
calculateNewBio cost faction = MaybeT $ do
    let currentBio = factionBiologicals faction
    return $ if currentBio > 0
                then Just ( cost
                          , max 0 (currentBio - cost))
                else Nothing


-- | Update biologicals stockpile of the faction that is target of kragii attack
destroyCrops :: ( MonadIO m, PersistQueryWrite backend, BaseBackend backend ~ SqlBackend ) =>
                Entity Faction -> RawResource Biological
                -> RawResource Biological -> MaybeT (WriterT [KragiiResults] (ReaderT backend m)) ()
destroyCrops faction cost bioLeft = MaybeT $ do
    _ <- lift $ updateWhere [ FactionId ==. entityKey faction ]
                            [ FactionBiologicals =. bioLeft ]
    tell [ CropsDestroyed cost ]
    return $ Just ()


-- | Roll a die and see if kragii worms are removed from play
removeNews :: ( PersistStoreWrite backend, PersistQueryWrite backend, MonadIO m
              , BaseBackend backend ~ SqlBackend ) =>
              KragiiWormsEvent -> PercentileChance -> MaybeT (WriterT [KragiiResults] (ReaderT backend m)) EventRemoval
removeNews event odds = MaybeT $ do
    res <- liftIO $ roll odds
    case res of
        Success -> do
            _ <- lift $ deleteWhere [ PlanetStatusPlanetId ==. kragiiWormsPlanetId event
                                    , PlanetStatusStatus ==. KragiiAttack
                                    ]
            _ <- tell [ WormsRemoved ]
            return $ Just RemoveOriginalEvent
        Failure -> do
            _ <- tell [ WormsStillPresent ]
            return $ Just KeepOriginalEvent


instance ToDto KragiiWormsChoice KragiiWormsChoiceDto where
    toDto choice =
        case choice of
            EvadeWorms -> EvadeWormsDto
            AttackWorms -> AttackWormsDto
            TameWorms -> TameWormsDto


instance FromDto KragiiWormsChoice KragiiWormsChoiceDto where
    fromDto dto =
        case dto of
            EvadeWormsDto -> EvadeWorms
            AttackWormsDto -> AttackWorms
            TameWormsDto -> TameWorms


instance ToDto (UserOption KragiiWormsChoice) (UserOptionDto KragiiWormsChoiceDto) where
    toDto option =
        UserOptionDto
            { userOptionDtoTitle = userOptionTitle option
            , userOptionDtoExplanation = userOptionExplanation option
            , userOptionDtoChoice = toDto $ userOptionChoice option
            }


instance ToDto KragiiNews KragiiNewsDto where
    toDto event =
        KragiiNewsDto
            { kragiiNewsDtoPlanetId = kragiiNewsPlanetId event
            , kragiiNewsDtoPlanetName = kragiiNewsPlanetName event
            , kragiiNewsDtoSystemId = kragiiNewsSystemId event
            , kragiiNewsDtoSystemName = kragiiNewsSystemName event
            , kragiiNewsDtoResolution = kragiiNewsExplanation event
            , kragiiNewsDtoDate = kragiiNewsDate event
            }


instance FromDto KragiiNews KragiiNewsDto where
    fromDto dto =
        KragiiNews
        { kragiiNewsPlanetId = kragiiNewsDtoPlanetId dto
        , kragiiNewsPlanetName = kragiiNewsDtoPlanetName dto
        , kragiiNewsSystemId = kragiiNewsDtoSystemId dto
        , kragiiNewsSystemName = kragiiNewsDtoSystemName dto
        , kragiiNewsExplanation = kragiiNewsDtoResolution dto
        , kragiiNewsDate = kragiiNewsDtoDate dto
        }


$(deriveJSON defaultOptions ''KragiiWormsEvent)
$(deriveJSON defaultOptions ''KragiiWormsChoice)
$(deriveJSON defaultOptions ''KragiiNews)
