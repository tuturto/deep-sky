{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}

module Events.Pets
    ( ScurryingSoundsEvent(..), ScurryingSoundsChoice(..)
    , ScurryingSoundsResult(..), ScurryingSoundsNews(..)
    , NamingPetEvent(..), NamingPetChoice(..), NamingPetResult(..)
    , NamingPetNews(..) )
    where


import Import
import Control.Monad.Trans.Maybe ( MaybeT(..), runMaybeT )
import Control.Monad.Trans.Writer ( WriterT, runWriterT, tell )
import Control.Monad.Random ( evalRand )
import Data.Aeson.TH
import System.Random ( getStdGen )
import Common ( ToDto(..), FromDto(..) )
import CustomTypes ( StarDate, RollResult(..), PercentileChance(..), roll )
import Dto.News ( ScurryingSoundsNewsDto(..), ScurryingSoundsChoiceDto(..)
                , UserOptionDto(..), NamingPetNewsDto(..)
                , NamingPetChoiceDto(..) )
import Events.Creation ( EventCreation(..) )
import Events.Import ( SpecialEvent(..), EventRemoval(..), UserOption(..)
                     , EventResolveType(..) )
import MenuHelpers ( starDate )
import Names ( petNameM )
import People.Data ( PetType(..), PetName(..) )


-- | Character hears scurrying sounds inside their walls
data ScurryingSoundsEvent = ScurryingSoundsEvent
    { scurryingSoundsEventPersonId :: !(Key Person)
    , scurryingSoundsEventDate :: !StarDate
    } deriving (Show, Read, Eq)


-- | Options for dealing with scurrying sounds
data ScurryingSoundsChoice
    = GetCat
    | TameRat
    | GetRidSomehowElse
    deriving (Show, Read, Eq)


instance FromDto ScurryingSoundsChoice ScurryingSoundsChoiceDto where
    fromDto = \case
                GetCatDto ->
                    GetCat

                TameRatDto ->
                    TameRat

                GetRidSomehowElseDto ->
                    GetRidSomehowElse


instance ToDto ScurryingSoundsChoice ScurryingSoundsChoiceDto where
    toDto = \case
                GetCat ->
                    GetCatDto

                TameRat ->
                    TameRatDto

                GetRidSomehowElse ->
                    GetRidSomehowElseDto


instance ToDto (UserOption ScurryingSoundsChoice) (UserOptionDto ScurryingSoundsChoiceDto) where
    toDto option =
        UserOptionDto
            { userOptionDtoTitle = userOptionTitle option
            , userOptionDtoExplanation = userOptionExplanation option
            , userOptionDtoChoice = toDto $ userOptionChoice option
            }


-- | End result for scurrying sounds
data ScurryingSoundsResult
    = PetObtained PetType (Key Pet)
    | TooManyPets
    | CrittersRemoved
    | SoundsStoppedByThemselves
    deriving (Show, Read, Eq)


-- | News article explaining end resolution of scurrying sounds
data ScurryingSoundsNews = ScurryingSoundsNews
    { scurryingSoundsNewsExplanation :: !Text
    , scurryingSoundsNewsPetId :: !(Maybe (Key Pet))
    , scurryingSoundsNewsPetType :: !(Maybe PetType)
    , scurryingSoundsNewsDate :: !StarDate
    } deriving (Show, Read, Eq)


instance ToDto ScurryingSoundsNews ScurryingSoundsNewsDto where
    toDto ScurryingSoundsNews{..} =
        ScurryingSoundsNewsDto
            { scurryingSoundsNewsDtoExplanation = scurryingSoundsNewsExplanation
            , scurryingSoundsNewsDtoPetId = scurryingSoundsNewsPetId
            , scurryingSoundsNewsDtoPetType = scurryingSoundsNewsPetType
            , scurryingSoundsNewsDtoDate = scurryingSoundsNewsDate
            }


instance FromDto ScurryingSoundsNews ScurryingSoundsNewsDto where
    fromDto ScurryingSoundsNewsDto{..} =
        ScurryingSoundsNews
            { scurryingSoundsNewsExplanation = scurryingSoundsNewsDtoExplanation
            , scurryingSoundsNewsPetId = scurryingSoundsNewsDtoPetId
            , scurryingSoundsNewsPetType = scurryingSoundsNewsDtoPetType
            , scurryingSoundsNewsDate = scurryingSoundsNewsDtoDate
            }


instance SpecialEvent ScurryingSoundsEvent ScurryingSoundsChoice ScurryingSoundsResult where
    eventOptions _ = [ UserOption { userOptionTitle = "Get a cat"
                                  , userOptionExplanation = [ "Get a cat and hope it will get rid of critters making sounds." ]
                                  , userOptionChoice = GetCat
                                  }
                     , UserOption { userOptionTitle = "Tame critter making sounds"
                                  , userOptionExplanation = [ "See if you can tame the animal that is making the sounds." ]
                                  , userOptionChoice = TameRat
                                  }
                     , UserOption { userOptionTitle = "Let someone else dispose of the noises"
                                  , userOptionExplanation = [ "Let someone else deal with the problem." ]
                                  , userOptionChoice = GetRidSomehowElse
                                  }
                     ]

    resolveType _ =
        DelayedEvent

    resolveEvent keyEventPair (Just choice) =
        runWriterT . runMaybeT $
            case choice of
                    GetCat ->
                        chooseToGetCat keyEventPair

                    TameRat ->
                        chooseToTame keyEventPair

                    GetRidSomehowElse ->
                        chooseToGetRidOf

    resolveEvent keyEventPair Nothing =
        runWriterT . runMaybeT $ noChoice keyEventPair


-- | Person has chosen to get a cat
-- this will stop the noises and person will acquire a new pet, if maximum
-- amount of pets hasn't been exceeded
chooseToGetCat :: (MonadIO m, PersistQueryWrite backend
    , BaseBackend backend ~ SqlBackend) =>
    (Key News, ScurryingSoundsEvent)
    -> MaybeT (WriterT [ScurryingSoundsResult] (ReaderT backend m)) (EventRemoval, [EventCreation])
chooseToGetCat (_, event) = do
    date <- (lift . lift) starDate
    person <- getPerson event
    peId <- addPet person date Cat
    let cEvent = NamingPet <$> Just (entityKey person)
                           <*> peId
    return (RemoveOriginalEvent, maybeToList cEvent)


-- | Person has chosen to tame the rate
-- this will stop the noises and person will acquire a new pet, if maximum
-- amount of pets hasn't been exceeded
chooseToTame :: (MonadIO m, PersistQueryWrite backend
    , BaseBackend backend ~ SqlBackend) =>
    (Key News, ScurryingSoundsEvent)
    -> MaybeT (WriterT [ScurryingSoundsResult] (ReaderT backend m)) (EventRemoval, [EventCreation])
chooseToTame (_, event) = do
    date <- (lift . lift) starDate
    person <- getPerson event
    peId <- addPet person date Rat
    let cEvent = NamingPet <$> Just (entityKey person)
                           <*> peId
    return (RemoveOriginalEvent, maybeToList cEvent)


-- | Person has chosen to get rid of critters. Noises will stop.
chooseToGetRidOf :: (MonadIO m, PersistQueryWrite backend
    , BaseBackend backend ~ SqlBackend) =>
    MaybeT (WriterT [ScurryingSoundsResult] (ReaderT backend m)) (EventRemoval, [EventCreation])
chooseToGetRidOf = do
    lift $ tell [ CrittersRemoved ]
    return (RemoveOriginalEvent, [])


-- | Handle case where person did not made any choice
-- there's small chance that noises stop by themselves and amount of existing
-- pets adds to this chance
noChoice :: (MonadIO m, PersistQueryWrite backend, PersistQueryRead backend
    , BaseBackend backend ~ SqlBackend) =>
    (Key News, ScurryingSoundsEvent)
    -> MaybeT (WriterT [ScurryingSoundsResult] (ReaderT backend m)) (EventRemoval, [EventCreation])
noChoice (_, event) = do
    pets <- (lift . lift) $ selectList [ PetOwnerId ==. (scurryingSoundsEventPersonId event)
                                       , PetDateOfDeath ==. Nothing
                                       ] []
    let odds = PercentileChance $ 15 * (length pets) + 15
    res <- liftIO $ roll odds
    case res of
        Success -> do
            lift $ tell [ SoundsStoppedByThemselves ]
            return (RemoveOriginalEvent, [])
        Failure -> do
            return (KeepOriginalEvent, [])


-- | Get person information from database
getPerson :: ( MonadIO m, PersistStoreRead backend
    , BaseBackend backend ~ SqlBackend ) =>
    ScurryingSoundsEvent
    -> MaybeT (WriterT [ScurryingSoundsResult] (ReaderT backend m)) (Entity Person)
getPerson event = MaybeT $ do
    person <- lift $ getEntity $ scurryingSoundsEventPersonId event
    -- TODO: check that person isn't dead
    return person


-- | Tries to create a new pet and give to a person
-- if person already has maximum amount of pets, pet is not given
addPet :: (PersistQueryRead backend, MonadIO m,
    PersistStoreWrite backend, BaseBackend backend ~ SqlBackend) =>
    Entity Person
    -> StarDate
    -> PetType
    -> MaybeT (WriterT [ScurryingSoundsResult] (ReaderT backend m)) (Maybe (Key Pet))
addPet person date pType = MaybeT $ do
    pets <- lift $ selectList [ PetOwnerId ==. (entityKey person)
                              , PetDateOfDeath ==. Nothing
                              ] []
    if (length pets) >= 5
        then do
            tell [ TooManyPets ]
            return (Just Nothing)
        else do
            pId <- lift $ insert $ Pet { petType = pType
                                       , petName = petTypeToName pType
                                       , petDateOfBirth = date - 6
                                       , petDateOfDeath = Nothing
                                       , petOwnerId = entityKey person
                                       }
            tell [ PetObtained pType pId ]

            return $ Just $ Just pId


petTypeToName :: PetType -> PetName
petTypeToName Cat = "Cat"
petTypeToName Rat = "Rat"


data NamingPetEvent = NamingPetEvent
    { namingPetEventPersonId :: !(Key Person)
    , namingPetEventPetId :: !(Key Pet)
    , namingPetEventPetType :: !PetType
    , namingPetEventDate :: !StarDate
    , namingPetNameOptions :: ![PetName]
    } deriving (Show, Read, Eq)


data NamingPetChoice
    = GiveName PetName
    | LetSomeoneElseDecide
    deriving (Show, Read, Eq)


instance FromDto NamingPetChoice NamingPetChoiceDto where
    fromDto =
        \case
            (GiveNameDto name) ->
                GiveName name

            LetSomeoneElseDecideDto ->
                LetSomeoneElseDecide


instance ToDto NamingPetChoice NamingPetChoiceDto where
    toDto =
        \case
            (GiveName name) ->
                GiveNameDto name

            LetSomeoneElseDecide ->
                LetSomeoneElseDecideDto


-- TODO: deduplicate
instance ToDto (UserOption NamingPetChoice) (UserOptionDto NamingPetChoiceDto) where
    toDto option =
        UserOptionDto
            { userOptionDtoTitle = userOptionTitle option
            , userOptionDtoExplanation = userOptionExplanation option
            , userOptionDtoChoice = toDto $ userOptionChoice option
            }


data NamingPetResult
    = PetNamed (Key Pet) PetName
    | RandomNameGiven (Key Pet) PetName
    deriving (Show, Read, Eq)


data NamingPetNews = NamingPetNews
    { namingPetNewsExplanation :: !Text
    , namingPetNewsPetId :: !(Key Pet)
    , namingPetNewsPetType :: !PetType
    , namingPetNewsDate :: !StarDate
    } deriving (Show, Read, Eq)


instance ToDto NamingPetNews NamingPetNewsDto where
    toDto NamingPetNews{..} =
        NamingPetNewsDto
            { namingPetNewsDtoExplanation = namingPetNewsExplanation
            , namingPetNewsDtoPetId = namingPetNewsPetId
            , namingPetNewsDtoPetType = namingPetNewsPetType
            , namingPetNewsDtoDate = namingPetNewsDate
            }


instance FromDto NamingPetNews NamingPetNewsDto where
    fromDto NamingPetNewsDto{..} =
        NamingPetNews
            { namingPetNewsExplanation = namingPetNewsDtoExplanation
            , namingPetNewsPetId = namingPetNewsDtoPetId
            , namingPetNewsPetType = namingPetNewsDtoPetType
            , namingPetNewsDate = namingPetNewsDtoDate
            }


instance SpecialEvent NamingPetEvent NamingPetChoice NamingPetResult where
    eventOptions event =
        --in this particular case, options available to user are stored in event
        (petNameOption <$> namingPetNameOptions event)
            ++ [ UserOption { userOptionTitle = "Let someone else decide"
                            , userOptionExplanation = [ "Your pet will get a random name" ]
                            , userOptionChoice = LetSomeoneElseDecide
                            }
               ]

    resolveType _ =
        ImmediateEvent

    resolveEvent keyEventPair (Just choice) =
        runWriterT . runMaybeT $ chooseToGiveName keyEventPair choice

    resolveEvent keyEventPair Nothing =
        runWriterT . runMaybeT $ chooseToGiveName keyEventPair LetSomeoneElseDecide


chooseToGiveName :: (MonadIO m, PersistQueryWrite backend
    , BaseBackend backend ~ SqlBackend) =>
    (Key News, NamingPetEvent)
    -> NamingPetChoice
    -> MaybeT (WriterT [NamingPetResult] (ReaderT backend m)) (EventRemoval, [EventCreation])
chooseToGiveName (_, event) (GiveName name) = do
    pets <- (lift . lift) $ selectList [ PetName ==. name
                                       , PetOwnerId ==. namingPetEventPersonId event
                                       ] []
    let pId = namingPetEventPetId event

    if null pets
        then do
            lift $ tell [ PetNamed pId name ]
            (lift . lift) $ update pId [ PetName =. name ]
            return (RemoveOriginalEvent, [])
        else do
            g <- liftIO getStdGen
            lift $ tell [ RandomNameGiven pId (evalRand petNameM g) ]
            return (RemoveOriginalEvent, [])

chooseToGiveName (_, event) LetSomeoneElseDecide = do
    let pId = namingPetEventPetId event
    g <- liftIO getStdGen
    lift $ tell [ RandomNameGiven pId (evalRand petNameM g) ]
    return (RemoveOriginalEvent, [])


-- | Map between pet name and user option
petNameOption :: PetName -> UserOption NamingPetChoice
petNameOption name =
    UserOption { userOptionTitle = unPetName name
               , userOptionExplanation = [ "Your pet will be named to " ++ (unPetName name) ]
               , userOptionChoice = GiveName name
               }


$(deriveJSON defaultOptions ''ScurryingSoundsEvent)
$(deriveJSON defaultOptions ''ScurryingSoundsChoice)
$(deriveJSON defaultOptions ''ScurryingSoundsNews)
$(deriveJSON defaultOptions ''NamingPetEvent)
$(deriveJSON defaultOptions ''NamingPetChoice)
$(deriveJSON defaultOptions ''NamingPetNews)
