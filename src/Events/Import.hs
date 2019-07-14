{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE LambdaCase                 #-}

module Events.Import
    ( UserOption(..), SpecialEvent(..), EventRemoval(..), EventResolveType(..) )

    where

import Import
import Data.Aeson.TH

import Events.Creation


-- | General user option regarding a special event
data UserOption a =
    UserOption { userOptionTitle :: Text
               , userOptionExplanation :: [Text]
               , userOptionChoice :: a
               }
    deriving (Show, Read, Eq)


-- | Special events give users chance to react to them by selecting one
-- of the predefined user options. If user selects an option, consequences
-- are handled in resolveChoice function. In some cases users might forget
-- to select an option or wilfully choose not to choose anything. These
-- cases are handled by resolveNoChoice function.
class SpecialEvent a b c | a -> b, a-> c where

    -- | Options available for user when this special event occurs
    eventOptions :: a -> [UserOption b]

    -- | Is event resolved as soon as choice is made or when simulation is run
    resolveType :: a -> EventResolveType

    -- | Resolve special event according to choice the user made
    -- Function returns a list describing end result of resolution
    resolveEvent :: ( PersistQueryRead backend, PersistQueryWrite backend
                    , MonadIO m, BaseBackend backend ~ SqlBackend ) =>
                    (Key News, a)
                    -> Maybe b
                    -> ReaderT backend m (Maybe (EventRemoval, [EventCreation]), [c])


-- | Should event which is currently being handled removed from database
data EventRemoval =
    RemoveOriginalEvent
    | KeepOriginalEvent
    deriving (Show, Read, Eq)


-- | Is event resolved as soon as choice has been made
data EventResolveType =
    ImmediateEvent
    | DelayedEvent
    deriving (Show, Read, Eq)


$(deriveJSON defaultOptions ''EventResolveType)
$(deriveJSON defaultOptions ''UserOption)
