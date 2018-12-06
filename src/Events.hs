{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Events ( SpecialEvent(..), KragiiWormsEvent(..) )
    where

import Import
import Data.Aeson.TH

data UserOption a = UserOption a Text
    deriving (Show, Read, Eq)

data UserChoice = NoUserChoice
    | UserChoice Int
    deriving (Show, Read, Eq)

class SpecialEvent a b | a -> b where
    eventDescription :: a -> WidgetFor App ()
    eventOptions :: a -> [UserOption b]
    resolveChoice :: a -> b -> ()
    resolveNoChoice :: a -> ()

data KragiiWormsEvent = KragiiWormsEvent
    { kragiiWormsPlanetId :: Key Planet
    , kragiiWormsPlanetName :: Text
    , kragiiWormsSystemId :: Key StarSystem
    , kragiiWormsSystemName :: Text
    }
    deriving (Show, Read, Eq)

data KragiiWormsChoice = EvadeWorms
    | AttackWorms
    | TameWorms
    deriving (Show, Read, Eq)

instance SpecialEvent KragiiWormsEvent KragiiWormsChoice where
    eventDescription KragiiWormsEvent
        { kragiiWormsPlanetId = planetId
        , kragiiWormsPlanetName = pName
        , kragiiWormsSystemId = systemId
        , kragiiWormsSystemName = sName } =
            $(widgetFile "widgets/news/planetFoundW")

    eventOptions _ = [ UserOption EvadeWorms "Avoid the worms"
                     , UserOption AttackWorms "Attack worms and drive them away"
                     , UserOption TameWorms "Try and tame them"
                     ]

    resolveChoice KragiiWormsEvent {} _ =
        ()

    resolveNoChoice KragiiWormsEvent {} =
        ()


$(deriveJSON defaultOptions ''UserOption)
$(deriveJSON defaultOptions ''KragiiWormsEvent)
$(deriveJSON defaultOptions ''KragiiWormsChoice)
