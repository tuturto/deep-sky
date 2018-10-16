{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}

module Events 
    where

import Import

data UserOption a = UserOption a Text
    deriving (Show, Read, Eq)

data UserChoice = NoUserChoice
    | UserChoice Int
    deriving (Show, Read, Eq)

class SpecialEvent a b | a -> b where
    eventDescription :: a -> Text
    eventOptions :: a -> [UserOption b]
    eventHandler :: a -> b -> ()

data KragiiWormsEvent = KragiiWormsEvent
    | LargeKragiiWormsEvent
    deriving (Show, Read, Eq)

data KragiiWormsChoice = EvadeWorms
    | AttackWorms
    | TameWorms
    deriving (Show, Read, Eq)

instance SpecialEvent KragiiWormsEvent KragiiWormsChoice where
    eventDescription KragiiWormsEvent = "Group of kragii has appeared!"
    eventDescription LargeKragiiWormsEvent = "Large group of kragii has appeared!"

    eventOptions _ = [ UserOption EvadeWorms "Avoid the worms"
                     , UserOption AttackWorms "Attack worms and drive them away"
                     , UserOption TameWorms "Try and tame them"
                     ]

    eventHandler _ EvadeWorms = ()
    eventHandler _ AttackWorms = ()
    eventHandler _ TameWorms = ()
