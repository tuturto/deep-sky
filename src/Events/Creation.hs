{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}

module Events.Creation
    ( EventCreation(..) )
    where

import Import

data EventCreation
    = NamingPet (Key Person) (Key Pet)
    deriving (Show, Read, Eq)
