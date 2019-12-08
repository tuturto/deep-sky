{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}

module Events.Creation
    ( EventCreation(..) )
    where

import Import

data EventCreation
    = NamingPet PersonId PetId
    deriving (Show, Read, Eq)
