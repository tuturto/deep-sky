{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Handler.Faction
    ( getFactionR )
    where

import Import
import Handler.Home ( getNewHomeR )


getFactionR :: Handler Html
getFactionR = getNewHomeR
