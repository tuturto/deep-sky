{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Profile
    ( getProfileR )
    where

import Import
import Handler.Home ( getNewHomeR )

getProfileR :: Handler Html
getProfileR = getNewHomeR
