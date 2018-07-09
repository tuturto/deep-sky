{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import
import MenuHelpers

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    mFac <- maybeFaction user
    faction <- case mFac of
                   Just fac -> return fac
                   Nothing  -> redirect FactionR

    defaultLayout $ do
        setTitle . toHtml $ "Deep Sky - " <> userIdent user <> "'s User page"
        $(widgetFile "profile")
