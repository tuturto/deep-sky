{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Faction where

import Import

getFactionR :: Handler Html
getFactionR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle "Deep Sky - Faction"
        $(widgetFile "faction")
