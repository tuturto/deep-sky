{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Bases where

import Import

getBasesR :: Handler Html
getBasesR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle "Deep Sky - Bases"
        $(widgetFile "bases")
