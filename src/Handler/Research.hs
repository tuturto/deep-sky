{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Research where

import Import

getResearchR :: Handler Html
getResearchR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle "Deep Sky - Research"
        $(widgetFile "research")