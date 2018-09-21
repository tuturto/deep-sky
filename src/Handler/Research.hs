{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Research where

import Import
import Common (requireFaction)

getResearchR :: Handler Html
getResearchR = do
    (_, user, _) <- requireFaction
    defaultLayout $ do
        setTitle "Deep Sky - Research"
        $(widgetFile "research")
