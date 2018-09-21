{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Fleet where

import Import
import Common (requireFaction)

getFleetR :: Handler Html
getFleetR = do
    (_, user, _) <- requireFaction
    defaultLayout $ do
        setTitle "Deep Sky - Fleet"
        $(widgetFile "fleet")
