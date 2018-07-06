{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Construction where

import Import

getConstructionR :: Handler Html
getConstructionR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle "Deep Sky - Construction"
        $(widgetFile "construction")
