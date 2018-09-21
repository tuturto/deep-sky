{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Fleet where

import Import

getFleetR :: Handler Html
getFleetR = do
    defaultLayout $ do
        setTitle "Deep Sky - Fleet"
        $(widgetFile "fleet")
