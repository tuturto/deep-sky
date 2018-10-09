{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.AdminPanel where

import Import
import MenuHelpers
import Simulation.Main (processTurn)

getAdminPanelR :: Handler Html
getAdminPanelR = 
    defaultLayout $ do
        setTitle "Deep Sky - Admin"
        $(widgetFile "admin/adminpanel")

getAdminAdvanceTimeR :: Handler Html
getAdminAdvanceTimeR = do
    newTime <- runDB processTurn

    defaultLayout $ do
        setTitle "Deep Sky - Admin"
        $(widgetFile "admin/advancetime")
