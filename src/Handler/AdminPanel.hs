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
getAdminPanelR = do
    (userId, _) <- requireAuthPair   
    _ <- runDB $ isAdmin userId

    defaultLayout $ do
        setTitle "Deep Sky - Admin"
        $(widgetFile "admin/adminpanel")

getAdminAdvanceTimeR :: Handler Html
getAdminAdvanceTimeR = do
    (userId, _) <- requireAuthPair   
    _ <- runDB $ isAdmin userId
    (newTime) <- runDB processTurn

    defaultLayout $ do
        setTitle "Deep Sky - Admin"
        $(widgetFile "admin/advancetime")
