{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.AdminPanel where

import Import
import Text.Blaze.Html5
import Report
import Widgets
import MenuHelpers

getAdminPanelR :: Handler Html
getAdminPanelR = do
    (userId, _) <- requireAuthPair   
    adminCheck <- isAdmin userId

    defaultLayout $ do
        setTitle "Deep Sky - Admin"
        $(widgetFile "admin/adminpanel")

getAdminAdvanceTimeR :: Handler Html
getAdminAdvanceTimeR = do
    (userId, _) <- requireAuthPair   
    adminCheck <- isAdmin userId

    defaultLayout $ do
        setTitle "Deep Sky - Admin"
        $(widgetFile "admin/advancetime")
