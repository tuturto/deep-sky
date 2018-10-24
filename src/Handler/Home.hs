{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = 
    defaultLayout $ do
        setTitle "Deep sky"
        $(widgetFile "homepage")

getNewHomeR :: Handler Html
getNewHomeR =
    defaultLayout $ do
        setTitle "Deep Sky - New Home"
        addScript $ StaticR js_client_js
        addStylesheet $ StaticR css_site_css
        $(widgetFile "newhome")
