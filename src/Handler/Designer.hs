{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Designer where

import Data.Aeson (object, (.=))
import Import

getDesignerR :: Handler Html
getDesignerR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    defaultLayout $ do
        setTitle "Deep Sky - Designs"
        $(widgetFile "shipdesigns")

getNewDesignR :: Handler Html
getNewDesignR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    defaultLayout $ do
        setTitle "Deep Sky - Ship designer"
        addScript $ StaticR js_shipdesigner_js
        addStylesheet $ StaticR css_site_css
        $(widgetFile "shipdesigner")

data ComponentDto = ComponentDto { cdName :: String
                                 , cdDescription :: String
                                 , cdWeight :: Int }

instance ToJSON ComponentDto where
    toJSON (ComponentDto name desc weight) = object [ "name" .= name
                                                    , "desc" .= desc
                                                    , "weight" .= weight ]

getApiComponentsR :: Handler Value
getApiComponentsR = do
    let obj1 = ComponentDto "Long range sensors" "Long range sensors let you see long" 1
    let obj2 = ComponentDto "Engines" "Engines let you move" 2
    let objList = [ obj1, obj2 ]
    let json = toJSON objList
    return json
