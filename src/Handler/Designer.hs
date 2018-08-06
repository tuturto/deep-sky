{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Designer where

import Data.Aeson (object, (.=))
import Data.Aeson.TH
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

data ComponentDto = ComponentDto { dCompId :: Int
                                 , dCompName :: String
                                 , dCompDescription :: String
                                 , dCompWeight :: Int 
                                 , dCompSlots :: [EquipmentSlot] }
    deriving (Show, Read, Eq)

instance ToJSON ComponentDto where
    toJSON (ComponentDto id name desc weight slots) = 
        object [ "id" .= id
               , "name" .= name
               , "description" .= desc
               , "weight" .= weight
               , "slots" .= slots ]

data EquipmentSlot = InnerSlot
                   | OuterSlot
                   | ArmourSlot
    deriving (Show, Read, Eq)
$(deriveJSON defaultOptions ''EquipmentSlot)

getApiComponentsR :: Handler Value
getApiComponentsR = do
    let json = toJSON [ ComponentDto 1 "Long range sensors" "Long range sensors let you see long" 1 [ OuterSlot ]
                      , ComponentDto 2"Engines" "Engines let you move" 2 [ OuterSlot ] 
                      , ComponentDto 3 "Armor" "Protects ship" 10 [ ArmourSlot ]
                      , ComponentDto 4 "Bridge" "Control center of ship" 10 [ InnerSlot, OuterSlot ]
                      ]
    return json
