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

data EquipmentSlot = InnerSlot
                   | OuterSlot
                   | ArmourSlot
    deriving (Show, Read, Eq)
$(deriveJSON defaultOptions ''EquipmentSlot)

data EquipmentType = BridgeEquipment
                   | SensorEquipment
                   | EngineEquipment
    deriving (Show, Read, Eq)
$(deriveJSON defaultOptions ''EquipmentType)

data ComponentLevel = ComponentLevel { compLeLevel :: Int
                                     , compLeType :: EquipmentType }
    deriving (Show, Read, Eq)

instance ToJSON ComponentLevel where
    toJSON (ComponentLevel lvl compType) =
        object [ "level" .= lvl
               , "type" .= compType ]

data ComponentDto = ComponentDto { dCompId :: Int
                                 , dCompName :: String
                                 , dCompDescription :: String
                                 , dCompWeight :: Int 
                                 , dCompSlots :: [EquipmentSlot] 
                                 , dCompType :: [ComponentLevel] }
    deriving (Show, Read, Eq)

instance ToJSON ComponentDto where
    toJSON (ComponentDto idKey name desc weight slots types) = 
        object [ "id" .= idKey
               , "name" .= name
               , "description" .= desc
               , "weight" .= weight
               , "slots" .= slots 
               , "types" .= array types ]

getApiComponentsR :: Handler Value
getApiComponentsR = do
    let json = toJSON [ ComponentDto 1 "Long range sensors" "Long range sensors let you see long" 1 [ OuterSlot ] [ ComponentLevel 1 SensorEquipment ]
                      , ComponentDto 2 "Engines" "Engines let you move" 2 [ OuterSlot ] [ ComponentLevel 1 EngineEquipment ]
                      , ComponentDto 3 "Armor" "Protects ship" 10 [ ArmourSlot ] []
                      , ComponentDto 4 "Bridge" "Control center of ship" 10 [ InnerSlot, OuterSlot ] [ ComponentLevel 1 BridgeEquipment ]
                      ]
    return json
