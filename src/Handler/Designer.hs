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
                   | SensorSlot
                   | WeaponSlot
                   | EngineSlot
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
                                 , dCompSlot :: EquipmentSlot
                                 , dCompType :: [ComponentLevel] 
                                 , dCompCost :: ComponentCostDto }
    deriving (Show, Read, Eq)

data ComponentCostDto = ComponentCostDto { ccdMechanicCost :: Int
                                         , ccdBiologicalCost :: Int
                                         , ccdChemicalCost :: Int }
    deriving (Show, Read, Eq)

instance ToJSON ComponentCostDto where
    toJSON (ComponentCostDto mech bio chem) =
        object [ "mechanical" .= mech
               , "biological" .= bio
               , "chemical" .= chem ]

instance ToJSON ComponentDto where
    toJSON (ComponentDto idKey name desc weight slots types cost) = 
        object [ "id" .= idKey
               , "name" .= name
               , "description" .= desc
               , "weight" .= weight
               , "slot" .= slots 
               , "types" .= array types
               , "cost" .= cost ]

data ChassisDto = ChassisDto { cdId :: Int
                             , cdName :: String
                             , cdMaxTonnage :: Int
                             , cdRequiredTypes :: [ComponentLevel]}
    deriving (Show, Read, Eq)

instance ToJSON ChassisDto where
    toJSON (ChassisDto idKey name maxTonnage types) =
        object [ "id" .= idKey
               , "name" .= name
               , "maxTonnage" .= maxTonnage
               , "requiredTypes" .= array types 
               ]

data SaveDesign = SaveDesign { sdName :: String
                             , sdComponents :: [ InstalledComponent ]
                             , sdChassisId :: Int }
    deriving (Show, Read, Eq)

instance ToJSON SaveDesign where
    toJSON (SaveDesign name components chassisId) =
        object [ "chassis" .= chassisId
               , "name" .= name
               , "components" .= array components ]

-- TODO: component id to real key
data InstalledComponent = InstalledComponent { icComponentId :: Int
                                             , icAmount :: Int }
    deriving (Show, Read, Eq)

instance ToJSON InstalledComponent where
    toJSON (InstalledComponent cid camount) =
        object [ "id" .= cid
               , "amount" .= camount ]
    
getApiComponentsR :: Handler Value
getApiComponentsR = do
    let json = toJSON [ ComponentDto 1 "Long range sensors" "Long range sensors let you see long" 1 SensorSlot [ ComponentLevel 1 SensorEquipment ]
                            (ComponentCostDto 5 0 1)
                      , ComponentDto 2 "Engines" "Engines let you move" 2 EngineSlot [ ComponentLevel 1 EngineEquipment ]
                            (ComponentCostDto 15 0 10)
                      , ComponentDto 3 "Armor" "Protects ship" 10 ArmourSlot []
                            (ComponentCostDto 20 0 0)
                      , ComponentDto 4 "Bridge" "Control center of ship" 10 InnerSlot [ ComponentLevel 1 BridgeEquipment ]
                            (ComponentCostDto 10 5 10)
                      ]
    return json

getApiChassisR :: Handler Value
getApiChassisR = do
    let json = toJSON [ ChassisDto 1 "Destroyer" 150 [ ComponentLevel 1 BridgeEquipment 
                                                     , ComponentLevel 1 EngineEquipment ]
                      , ChassisDto 2 "Satellite" 20 [ ]
                      ]
    return json

postApiDesignR :: Handler Value
postApiDesignR = do
    let json = toJSON $ SaveDesign "S.S. Kickstart" [] 1
    return json
