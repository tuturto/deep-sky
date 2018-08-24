{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Dto.Ship where

import Data.Aeson (object, (.=), (.:?))
import Import
import Components

data ChassisDto = ChassisDto { cdId :: Key Chassis
                             , cdName :: Text
                             , cdMaxTonnage :: Int
                             , cdRequiredTypes :: [ ComponentLevel ]}
    deriving Show

instance ToJSON ChassisDto where
    toJSON (ChassisDto idKey name maxTonnage types) =
        object [ "id" .= idKey
               , "name" .= name
               , "maxTonnage" .= maxTonnage
               , "requiredTypes" .= array types 
               ]

data ComponentDto = ComponentDto
    { saveComponentId :: ComponentId
    , saveComponentLevel :: Int
    } deriving Show

data InstalledComponentDto = InstalledComponentDto
    { saveInstalledComponentComponents :: ComponentDto
    , saveInstalledComponentAmount :: Int
    } deriving Show

data DesignDto = DesignDto
    { saveDesignId :: Maybe (Key Design)
    , saveDesignChassisId :: Key Chassis
    , saveDesignName :: Text
    , saveDesignComponents :: [ InstalledComponentDto ]
    } deriving Show

instance FromJSON ComponentDto where
    parseJSON (Object v) =
        ComponentDto <$> v .: "id"
                     <*> v .: "level"
    parseJSON _ = mzero

instance ToJSON ComponentDto where
    toJSON (ComponentDto cId level) =
        object [ "id" .= cId
               , "level" .= level
               ]

instance FromJSON InstalledComponentDto where
    parseJSON (Object v) =
        InstalledComponentDto <$> v .: "component"
                              <*> v .: "amount"
    parseJSON _ = mzero

instance ToJSON InstalledComponentDto where
    toJSON (InstalledComponentDto comp amount) =
        object [ "component" .= comp
               , "amount" .= amount 
               ]

instance FromJSON DesignDto where
    parseJSON (Object v) =
        DesignDto <$> v .:? "id"
                  <*> v .: "chassisId"
                  <*> v .: "name"
                  <*> v .: "components"
    parseJSON _ = mzero

instance ToJSON DesignDto where
    toJSON (DesignDto dId chassisId name components) =
        object [ "id" .= dId
               , "chassisId" .= chassisId
               , "name" .= name
               , "components" .= components
               ]

designToSaveDesign :: (Key Design, Design) -> [ Entity PlannedComponent ] -> DesignDto
designToSaveDesign (newId, design) comps = 
    DesignDto (Just newId) (designChassisId design) (designName design) $ map plannedComponentToSaveComponent comps

plannedComponentToSaveComponent :: Entity PlannedComponent -> InstalledComponentDto
plannedComponentToSaveComponent entity =
    InstalledComponentDto (ComponentDto (plannedComponentComponentId comp) (plannedComponentLevel comp)) (plannedComponentAmount comp)
    where
        comp = entityVal entity

saveComponentToPlannetComponent :: Key Design -> InstalledComponentDto -> PlannedComponent
saveComponentToPlannetComponent dId (InstalledComponentDto (ComponentDto cId level) amount) =
    PlannedComponent dId cId level amount
