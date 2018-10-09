{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Dto.Ship where

import Data.Aeson (object, (.=), (.:?))
import Import
import Components (ComponentLevel, ComponentId)

data ChassisDto = ChassisDto { chassisDtoId :: Key Chassis
                             , chassisDtoName :: Text
                             , chassisDtoMaxTonnage :: Int
                             , chassisDtoRequiredTypes :: [ ComponentLevel ]}
    deriving Show

instance ToJSON ChassisDto where
    toJSON (ChassisDto idKey name maxTonnage types) =
        object [ "id" .= idKey
               , "name" .= name
               , "maxTonnage" .= maxTonnage
               , "requiredTypes" .= array types 
               ]

data ComponentDto = ComponentDto
    { componentDtoId :: ComponentId
    , componentDtoLevel :: Int
    } deriving Show

data InstalledComponentDto = InstalledComponentDto
    { installedComponentDtoComponents :: ComponentDto
    , installedComponentDtoAmount :: Int
    } deriving Show

data DesignDto = DesignDto
    { designDtoId :: Maybe (Key Design)
    , designDtoChassisId :: Key Chassis
    , designDtoName :: Text
    , designDtoComponents :: [ InstalledComponentDto ]
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

designToDesignDto :: (Key Design, Design) -> [ Entity PlannedComponent ] -> DesignDto
designToDesignDto (newId, design) comps = 
    DesignDto (Just newId) (designChassisId design) (designName design) $ map plannedComponentToComponentDto comps

plannedComponentToComponentDto :: Entity PlannedComponent -> InstalledComponentDto
plannedComponentToComponentDto entity =
    InstalledComponentDto (ComponentDto (plannedComponentComponentId comp) (plannedComponentLevel comp)) (plannedComponentAmount comp)
    where
        comp = entityVal entity

componentDtoToPlannedComponent :: Key Design -> InstalledComponentDto -> PlannedComponent
componentDtoToPlannedComponent dId (InstalledComponentDto (ComponentDto cId level) amount) =
    PlannedComponent dId cId level amount
