{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}

module Dto.Ship
    where

import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import Import
import Vehicles.Components ( ComponentId, ComponentPower, ChassisType, SlotAmount
                           , ChassisName, Weight, ComponentLevel, ComponentAmount )


data ChassisDto = ChassisDto
    { chassisDtoId :: Key Chassis
    , chassisDtoName :: ChassisName
    , chassisDtoType :: ChassisType
    , chassisDtoMaxTonnage :: Weight
    , chassisDtoRequiredTypes :: [ RequiredComponentDto ]
    , chassisDtoArmourSlots :: SlotAmount
    , chassisDtoInnerSlots :: SlotAmount
    , chassisDtoOuterSlots :: SlotAmount
    , chassisDtoSensorSlots :: SlotAmount
    , chassisDtoWeaponSlots :: SlotAmount
    , chassisDtoEngineSlots :: SlotAmount
    , chassisDtoMotiveSlots :: SlotAmount
    , chassisDtoSailSlots :: SlotAmount
    }
    deriving (Show, Read, Eq)


data RequiredComponentDto = RequiredComponentDto
    { requiredComponentDtoPower :: ComponentPower
    , requiredComponentDtoAmount :: ComponentAmount
    }
    deriving (Show, Read, Eq)


data PlannedComponentDto = PlannedComponentDto
    { plannedComponentDtoId :: ComponentId
    , plannedComponentDtoLevel :: ComponentLevel
    , plannedComponentDtoAmount :: ComponentAmount
    }
    deriving (Show, Read, Eq)


data DesignDto = DesignDto
    { designDtoId :: Maybe (Key Design)
    , designDtoChassisId :: Key Chassis
    , designDtoChassisLevel :: Int
    , designDtoName :: Text
    , designDtoComponents :: [ PlannedComponentDto ]
    }
    deriving (Show, Read, Eq)


designToDesignDto :: (Key Design, Design) -> [ Entity PlannedComponent ] -> DesignDto
designToDesignDto (newId, Design{..}) comps =
    -- TODO: chassis level
    DesignDto (Just newId) designChassisId 1 designName $ fmap (plannedComponentToComponentDto . entityVal) comps


plannedComponentToComponentDto :: PlannedComponent -> PlannedComponentDto
plannedComponentToComponentDto PlannedComponent{..} =
    PlannedComponentDto plannedComponentComponentId plannedComponentLevel plannedComponentAmount


componentDtoToPlannedComponent :: Key Design -> PlannedComponentDto -> PlannedComponent
componentDtoToPlannedComponent dId (PlannedComponentDto {..}) =
    PlannedComponent dId plannedComponentDtoId plannedComponentDtoLevel plannedComponentDtoAmount


$(deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''ChassisDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 19 } ''PlannedComponentDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 9 } ''DesignDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 20 } ''RequiredComponentDto)

