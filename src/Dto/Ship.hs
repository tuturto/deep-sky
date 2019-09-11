{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}

module Dto.Ship ( ChassisDto(..), RequiredComponentDto(..)
                , PlannedComponentDto(..), DesignDto(..), designToDesignDto
                , plannedComponentToComponentDto, toRequirement
                , componentDtoToPlannedComponent, toChassisDto
                )
    where

import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import Import
import Vehicles.Components ( ComponentId, ComponentPower, ChassisType, SlotAmount
                           , ChassisName, Weight, ComponentLevel, ComponentAmount
                           , ComponentPower(..) )


data ChassisDto = ChassisDto
    { chassisDtoId :: !(Key Chassis)
    , chassisDtoName :: !ChassisName
    , chassisDtoType :: !ChassisType
    , chassisDtoMaxTonnage :: !Weight
    , chassisDtoRequiredTypes :: ![ RequiredComponentDto ]
    , chassisDtoArmourSlots :: !SlotAmount
    , chassisDtoInnerSlots :: !SlotAmount
    , chassisDtoOuterSlots :: !SlotAmount
    , chassisDtoSensorSlots :: !SlotAmount
    , chassisDtoWeaponSlots :: !SlotAmount
    , chassisDtoEngineSlots :: !SlotAmount
    , chassisDtoMotiveSlots :: !SlotAmount
    , chassisDtoSailSlots :: !SlotAmount
    } deriving (Show, Read, Eq)


data RequiredComponentDto = RequiredComponentDto
    { requiredComponentDtoPower :: !ComponentPower
    , requiredComponentDtoAmount :: !ComponentAmount
    } deriving (Show, Read, Eq)


data PlannedComponentDto = PlannedComponentDto
    { plannedComponentDtoId :: !ComponentId
    , plannedComponentDtoLevel :: !ComponentLevel
    , plannedComponentDtoAmount :: !ComponentAmount
    } deriving (Show, Read, Eq)


data DesignDto = DesignDto
    { designDtoId :: !(Maybe (Key Design))
    , designDtoChassisId :: !(Key Chassis)
    , designDtoChassisLevel :: !Int
    , designDtoName :: !Text
    , designDtoComponents :: ![ PlannedComponentDto ]
    } deriving (Show, Read, Eq)


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


-- | Map chassis and required component information into chassis dto
toChassisDto :: (Entity Chassis, [Entity RequiredComponent]) -> ChassisDto
toChassisDto (chassis, reqs) =
    ChassisDto
        { chassisDtoId = entityKey chassis
        , chassisDtoName = chassisName . entityVal $ chassis
        , chassisDtoType = chassisType . entityVal $ chassis
        , chassisDtoMaxTonnage = chassisTonnage . entityVal $ chassis
        , chassisDtoRequiredTypes = toRequirement . entityVal <$> reqs
        , chassisDtoArmourSlots = chassisArmourSlots . entityVal $ chassis
        , chassisDtoInnerSlots = chassisInnerSlots . entityVal $ chassis
        , chassisDtoOuterSlots = chassisOuterSlots . entityVal $ chassis
        , chassisDtoSensorSlots = chassisSensorSlots . entityVal $ chassis
        , chassisDtoWeaponSlots = chassisWeaponSlots . entityVal $ chassis
        , chassisDtoEngineSlots = chassisEngineSlots . entityVal $ chassis
        , chassisDtoMotiveSlots = chassisMotiveSlots . entityVal $ chassis
        , chassisDtoSailSlots = chassisSailSlots . entityVal $ chassis
        }


-- | Map required component to requirement
toRequirement :: RequiredComponent -> RequiredComponentDto
toRequirement comp =
    RequiredComponentDto
        { requiredComponentDtoPower = ComponentPower
                                        { componentPowerLevel = requiredComponentLevel comp
                                        , componentPowerType = requiredComponentComponentType comp
                                        }
        , requiredComponentDtoAmount = requiredComponentAmount comp
        }


$(deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''ChassisDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 19 } ''PlannedComponentDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 9 } ''DesignDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 20 } ''RequiredComponentDto)

