module Data.Vehicles exposing
    ( Chassis
    , ChassisId(..)
    , ChassisLevel(..)
    , ChassisName(..)
    , ChassisRequirement
    , ChassisTonnage(..)
    , ChassisType(..)
    , Component
    , ComponentAmount(..)
    , ComponentDescription(..)
    , ComponentId(..)
    , ComponentLevel(..)
    , ComponentName(..)
    , ComponentPower
    , ComponentSlot(..)
    , ComponentType(..)
    , Design
    , DesignName(..)
    , PlannedChassis
    , PlannedComponent
    , SlotAmount(..)
    , ValidationMessage(..)
    , Weight(..)
    , chassisTypeToString
    , componentSlotToString
    , installPlan
    , slotToIndex
    , totalCost
    , unChassisId
    , unChassisLevel
    , unChassisName
    , unChassisTonnage
    , unComponentAmount
    , unComponentDescription
    , unComponentId
    , unComponentLevel
    , unComponentName
    , unDesignName
    , unSlotAmount
    , unValidationMessage
    , unWeight
    , validateDesign
    )

import Data.Common
    exposing
        ( BioResource(..)
        , ChemResource(..)
        , DesignId
        , MechResource(..)
        , Resources
        , capitalize
        , findFirst
        , unBio
        , unChem
        , unMech
        , writtenNumber
        )
import List exposing (foldl, map, sum)


type ComponentId
    = ComponentId String


unComponentId : ComponentId -> String
unComponentId (ComponentId x) =
    x


type ComponentLevel
    = ComponentLevel Int


unComponentLevel : ComponentLevel -> Int
unComponentLevel (ComponentLevel x) =
    x


type Weight
    = Weight Int


unWeight : Weight -> Int
unWeight (Weight x) =
    x


type ComponentName
    = ComponentName String


unComponentName : ComponentName -> String
unComponentName (ComponentName x) =
    x


type ComponentDescription
    = ComponentDescription String


unComponentDescription : ComponentDescription -> String
unComponentDescription (ComponentDescription x) =
    x


type ComponentSlot
    = InnerSlot
    | OuterSlot
    | ArmourSlot
    | SensorSlot
    | WeaponSlot
    | EngineSlot
    | MotiveSlot
    | SailSlot


{-| Index of component slot, useful for ordering
-}
slotToIndex : ComponentSlot -> Int
slotToIndex slot =
    case slot of
        InnerSlot ->
            1

        OuterSlot ->
            2

        ArmourSlot ->
            3

        SensorSlot ->
            4

        WeaponSlot ->
            5

        EngineSlot ->
            6

        MotiveSlot ->
            7

        SailSlot ->
            8


{-| Name of component slot
-}
componentSlotToString : ComponentSlot -> String
componentSlotToString slot =
    case slot of
        InnerSlot ->
            "Inner"

        OuterSlot ->
            "Outer"

        ArmourSlot ->
            "Armour"

        SensorSlot ->
            "Sensors"

        WeaponSlot ->
            "Weapons"

        EngineSlot ->
            "Engine"

        MotiveSlot ->
            "Motive"

        SailSlot ->
            "Sails"


type ComponentType
    = BridgeComponent
    | SensorComponent
    | EngineComponent
    | StarSailComponent
    | SupplyComponent
    | MotiveComponent


type ChassisType
    = SpaceShip
    | AirShip
    | LandVehicle
    | TalosArmour
    | IcarusSuit


chassisTypeToString : ChassisType -> String
chassisTypeToString c =
    case c of
        SpaceShip ->
            "Spaceship"

        AirShip ->
            "Airship"

        LandVehicle ->
            "Land vehicle"

        TalosArmour ->
            "Talos"

        IcarusSuit ->
            "Icarus"


type alias ComponentPower =
    { level : ComponentLevel
    , powerType : ComponentType
    }


type alias Component =
    { id : ComponentId
    , level : ComponentLevel
    , name : ComponentName
    , description : ComponentDescription
    , weight : Weight
    , slot : ComponentSlot
    , powers : List ComponentPower
    , cost : Resources
    , chassisType : ChassisType
    }


type alias Chassis =
    { id : ChassisId
    , name : ChassisName
    , chassisType : ChassisType
    , tonnage : ChassisTonnage
    , requirements : List ChassisRequirement
    , armourSlots : SlotAmount
    , innerSlots : SlotAmount
    , outerSlots : SlotAmount
    , sensorSlots : SlotAmount
    , weaponSlots : SlotAmount
    , engineSlots : SlotAmount
    , motiveSlots : SlotAmount
    , sailSlots : SlotAmount
    }


type SlotAmount
    = SlotAmount Int


unSlotAmount : SlotAmount -> Int
unSlotAmount (SlotAmount x) =
    x


type ChassisId
    = ChassisId Int


unChassisId : ChassisId -> Int
unChassisId (ChassisId x) =
    x


type ChassisName
    = ChassisName String


unChassisName : ChassisName -> String
unChassisName (ChassisName x) =
    x


type ChassisTonnage
    = ChassisTonnage Int


unChassisTonnage : ChassisTonnage -> Int
unChassisTonnage (ChassisTonnage x) =
    x


type alias ChassisRequirement =
    { power : ComponentPower
    , amount : ComponentAmount
    }


type ComponentAmount
    = ComponentAmount Int


unComponentAmount : ComponentAmount -> Int
unComponentAmount (ComponentAmount x) =
    x


type alias Design =
    { id : Maybe DesignId
    , chassis : PlannedChassis
    , name : DesignName
    , components : List PlannedComponent
    }


type DesignName
    = DesignName String


unDesignName : DesignName -> String
unDesignName (DesignName x) =
    x


type alias PlannedComponent =
    { id : ComponentId
    , level : ComponentLevel
    , amount : ComponentAmount
    }


type alias PlannedChassis =
    { id : ChassisId
    , level : ChassisLevel
    }


type ChassisLevel
    = ChassisLevel Int


unChassisLevel : ChassisLevel -> Int
unChassisLevel (ChassisLevel x) =
    x


{-| Message informing that there's validation error in vehicle design
-}
type ValidationMessage
    = ValidationMessage String


unValidationMessage : ValidationMessage -> String
unValidationMessage (ValidationMessage x) =
    x


{-| Validate design
-}
validateDesign : List Component -> List Chassis -> Design -> List ValidationMessage
validateDesign components availableChassis design =
    let
        candidate =
            findFirst (\x -> x.id == design.chassis.id) availableChassis
    in
    case candidate of
        Nothing ->
            [ ValidationMessage "Unknown chassis" ]

        Just chassis ->
            validateName design
                ++ validateWeight chassis components design
                ++ validateChassisRequirements chassis components design
                ++ validateSlotUsage chassis components design


{-| Validate that design has name
-}
validateName : Design -> List ValidationMessage
validateName design =
    if unDesignName design.name == "" then
        [ ValidationMessage "Design lacks a name" ]

    else
        []


{-| Validate that weight of selected components doesn't exceed maximum
-}
validateWeight : Chassis -> List Component -> Design -> List ValidationMessage
validateWeight chassis availableComponents design =
    let
        weights =
            map (\x -> unWeight <| plannedWeight availableComponents x) design.components

        totalWeight =
            List.sum weights
    in
    if totalWeight > unChassisTonnage chassis.tonnage then
        [ ValidationMessage "Design exceeds chassis tonnage" ]

    else
        []


{-| Weight of planned component setup
-}
plannedWeight : List Component -> PlannedComponent -> Weight
plannedWeight components plan =
    let
        match =
            findFirst (\x -> x.id == plan.id) components
    in
    case match of
        Nothing ->
            Weight 0

        Just component ->
            Weight <| unWeight component.weight * unComponentAmount plan.amount


{-| Validate design against chassis requirements of selected chassis
-}
validateChassisRequirements : Chassis -> List Component -> Design -> List ValidationMessage
validateChassisRequirements chassis components design =
    let
        installed =
            installPlan components design.components
    in
    List.filterMap (validateRequirement installed) chassis.requirements


{-| Verify if given list of components and installed amount satisfy a chassis requirement
-}
validateRequirement : List ( Component, ComponentAmount ) -> ChassisRequirement -> Maybe ValidationMessage
validateRequirement installed requirement =
    let
        matches =
            List.filter
                (\( comp, _ ) ->
                    List.any
                        (\x ->
                            (x.powerType == requirement.power.powerType)
                                && (unComponentLevel x.level >= unComponentLevel requirement.power.level)
                        )
                        comp.powers
                )
                installed

        amount =
            List.foldl (\( _, a ) b -> unComponentAmount a + b) 0 matches
    in
    if amount >= unComponentAmount requirement.amount then
        Nothing

    else
        Just <| missingPowerMessage requirement


{-| Validation message stating a unfulfilled requirement
-}
missingPowerMessage : ChassisRequirement -> ValidationMessage
missingPowerMessage requirement =
    let
        amount =
            unComponentAmount requirement.amount

        missing =
            if amount > 1 then
                (capitalize <| writtenNumber amount)
                    ++ " level "
                    ++ String.fromInt (unComponentLevel requirement.power.level)
                    ++ " "
                    ++ (case requirement.power.powerType of
                            BridgeComponent ->
                                "bridges"

                            SensorComponent ->
                                "sensor systems"

                            EngineComponent ->
                                "engines"

                            StarSailComponent ->
                                "star sails"

                            SupplyComponent ->
                                "supply storages"

                            MotiveComponent ->
                                "motive systems"
                       )

            else
                "Level "
                    ++ String.fromInt (unComponentLevel requirement.power.level)
                    ++ " "
                    ++ (case requirement.power.powerType of
                            BridgeComponent ->
                                "Bridge"

                            SensorComponent ->
                                "Sensor system"

                            EngineComponent ->
                                "Engine"

                            StarSailComponent ->
                                "Star sail"

                            SupplyComponent ->
                                "Supply storage"

                            MotiveComponent ->
                                "Motive system"
                       )
    in
    if amount > 1 then
        ValidationMessage <| missing ++ " are required"

    else
        ValidationMessage <| missing ++ " is required"


{-| Validate that slot usage doesn't exceed chassis slots
-}
validateSlotUsage : Chassis -> List Component -> Design -> List ValidationMessage
validateSlotUsage chassis availableComponents design =
    let
        installed =
            installPlan availableComponents design.components

        armourComponents =
            componentCount installed ArmourSlot

        innerComponents =
            componentCount installed InnerSlot

        outerComponents =
            componentCount installed OuterSlot

        sensorComponents =
            componentCount installed SensorSlot

        weaponComponents =
            componentCount installed WeaponSlot

        engineComponents =
            componentCount installed EngineSlot

        motiveComponents =
            componentCount installed MotiveSlot

        sailComponents =
            componentCount installed SailSlot
    in
    slotsExceededMessage armourComponents chassis.armourSlots ArmourSlot
        ++ slotsExceededMessage innerComponents chassis.innerSlots InnerSlot
        ++ slotsExceededMessage outerComponents chassis.outerSlots OuterSlot
        ++ slotsExceededMessage sensorComponents chassis.sensorSlots SensorSlot
        ++ slotsExceededMessage weaponComponents chassis.weaponSlots WeaponSlot
        ++ slotsExceededMessage engineComponents chassis.engineSlots EngineSlot
        ++ slotsExceededMessage motiveComponents chassis.motiveSlots MotiveSlot
        ++ slotsExceededMessage sailComponents chassis.sailSlots SailSlot


{-| How many components are of specific slot
-}
componentCount : List ( Component, ComponentAmount ) -> ComponentSlot -> ComponentAmount
componentCount components slot =
    List.filter (\( comp, _ ) -> comp.slot == slot) components
        |> List.map (\( _, amount ) -> unComponentAmount amount)
        |> List.sum
        |> ComponentAmount


slotsExceededMessage : ComponentAmount -> SlotAmount -> ComponentSlot -> List ValidationMessage
slotsExceededMessage installed available slot =
    if unComponentAmount installed > unSlotAmount available then
        let
            slotName =
                case slot of
                    InnerSlot ->
                        "inner slots"

                    OuterSlot ->
                        "outer slots"

                    ArmourSlot ->
                        "armour slots"

                    SensorSlot ->
                        "sensor slots"

                    WeaponSlot ->
                        "weapon slots"

                    EngineSlot ->
                        "engine slots"

                    MotiveSlot ->
                        "motive slots"

                    SailSlot ->
                        "sail slots"
        in
        [ ValidationMessage <| "Too many " ++ slotName ++ " used" ]

    else
        []


{-| Turn list of planned components into tuples of component and amount
-}
installPlan : List Component -> List PlannedComponent -> List ( Component, ComponentAmount )
installPlan availableComponents plannedComponents =
    List.map (\plan -> ( findFirst (\comp -> plan.id == comp.id) availableComponents, plan.amount )) plannedComponents
        |> List.filterMap
            (\( comp, amount ) ->
                case comp of
                    Just c ->
                        Just ( c, amount )

                    Nothing ->
                        Nothing
            )


{-| Total cost of planned components, doesn't take chassis into account
-}
totalCost : List Component -> Design -> Resources
totalCost availableComponents design =
    let
        installed =
            installPlan availableComponents design.components

        costs =
            List.map (\( comp, amount ) -> scaleCost comp.cost (unComponentAmount amount)) installed
    in
    List.foldl
        (\a b ->
            { biological = BioResource <| unBio a.biological + unBio b.biological
            , mechanical = MechResource <| unMech a.mechanical + unMech b.mechanical
            , chemical = ChemResource <| unChem a.chemical + unChem b.chemical
            }
        )
        { biological = BioResource 0
        , mechanical = MechResource 0
        , chemical = ChemResource 0
        }
        costs


{-| Multiply cost with scalar
-}
scaleCost : Resources -> Int -> Resources
scaleCost res n =
    { biological = BioResource <| unBio res.biological * n
    , mechanical = MechResource <| unMech res.mechanical * n
    , chemical = ChemResource <| unChem res.chemical * n
    }
