module Data.Vehicles exposing
    ( Band(..)
    , Chassis
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
    , CrewAmount(..)
    , CrewPosition(..)
    , CrewRank(..)
    , unitStats
    , CrewReport
    , CrewRequirement
    , CrewSpace(..)
    , CrewSpaceReq(..)
    , Design
    , DesignName(..)
    , PlannedChassis
    , PlannedComponent
    , ShipDetails
    , ShipLocation(..)
    , ShipName(..)
    , SlotAmount(..)
    , TotalCrewSpace
    , Unit(..)
    , UnitLocation(..)
    , UnitName(..)
    , UnitStats
    , ValidationMessage(..)
    , VehicleDetails
    , VehicleLocation(..)
    , VehicleName(..)
    , Weight(..)
    , chassisTypeToString
    , combinedCrewSpace
    , componentSlotToString
    , crewCount
    , crewPositionToString
    , crewRankToString
    , installPlan
    , positionOrdering
    , rankOrdering
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
    , unCrewAmount
    , unCrewSpace
    , unDesignName
    , unShipName
    , unSlotAmount
    , unUnitName
    , unValidationMessage
    , unVehicleName
    , unWeight
    , validateDesign
    )

import Data.Common
    exposing
        ( BioResource(..)
        , ChemResource(..)
        , DesignId
        , MechResource(..)
        , PersonId
        , PlanetId
        , Resources
        , StarSystemId
        , capitalize
        , findFirst
        , unBio
        , unChem
        , unMech
        , writtenNumber
        )
import Data.PersonNames exposing (PersonName)
import List exposing (foldl, map, sum)
import Ordering exposing (Ordering)


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


type DesignName
    = DesignName String


unDesignName : DesignName -> String
unDesignName (DesignName s) =
    s


type UnitName
    = UnitName String


unUnitName : UnitName -> String
unUnitName (UnitName s) =
    s


type VehicleName
    = VehicleName String


unVehicleName : VehicleName -> String
unVehicleName (VehicleName s) =
    s


type ShipName
    = ShipName String


unShipName : ShipName -> String
unShipName (ShipName s) =
    s


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
    | QuartersComponent
    | InfantryBayComponent
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
validateDesign : List Component -> List Chassis -> Maybe UnitStats -> Design -> List ValidationMessage
validateDesign components availableChassis stats design =
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
                ++ validateQuarters stats


{-| Validate that there's enough space for crew
-}
validateQuarters : Maybe UnitStats -> List ValidationMessage
validateQuarters s =
    case s of
        Nothing ->
            [ ValidationMessage "Design performance has not been estimated" ]

        Just stats ->
            case stats.crewSpaceRequired of
                Just CrewSpaceOptional ->
                    []

                Just CrewSpaceRequired ->
                    let
                        provided =
                            Maybe.map (unCrewSpace << combinedCrewSpace) stats.crewSpace
                                |> Maybe.withDefault 0

                        required =
                            Maybe.map (unCrewAmount << crewCount) stats.nominalCrew
                                |> Maybe.withDefault 0
                    in
                    if provided < required then
                        [ ValidationMessage <| String.fromInt (required - provided) ++ " more quarters are needed" ]

                    else
                        []

                Nothing ->
                    []


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

                            QuartersComponent ->
                                "quarters"

                            InfantryBayComponent ->
                                "infantry bays"

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

                            QuartersComponent ->
                                "Quarters"

                            InfantryBayComponent ->
                                "Infantry bay"

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
                Maybe.map (\c -> ( c, amount )) comp
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


{-| Stats of a unit
-}
type alias UnitStats =
    { minimumCrew : Maybe (List CrewRequirement)
    , nominalCrew : Maybe (List CrewRequirement)
    , crewSpace : Maybe TotalCrewSpace
    , crewSpaceRequired : Maybe CrewSpaceReq
    }


{-| Crew requirements of a unit
-}
type alias CrewRequirement =
    { position : CrewPosition
    , rank : CrewRank
    , amount : CrewAmount
    }


{-| Positions crew can assume
-}
type CrewPosition
    = Commander
    | Navigator
    | Signaler
    | SensorOperator
    | Gunner
    | Doctor
    | Nurse
    | Driver
    | Helmsman
    | Artificer
    | Crew
    | Passenger


{-| Seniority ranks for crew
-}
type CrewRank
    = SecondClass
    | FirstClass
    | Senior
    | Chief


{-| Amount of crew
-}
type CrewAmount
    = CrewAmount Int


{-| Map from crew amount to plain int
-}
unCrewAmount : CrewAmount -> Int
unCrewAmount (CrewAmount n) =
    n


{-| Total space reserved for crew, split into various levels of comfort
-}
type alias TotalCrewSpace =
    { steerageSpace : CrewSpace
    , standardSpace : CrewSpace
    , luxurySpace : CrewSpace
    }


{-| Combined crew space, regardless of type
-}
combinedCrewSpace : TotalCrewSpace -> CrewSpace
combinedCrewSpace total =
    CrewSpace <|
        unCrewSpace total.steerageSpace
            + unCrewSpace total.standardSpace
            + unCrewSpace total.luxurySpace


{-| Amount of crew that fits in certain place
-}
type CrewSpace
    = CrewSpace Int


{-| Map from crew space to plain int
-}
unCrewSpace : CrewSpace -> Int
unCrewSpace (CrewSpace n) =
    n


{-| Is crew space required or optional
-}
type CrewSpaceReq
    = CrewSpaceRequired
    | CrewSpaceOptional


{-| Total amount of crew in a list
-}
crewCount : List CrewRequirement -> CrewAmount
crewCount reqs =
    List.map (\req -> unCrewAmount req.amount) reqs
        |> List.sum
        |> CrewAmount


type Unit
    = Ship ShipDetails
    | Vehicle VehicleDetails


type alias ShipDetails =
    { name : ShipName
    , designName : DesignName
    , stats : UnitStats
    , location : Maybe ShipLocation
    , crew : List CrewReport
    }


type alias VehicleDetails =
    { name : VehicleName
    , designName : DesignName
    , stats : UnitStats
    , location : Maybe VehicleLocation
    , crew : List CrewReport
    }

unitStats : Unit -> UnitStats
unitStats unit =
    case unit of
        Ship ship ->
            ship.stats

        Vehicle vehicle ->
            vehicle.stats

type UnitLocation
    = ShipLocation ShipLocation
    | VehicleLocation VehicleLocation


type ShipLocation
    = PlanetarySpace PlanetId Band
    | SystemSpace StarSystemId Band


type VehicleLocation
    = VehicleOnPlanet PlanetId


type Band
    = BandSurface
    | BandOrbit
    | BandA
    | BandB
    | BandC
    | BandD
    | BandE
    | BandF
    | BandG


type alias CrewReport =
    { personId : PersonId
    , name : PersonName
    , position : CrewPosition
    , rank : CrewRank
    }


{-| Map crew position into descriptive text
-}
crewPositionToString : CrewPosition -> String
crewPositionToString pos =
    case pos of
        Commander ->
            "Commander"

        Navigator ->
            "Navigator"

        Signaler ->
            "Signaler"

        SensorOperator ->
            "Sensor operator"

        Gunner ->
            "Gunner"

        Doctor ->
            "Doctor"

        Nurse ->
            "Nurse"

        Driver ->
            "Driver"

        Helmsman ->
            "Helmsman"

        Artificer ->
            "Artificer"

        Crew ->
            "Crew"

        Passenger ->
            "Passenger"


crewRankToString : CrewRank -> String
crewRankToString rank =
    case rank of
        SecondClass ->
            "Second class"

        FirstClass ->
            "First class"

        Senior ->
            "Senior"

        Chief ->
            "Chief"


rankToIndex : CrewRank -> Int
rankToIndex rank =
    case rank of
        SecondClass ->
            4

        FirstClass ->
            3

        Senior ->
            2

        Chief ->
            1


rankOrdering : Ordering CrewRank
rankOrdering a b =
    Ordering.natural (rankToIndex a) (rankToIndex b)


positionToIndex : CrewPosition -> Int
positionToIndex pos =
    case pos of
        Commander ->
            1

        Navigator ->
            2

        Signaler ->
            3

        SensorOperator ->
            4

        Gunner ->
            5

        Doctor ->
            6

        Nurse ->
            7

        Driver ->
            8

        Helmsman ->
            9

        Artificer ->
            10

        Crew ->
            11

        Passenger ->
            12


positionOrdering : Ordering CrewPosition
positionOrdering a b =
    Ordering.natural (positionToIndex a) (positionToIndex b)
