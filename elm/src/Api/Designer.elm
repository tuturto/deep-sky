module Api.Designer exposing
    ( availableChassisCmd
    , availableComponentsCmd
    , availableDesignsCmd
    , deleteDesignCmd
    , designIdDecoder
    , designNameDecoder
    , estimateDesign
    , saveDesignCmd
    )

import Api.Common
    exposing
        ( delete
        , encodeMaybe
        , get
        , post
        , put
        , resourcesDecoder
        )
import Api.Endpoints exposing (Endpoint(..))
import Data.Common exposing (DesignId(..))
import Data.Model
    exposing
        ( ApiMsg(..)
        , Model
        , Msg(..)
        )
import Data.Vehicles
    exposing
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
        , CrewAmount(..)
        , CrewPosition(..)
        , CrewRank(..)
        , CrewRequirement
        , CrewSpace(..)
        , CrewSpaceReq(..)
        , Design
        , DesignName(..)
        , PlannedChassis
        , PlannedComponent
        , SlotAmount(..)
        , TotalCrewSpace
        , UnitStats
        , Weight(..)
        )
import Http
import Json.Decode as Decode
    exposing
        ( andThen
        , fail
        , field
        , index
        , int
        , list
        , maybe
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Extra exposing (andMap)
import Json.Encode as Encode


availableComponentsCmd : Cmd Msg
availableComponentsCmd =
    Http.send (ApiMsgCompleted << ComponentsReceived) (get ApiAvailableComponents (list componentDecoder))


availableChassisCmd : Cmd Msg
availableChassisCmd =
    Http.send (ApiMsgCompleted << ChassisReceived) (get ApiAvailableChassis (list chassisDecoder))


availableDesignsCmd : Cmd Msg
availableDesignsCmd =
    Http.send (ApiMsgCompleted << DesignsReceived) (get ApiAllDesigns (list designDecoder))


saveDesignCmd : Design -> Cmd Msg
saveDesignCmd design =
    case design.id of
        Just dId ->
            Http.send (ApiMsgCompleted << DesignSaved) (put (ApiSingleDesign dId) (designEncoder design) designDecoder)

        Nothing ->
            Http.send (ApiMsgCompleted << DesignSaved) (post ApiAllDesigns (designEncoder design) designDecoder)


deleteDesignCmd : Design -> Cmd Msg
deleteDesignCmd design =
    case design.id of
        Just dId ->
            Http.send (ApiMsgCompleted << DesignsReceived) (delete (ApiSingleDesign dId) Nothing (list designDecoder))

        Nothing ->
            Cmd.none


estimateDesign : Design -> Cmd Msg
estimateDesign design =
    Http.send (ApiMsgCompleted << DesignEstimated) (post ApiDesignEstimate (designEncoder design) unitStatsDecoder)


componentDecoder : Decode.Decoder Component
componentDecoder =
    succeed Component
        |> andMap (field "Id" componentIdDecoder)
        |> andMap (field "Level" componentLevelDecoder)
        |> andMap (field "Name" componentNameDecoder)
        |> andMap (field "Description" componentDescriptionDecoder)
        |> andMap (field "Weight" weightDecoder)
        |> andMap (field "Slot" componentSlotDecoder)
        |> andMap (field "Type" (list componentPowerDecoder))
        |> andMap (field "Cost" resourcesDecoder)
        |> andMap (field "ChassisType" chassisTypeDecoder)


componentIdDecoder : Decode.Decoder ComponentId
componentIdDecoder =
    succeed ComponentId
        |> andMap string


componentIdEncoder : ComponentId -> Encode.Value
componentIdEncoder (ComponentId x) =
    Encode.string x


componentLevelDecoder : Decode.Decoder ComponentLevel
componentLevelDecoder =
    succeed ComponentLevel
        |> andMap int


componentLevelEncoder : ComponentLevel -> Encode.Value
componentLevelEncoder (ComponentLevel x) =
    Encode.int x


componentNameDecoder : Decode.Decoder ComponentName
componentNameDecoder =
    succeed ComponentName
        |> andMap string


componentDescriptionDecoder : Decode.Decoder ComponentDescription
componentDescriptionDecoder =
    succeed ComponentDescription
        |> andMap string


weightDecoder : Decode.Decoder Weight
weightDecoder =
    succeed Weight
        |> andMap int


componentSlotDecoder : Decode.Decoder ComponentSlot
componentSlotDecoder =
    string |> andThen stringToComponentSlot


stringToComponentSlot : String -> Decode.Decoder ComponentSlot
stringToComponentSlot s =
    case s of
        "InnerSlot" ->
            succeed InnerSlot

        "OuterSlot" ->
            succeed OuterSlot

        "ArmourSlot" ->
            succeed ArmourSlot

        "SensorSlot" ->
            succeed SensorSlot

        "WeaponSlot" ->
            succeed WeaponSlot

        "EngineSlot" ->
            succeed EngineSlot

        "MotiveSlot" ->
            succeed MotiveSlot

        "SailSlot" ->
            succeed SailSlot

        _ ->
            fail ("unknown component type: " ++ s)


componentPowerDecoder : Decode.Decoder ComponentPower
componentPowerDecoder =
    succeed ComponentPower
        |> andMap (field "Level" componentLevelDecoder)
        |> andMap (field "Type" componentTypeDecoder)


componentTypeDecoder : Decode.Decoder ComponentType
componentTypeDecoder =
    string |> andThen stringToComponentType


stringToComponentType : String -> Decode.Decoder ComponentType
stringToComponentType s =
    case s of
        "BridgeComponent" ->
            succeed BridgeComponent

        "SensorComponent" ->
            succeed SensorComponent

        "EngineComponent" ->
            succeed EngineComponent

        "StarSailComponent" ->
            succeed StarSailComponent

        "QuartersComponent" ->
            succeed QuartersComponent

        "InfantryBayComponent" ->
            succeed InfantryBayComponent

        "SupplyComponent" ->
            succeed SupplyComponent

        "MotiveComponent" ->
            succeed MotiveComponent

        _ ->
            fail ("unknown component type: " ++ s)


chassisTypeDecoder : Decode.Decoder ChassisType
chassisTypeDecoder =
    string |> andThen stringToChassisType


stringToChassisType : String -> Decode.Decoder ChassisType
stringToChassisType s =
    case s of
        "SpaceShip" ->
            succeed SpaceShip

        "AirShip" ->
            succeed AirShip

        "LandVehicle" ->
            succeed LandVehicle

        "TalosArmour" ->
            succeed TalosArmour

        "IcarusSuit" ->
            succeed IcarusSuit

        _ ->
            fail ("unknown chassis type: " ++ s)


chassisDecoder : Decode.Decoder Chassis
chassisDecoder =
    succeed Chassis
        |> andMap (field "Id" chassisIdDecoder)
        |> andMap (field "Name" chassisNameDecoder)
        |> andMap (field "Type" chassisTypeDecoder)
        |> andMap (field "MaxTonnage" chassisTonnageDecoder)
        |> andMap (field "RequiredTypes" (list chassisRequirementDecoder))
        |> andMap (field "ArmourSlots" slotAmountDecoder)
        |> andMap (field "InnerSlots" slotAmountDecoder)
        |> andMap (field "OuterSlots" slotAmountDecoder)
        |> andMap (field "SensorSlots" slotAmountDecoder)
        |> andMap (field "WeaponSlots" slotAmountDecoder)
        |> andMap (field "EngineSlots" slotAmountDecoder)
        |> andMap (field "MotiveSlots" slotAmountDecoder)
        |> andMap (field "SailSlots" slotAmountDecoder)


chassisIdDecoder : Decode.Decoder ChassisId
chassisIdDecoder =
    succeed ChassisId
        |> andMap int


chassisIdEncoder : ChassisId -> Encode.Value
chassisIdEncoder (ChassisId x) =
    Encode.int x


chassisLevelDecoder : Decode.Decoder ChassisLevel
chassisLevelDecoder =
    succeed ChassisLevel
        |> andMap int


chassisLevelEncoder : ChassisLevel -> Encode.Value
chassisLevelEncoder (ChassisLevel x) =
    Encode.int x


chassisNameDecoder : Decode.Decoder ChassisName
chassisNameDecoder =
    succeed ChassisName
        |> andMap string


chassisTonnageDecoder : Decode.Decoder ChassisTonnage
chassisTonnageDecoder =
    succeed ChassisTonnage
        |> andMap int


chassisRequirementDecoder : Decode.Decoder ChassisRequirement
chassisRequirementDecoder =
    succeed ChassisRequirement
        |> andMap (field "Power" componentPowerDecoder)
        |> andMap (field "Amount" componentAmountDecoder)


componentAmountDecoder : Decode.Decoder ComponentAmount
componentAmountDecoder =
    succeed ComponentAmount
        |> andMap int


componentAmountEncoder : ComponentAmount -> Encode.Value
componentAmountEncoder (ComponentAmount x) =
    Encode.int x


designDecoder : Decode.Decoder Design
designDecoder =
    let
        plannedChassis =
            succeed PlannedChassis
                |> andMap (field "ChassisId" chassisIdDecoder)
                |> andMap (field "ChassisLevel" chassisLevelDecoder)
    in
    succeed Design
        |> andMap (field "Id" (maybe designIdDecoder))
        |> andMap plannedChassis
        |> andMap (field "Name" designNameDecoder)
        |> andMap (field "Components" (list plannedComponentDecoder))


designEncoder : Design -> Encode.Value
designEncoder design =
    Encode.object
        [ ( "Id", encodeMaybe designIdEncoder design.id )
        , ( "ChassisId", chassisIdEncoder design.chassis.id )
        , ( "ChassisLevel", chassisLevelEncoder design.chassis.level )
        , ( "Name", designNameEncoder design.name )
        , ( "Components", Encode.list plannedComponentEncoder design.components )
        ]


designIdDecoder : Decode.Decoder DesignId
designIdDecoder =
    succeed DesignId
        |> andMap int


designIdEncoder : DesignId -> Encode.Value
designIdEncoder (DesignId x) =
    Encode.int x


designNameDecoder : Decode.Decoder DesignName
designNameDecoder =
    succeed DesignName
        |> andMap string


designNameEncoder : DesignName -> Encode.Value
designNameEncoder (DesignName x) =
    Encode.string x


plannedComponentDecoder : Decode.Decoder PlannedComponent
plannedComponentDecoder =
    succeed PlannedComponent
        |> andMap (field "Id" componentIdDecoder)
        |> andMap (field "Level" componentLevelDecoder)
        |> andMap (field "Amount" componentAmountDecoder)


plannedComponentEncoder : PlannedComponent -> Encode.Value
plannedComponentEncoder component =
    Encode.object
        [ ( "Id", componentIdEncoder component.id )
        , ( "Level", componentLevelEncoder component.level )
        , ( "Amount", componentAmountEncoder component.amount )
        ]


slotAmountDecoder : Decode.Decoder SlotAmount
slotAmountDecoder =
    succeed SlotAmount
        |> andMap int


unitStatsDecoder : Decode.Decoder UnitStats
unitStatsDecoder =
    succeed UnitStats
        |> andMap (field "MinimumCrew" (list crewRequirementDecoder))
        |> andMap (field "NominalCrew" (list crewRequirementDecoder))
        |> andMap (field "CrewSpace" totalCrewSpaceDecoder)
        |> andMap (field "CrewSpaceRequired" crewSpaceReqDecoder)


crewRequirementDecoder : Decode.Decoder CrewRequirement
crewRequirementDecoder =
    succeed CrewRequirement
        |> andMap (index 0 crewPositionDecoder)
        |> andMap (index 1 crewRankDecoder)
        |> andMap (index 2 crewAmountDecoder)


crewPositionDecoder : Decode.Decoder CrewPosition
crewPositionDecoder =
    string |> andThen stringToCrewPosition


stringToCrewPosition : String -> Decode.Decoder CrewPosition
stringToCrewPosition s =
    case s of
        "Commander" ->
            succeed Commander

        "Navigator" ->
            succeed Navigator

        "Signaler" ->
            succeed Signaler

        "SensorOperator" ->
            succeed SensorOperator

        "Gunner" ->
            succeed Gunner

        "Doctor" ->
            succeed Doctor

        "Nurse" ->
            succeed Nurse

        "Driver" ->
            succeed Driver

        "Helmsman" ->
            succeed Helmsman

        "Artificer" ->
            succeed Artificer

        "Crew" ->
            succeed Crew

        "Passenger" ->
            succeed Passenger

        _ ->
            fail "unknown value"


crewRankDecoder : Decode.Decoder CrewRank
crewRankDecoder =
    string |> andThen stringToCrewRank


stringToCrewRank : String -> Decode.Decoder CrewRank
stringToCrewRank s =
    case s of
        "SecondClass" ->
            succeed SecondClass

        "FirstClass" ->
            succeed FirstClass

        "Senior" ->
            succeed Senior

        "Chief" ->
            succeed Chief

        _ ->
            fail "unknown value"


crewAmountDecoder : Decode.Decoder CrewAmount
crewAmountDecoder =
    succeed CrewAmount
        |> andMap int


totalCrewSpaceDecoder : Decode.Decoder TotalCrewSpace
totalCrewSpaceDecoder =
    succeed TotalCrewSpace
        |> andMap (field "Steerage" crewSpaceDecoder)
        |> andMap (field "Standard" crewSpaceDecoder)
        |> andMap (field "Luxury" crewSpaceDecoder)


crewSpaceDecoder : Decode.Decoder CrewSpace
crewSpaceDecoder =
    succeed CrewSpace
        |> andMap int


crewSpaceReqDecoder : Decode.Decoder CrewSpaceReq
crewSpaceReqDecoder =
    string |> andThen stringToCrewSpaceReq


stringToCrewSpaceReq : String -> Decode.Decoder CrewSpaceReq
stringToCrewSpaceReq s =
    case s of
        "CrewSpaceRequired" ->
            succeed CrewSpaceRequired

        "CrewSpaceOptional" ->
            succeed CrewSpaceOptional

        _ ->
            fail "Unknown value"
