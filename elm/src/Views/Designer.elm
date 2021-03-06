module Views.Designer exposing
    ( desginSaveFailure
    , designSaveOk
    , init
    , isLoading
    , page
    , update
    )

import Accessors exposing (over, set)
import Accessors.Library exposing (onEach, try)
import Api.Designer
    exposing
        ( deleteDesign
        , estimateDesign
        , getAvailableChassis
        , getAvailableComponents
        , getAvailableDesigns
        , saveDesign
        )
import Data.Accessors
    exposing
        ( amountA
        , availableChassisA
        , availableComponentsA
        , chassisCurrentPageA
        , chassisListStatusA
        , commandsStatusA
        , componentListStatusA
        , componentsA
        , componentsCurrentPageA
        , currentDesignA
        , designPanelStatusA
        , designStatsA
        , designerRA
        , designsA
        , designsCurrentPageA
        , designsPanelStatusA
        , errorsA
        , idA
        , messagesStatusA
        , nameA
        , statsStatusA
        )
import Data.Common
    exposing
        ( InfoPanelStatus(..)
        , error
        , findFirst
        , joinMaybe
        , maxPage
        , unDesignId
        )
import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..))
import Data.Vehicles
    exposing
        ( Chassis
        , ChassisLevel(..)
        , Component
        , ComponentAmount(..)
        , ComponentId
        , ComponentSlot(..)
        , CrewSpaceReq(..)
        , Design
        , DesignName(..)
        , PlannedComponent
        , SlotAmount(..)
        , UnitStats
        , ValidationMessage(..)
        , Weight(..)
        , chassisTypeToString
        , componentSlotToString
        , crewCount
        , installPlan
        , slotToIndex
        , totalCost
        , unChassisId
        , unChassisName
        , unChassisTonnage
        , unComponentAmount
        , unComponentName
        , unCrewAmount
        , unCrewSpace
        , unDesignName
        , unSlotAmount
        , unWeight
        , validateDesign
        )
import Html
    exposing
        ( Html
        , div
        , i
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes
    exposing
        ( class
        , id
        , placeholder
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Http
import Maybe exposing (andThen, withDefault)
import RemoteData exposing (RemoteData(..), WebData)
import SaveData exposing (SaveData(..))
import ViewModels.Designer exposing (DesignerRMsg(..), DesignerViewModel)
import Views.Helpers
    exposing
        ( biologicalsToText
        , chemicalsToText
        , infoPanel
        , mechanicalsToText
        , triplePanels
        )


{-| Render designer view
-}
page : Model -> Html Msg
page model =
    div [] <|
        triplePanels ( leftPanel, "col-lg-4" ) ( middlePanel, "col-lg-5" ) ( rightPanel, "col-lg-3" ) model


{-| Display left panel
-}
leftPanel : Model -> List (Html Msg)
leftPanel model =
    chassisList model ++ componentList model


{-| Display list of all chassis that are available for the current user
-}
chassisList : Model -> List (Html Msg)
chassisList model =
    infoPanel
        { title = "Chassis"
        , currentStatus = model.designerR.chassisListStatus
        , openingMessage = DesignerMessage <| ChassisListStatusChanged InfoPanelOpen
        , closingMessage = DesignerMessage <| ChassisListStatusChanged InfoPanelClosed
        , refreshMessage = Just (DesignerMessage ChassisListUpdatedRequested)
        }
        (Just
            { pageSize = model.designerR.chassisPageSize
            , currentPage = model.designerR.chassisCurrentPage
            , maxPage =
                model.designerR.availableChassis
                    |> RemoteData.withDefault []
                    |> maxPage model.designerR.chassisPageSize
            , pageChangedMessage = DesignerMessage << ChassisListPageChanged
            }
        )
        chassisListContent
        model


{-| Content of chassis list to display
-}
chassisListContent : Model -> List (Html Msg)
chassisListContent model =
    [ div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ table [ id "chassis-list" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Type" ]
                        , th [] [ text "Size" ]
                        , th [] []
                        ]
                    ]
                , tbody []
                    (model.designerR.availableChassis
                        |> RemoteData.withDefault []
                        |> List.sortWith sortChassis
                        |> List.drop (model.designerR.chassisCurrentPage * model.designerR.chassisPageSize)
                        |> List.take model.designerR.chassisPageSize
                        |> List.map (chassisEntry model.designerR)
                    )
                ]
            ]
        ]
    , div [ class "row space-top" ]
        [ div [ class "col-lg-12" ]
            [ case model.designerR.currentDesign of
                Nothing ->
                    text "Select chassis from list above to start designing a new vehicle."

                Just _ ->
                    text "Selecting a chassis of different type will remove all added components"
            ]
        ]
    ]


{-| Compare two chassis by their type and name
-}
sortChassis : Chassis -> Chassis -> Order
sortChassis a b =
    case sortChassisByType a b of
        EQ ->
            compare (unChassisName a.name) (unChassisName b.name)

        ordering ->
            ordering


{-| Order two chassis by their type
-}
sortChassisByType : Chassis -> Chassis -> Order
sortChassisByType a b =
    compare (chassisTypeToString a.chassisType) (chassisTypeToString b.chassisType)


{-| Order two chassis by their name
-}
sortChassisByName : Chassis -> Chassis -> Order
sortChassisByName a b =
    compare (unChassisName a.name) (unChassisName b.name)


{-| Render single chassis element on the chassis list
-}
chassisEntry : DesignerViewModel -> Chassis -> Html Msg
chassisEntry vm chassis =
    let
        rowClass =
            case vm.currentDesign of
                Just design ->
                    if design.chassis.id == chassis.id then
                        "selected-chassis"

                    else
                        ""

                Nothing ->
                    ""
    in
    tr [ class rowClass, onClick (DesignerMessage <| ChassisSelected chassis) ]
        [ td [] [ text <| unChassisName chassis.name ]
        , td [] [ text <| chassisTypeToString chassis.chassisType ]
        , td [] [ text <| String.fromInt <| unChassisTonnage chassis.tonnage ]
        ]


{-| Display list of all components that are available for the current user
-}
componentList : Model -> List (Html Msg)
componentList model =
    infoPanel
        { title = "Components"
        , currentStatus = model.designerR.componentListStatus
        , openingMessage = DesignerMessage <| ComponentListStatusChanged InfoPanelOpen
        , closingMessage = DesignerMessage <| ComponentListStatusChanged InfoPanelClosed
        , refreshMessage = Just (DesignerMessage ComponentListUpdatedRequested)
        }
        (Just
            { pageSize = model.designerR.componentsPageSize
            , currentPage = model.designerR.componentsCurrentPage
            , maxPage =
                applicableComponents model.designerR
                    |> maxPage model.designerR.componentsPageSize
            , pageChangedMessage = DesignerMessage << ComponentListPageChanged
            }
        )
        componentListContent
        model


{-| Content of component list to display
-}
componentListContent : Model -> List (Html Msg)
componentListContent model =
    (applicableComponents model.designerR
        |> List.drop (model.designerR.componentsPageSize * model.designerR.componentsCurrentPage)
        |> List.take model.designerR.componentsPageSize
        |> List.map componentEntry
    )
        ++ componentListHelp model.designerR


{-| List of components that are currently selectable
-}
applicableComponents : DesignerViewModel -> List Component
applicableComponents vm =
    vm.currentDesign
        |> Maybe.andThen (currentChassis vm.availableChassis)
        |> Maybe.andThen (matchingComponents vm.availableComponents)
        |> withDefault []
        |> List.sortWith componentSort


{-| Sort components by their slot and name
-}
componentSort : Component -> Component -> Order
componentSort a b =
    case sortByComponentSlot a b of
        EQ ->
            sortByComponentName a b

        ordering ->
            ordering


{-| Sort by slot type of components
-}
sortByComponentSlot : Component -> Component -> Order
sortByComponentSlot a b =
    compare (slotToIndex a.slot) (slotToIndex b.slot)


{-| Sort by component name
-}
sortByComponentName : Component -> Component -> Order
sortByComponentName a b =
    compare (unComponentName a.name) (unComponentName b.name)


{-| Given list of available chassis and current design, find matching chassis
-}
currentChassis : WebData (List Chassis) -> Design -> Maybe Chassis
currentChassis availableChassis design =
    RemoteData.map (\entries -> List.filter (\x -> x.id == design.chassis.id) entries) availableChassis
        |> RemoteData.map (\entries -> List.head entries)
        |> RemoteData.toMaybe
        |> joinMaybe


{-| Produce a list of components that are applicable for selected chassis
-}
matchingComponents : WebData (List Component) -> Chassis -> Maybe (List Component)
matchingComponents availableComponents chassis =
    RemoteData.map
        (\components ->
            List.filter (\x -> x.chassisType == chassis.chassisType) components
        )
        availableComponents
        |> RemoteData.toMaybe


{-| Single component entry in available components
-}
componentEntry : Component -> Html Msg
componentEntry component =
    div [ class "row available-component", onClick (DesignerMessage <| ComponentAdded component) ]
        [ div [ class "col-lg-12" ]
            [ div [ class "row" ]
                [ div [ class "col-lg-8" ] [ text <| unComponentName component.name ]
                , div [ class "col-lg-4" ] [ text <| componentSlotToString component.slot ]
                ]
            , div [ class "row" ]
                [ div [ class "col-lg-3" ]
                    [ text <| String.fromInt <| unWeight component.weight
                    , text " t"
                    ]
                , div [ class "col-lg-9" ]
                    ((biologicalsToText <| Just component.cost)
                        ++ (text " " :: (mechanicalsToText <| Just component.cost))
                        ++ (text " " :: (chemicalsToText <| Just component.cost))
                    )
                ]
            ]
        ]


{-| Help text for component list
-}
componentListHelp : DesignerViewModel -> List (Html Msg)
componentListHelp vm =
    [ div [ class "row space-top" ]
        [ div [ class "col-lg-12" ]
            [ case vm.currentDesign of
                Just _ ->
                    text "Add components to vehicle from list above."

                Nothing ->
                    text "Components can be added after chassis has been chosen."
            ]
        ]
    ]


{-| Display middle panel
-}
middlePanel : Model -> List (Html Msg)
middlePanel model =
    case model.designerR.currentDesign of
        Just _ ->
            designPanel model

        Nothing ->
            designsPanel model


{-| Display designs section
-}
designsPanel : Model -> List (Html Msg)
designsPanel model =
    infoPanel
        { title = "Existing designs"
        , currentStatus = model.designerR.designsPanelStatus
        , openingMessage = DesignerMessage <| DesignsPanelStatusChanged InfoPanelOpen
        , closingMessage = DesignerMessage <| DesignsPanelStatusChanged InfoPanelClosed
        , refreshMessage = Just (DesignerMessage DesignsPanelUpdatedRequested)
        }
        (Just
            { pageSize = model.designerR.designsPageSize
            , currentPage = model.designerR.designsCurrentPage
            , maxPage =
                model.designerR.designs
                    |> SaveData.withDefault []
                    |> maxPage model.designerR.designsPageSize
            , pageChangedMessage = DesignerMessage << DesignsPanelPageChanged
            }
        )
        designsPanelContent
        model


{-| Display list of designs that are available for editing
-}
designsPanelContent : Model -> List (Html Msg)
designsPanelContent model =
    div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ table [ id "design-table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Chassis" ]
                        , th [] [ text "Size" ]
                        , th [] []
                        ]
                    ]
                , tbody []
                    (case SaveData.toMaybe model.designerR.designs of
                        Nothing ->
                            []

                        Just designs ->
                            List.sortWith (designSort (RemoteData.withDefault [] model.designerR.availableChassis)) designs
                                |> List.drop (model.designerR.designsCurrentPage * model.designerR.designsPageSize)
                                |> List.take model.designerR.designsPageSize
                                |> List.indexedMap
                                    (designsPanelEntry
                                        (RemoteData.withDefault [] model.designerR.availableChassis)
                                    )
                    )
                ]
            ]
        ]
        :: designsHelp model


{-| Help shown for existing designs
-}
designsHelp : Model -> List (Html Msg)
designsHelp _ =
    [ div [ class "row space-top" ]
        [ div [ class "col-lg-12" ]
            [ text "List of available designs is show above. You can choose one for editing by clicking its name."
            , text "Copying and deleting are done with the icons at the end of each row."
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ text "Copying creates a new design, using chassis and selected components of copied one." ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ text "Deleting will permanently remove design." ]
        ]
    ]


designsPanelEntry : List Chassis -> Int -> Design -> Html Msg
designsPanelEntry availableChassis index design =
    let
        chassis =
            findFirst (\x -> unChassisId x.id == unChassisId design.chassis.id) availableChassis

        chassisName =
            Maybe.map (\x -> unChassisName x.name) chassis
                |> Maybe.withDefault ""

        tonnage =
            Maybe.map (\x -> String.fromInt <| unChassisTonnage x.tonnage) chassis
                |> Maybe.withDefault ""

        idNumber =
            String.fromInt (index + 1)
    in
    tr []
        [ td [ id <| "design-entry-" ++ idNumber, onClick (DesignerMessage <| DesignSelected design) ] [ text <| unDesignName design.name ]
        , td [ onClick (DesignerMessage <| DesignSelected design) ] [ text chassisName ]
        , td [ onClick (DesignerMessage <| DesignSelected design) ] [ text tonnage ]
        , td [ class "icons" ]
            [ i [ id ("copy-design-" ++ idNumber), class "fas fa-copy", onClick (DesignerMessage <| DesignCopied design) ] []
            , i [ id ("delete-design-" ++ idNumber), class "fas fa-trash-alt small-space-left", onClick (DesignerMessage <| DesignDeleted design) ] []
            ]
        ]


{-| Sort designs based on the used chassis and name
-}
designSort : List Chassis -> Design -> Design -> Order
designSort availableChassis a b =
    let
        aCandidate =
            findFirst (\x -> unChassisId a.chassis.id == unChassisId x.id) availableChassis

        bCandidate =
            findFirst (\x -> unChassisId b.chassis.id == unChassisId x.id) availableChassis
    in
    case aCandidate of
        Nothing ->
            EQ

        Just aChassis ->
            case bCandidate of
                Nothing ->
                    EQ

                Just bChassis ->
                    case sortWithList [ sortChassisByType, sortChassisByName ] aChassis bChassis of
                        EQ ->
                            compare (unDesignName a.name) (unDesignName b.name)

                        ordering ->
                            ordering


{-| Use list of functions to find order of two items.
As soon as result different than EQ is found, it is returned
-}
sortWithList : List (a -> a -> Order) -> a -> a -> Order
sortWithList fns a b =
    case List.head fns of
        Nothing ->
            EQ

        Just fn ->
            let
                res =
                    fn a b
            in
            case res of
                EQ ->
                    case List.tail fns of
                        Nothing ->
                            EQ

                        Just rest ->
                            sortWithList rest a b

                ordering ->
                    ordering


{-| Display current design
-}
designPanel : Model -> List (Html Msg)
designPanel model =
    infoPanel
        { title = "Current design"
        , currentStatus = model.designerR.designPanelStatus
        , openingMessage = DesignerMessage <| DesignPanelStatusChanged InfoPanelOpen
        , closingMessage = DesignerMessage <| DesignPanelStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        designPanelContent
        model


{-| Contents of design panel
This view shows a single design
-}
designPanelContent : Model -> List (Html Msg)
designPanelContent model =
    case model.designerR.currentDesign of
        Nothing ->
            []

        Just design ->
            let
                chassis =
                    findFirst (\x -> x.id == design.chassis.id) (RemoteData.withDefault [] model.designerR.availableChassis)

                chassisType =
                    case chassis of
                        Nothing ->
                            "-"

                        Just c ->
                            chassisTypeToString c.chassisType

                tonnage =
                    String.fromInt <|
                        unWeight <|
                            sumOfDesignTonnage (RemoteData.withDefault [] model.designerR.availableComponents) design

                maxTonnage =
                    case chassis of
                        Nothing ->
                            "-"

                        Just c ->
                            String.fromInt <| unChassisTonnage c.tonnage

                armourSlots =
                    Maybe.map (\c -> c.armourSlots) chassis
                        |> withDefault (SlotAmount 0)

                innerSlots =
                    Maybe.map (\c -> c.innerSlots) chassis
                        |> withDefault (SlotAmount 0)

                outerSlots =
                    Maybe.map (\c -> c.outerSlots) chassis
                        |> withDefault (SlotAmount 0)

                sensorSlots =
                    Maybe.map (\c -> c.sensorSlots) chassis
                        |> withDefault (SlotAmount 0)

                weaponSlots =
                    Maybe.map (\c -> c.weaponSlots) chassis
                        |> withDefault (SlotAmount 0)

                engineSlots =
                    Maybe.map (\c -> c.engineSlots) chassis
                        |> withDefault (SlotAmount 0)

                motiveSlots =
                    Maybe.map (\c -> c.motiveSlots) chassis
                        |> withDefault (SlotAmount 0)

                sailSlots =
                    Maybe.map (\c -> c.sailSlots) chassis
                        |> withDefault (SlotAmount 0)

                cost =
                    case RemoteData.toMaybe model.designerR.availableComponents of
                        Nothing ->
                            Nothing

                        Just comps ->
                            Just <| totalCost comps design
            in
            [ div [ class "row" ]
                [ div [ class "col-lg-2 panel-sub-title" ] [ text "Name:" ]
                , div [ class "col-lg-4" ]
                    [ Html.input
                        [ id "ship-name-input"
                        , type_ "text"
                        , class "ship-name-input"
                        , placeholder "name for ship"
                        , value <| unDesignName design.name
                        , onInput (DesignerMessage << ShipNameChanged)
                        ]
                        []
                    ]
                , div [ class "col-lg-2 panel-sub-title" ] [ text "Type:" ]
                , div [ class "col-lg-4" ] [ text chassisType ]
                ]
            , div [ class "row" ]
                [ div [ class "col-lg-2 panel-sub-title" ] [ text "Slots:" ]
                , div [ class "col-lg-1 panel-table-heading" ] [ text "A" ]
                , div [ class "col-lg-1 panel-table-heading" ] [ text "I" ]
                , div [ class "col-lg-1 panel-table-heading" ] [ text "O" ]
                , div [ class "col-lg-1 panel-table-heading" ] [ text "S" ]
                , div [ class "col-lg-1 panel-table-heading" ] [ text "W" ]
                , div [ class "col-lg-1 panel-table-heading" ] [ text "E" ]
                , div [ class "col-lg-1 panel-table-heading" ] [ text "M" ]
                , div [ class "col-lg-1 panel-table-heading" ] [ text "Sa" ]
                ]
            , div [ class "row" ]
                [ div [ class "col-lg-2" ] []
                , div [ class "col-lg-1" ]
                    [ text <|
                        slotsLeftString (RemoteData.withDefault [] model.designerR.availableComponents)
                            armourSlots
                            design.components
                            ArmourSlot
                    ]
                , div [ class "col-lg-1" ]
                    [ text <|
                        slotsLeftString (RemoteData.withDefault [] model.designerR.availableComponents)
                            innerSlots
                            design.components
                            InnerSlot
                    ]
                , div [ class "col-lg-1" ]
                    [ text <|
                        slotsLeftString (RemoteData.withDefault [] model.designerR.availableComponents)
                            outerSlots
                            design.components
                            OuterSlot
                    ]
                , div [ class "col-lg-1" ]
                    [ text <|
                        slotsLeftString (RemoteData.withDefault [] model.designerR.availableComponents)
                            sensorSlots
                            design.components
                            SensorSlot
                    ]
                , div [ class "col-lg-1" ]
                    [ text <|
                        slotsLeftString (RemoteData.withDefault [] model.designerR.availableComponents)
                            weaponSlots
                            design.components
                            WeaponSlot
                    ]
                , div [ class "col-lg-1" ]
                    [ text <|
                        slotsLeftString (RemoteData.withDefault [] model.designerR.availableComponents)
                            engineSlots
                            design.components
                            EngineSlot
                    ]
                , div [ class "col-lg-1" ]
                    [ text <|
                        slotsLeftString (RemoteData.withDefault [] model.designerR.availableComponents)
                            motiveSlots
                            design.components
                            MotiveSlot
                    ]
                , div [ class "col-lg-1" ]
                    [ text <|
                        slotsLeftString (RemoteData.withDefault [] model.designerR.availableComponents)
                            sailSlots
                            design.components
                            SailSlot
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-lg-2 panel-sub-title" ] [ text "Tonnage:" ]
                , div [ class "col-lg-4" ]
                    [ text tonnage
                    , text " / "
                    , text maxTonnage
                    ]
                , div [ class "col-lg-1 panel-sub-title" ] [ text "Cost:" ]
                , div [ class "col-lg-5" ]
                    (biologicalsToText cost
                        ++ [ text " " ]
                        ++ mechanicalsToText cost
                        ++ [ text " " ]
                        ++ chemicalsToText cost
                    )
                ]
            , div [ class "row panel-sub-title" ]
                [ div [ class "col-lg-12" ] [ text "Components:" ] ]
            ]
                ++ List.map (designPanelEntry model.designerR.availableComponents)
                    (List.filter (\x -> unComponentAmount x.amount > 0)
                        (List.sortWith (plannedComponentSort <| RemoteData.withDefault [] model.designerR.availableComponents)
                            design.components
                        )
                    )
                ++ designPanelHelp model.designerR.currentDesign


{-| String representation how many slots of specific type are still left
-}
slotsLeftString : List Component -> SlotAmount -> List PlannedComponent -> ComponentSlot -> String
slotsLeftString availableComponents slots plannedComponents slot =
    if unSlotAmount slots > 0 then
        let
            slotsFree =
                unSlotAmount <| slotsLeft availableComponents slots plannedComponents slot
        in
        if slotsFree >= 0 then
            String.fromInt slotsFree

        else
            "0"

    else
        "-"


{-| How many slots of specific type are still unused?
-}
slotsLeft : List Component -> SlotAmount -> List PlannedComponent -> ComponentSlot -> SlotAmount
slotsLeft availableComponents slots plannedComponents slot =
    let
        components =
            installPlan availableComponents plannedComponents
                |> List.filter
                    (\( comp, _ ) ->
                        comp.slot == slot
                    )

        amounts =
            List.map (\( _, amount ) -> unComponentAmount amount) components

        unUsedSlots =
            unSlotAmount slots - List.sum amounts
    in
    SlotAmount unUsedSlots


{-| Sort planned components
-}
plannedComponentSort : List Component -> PlannedComponent -> PlannedComponent -> Order
plannedComponentSort comps a b =
    let
        ca =
            findFirst (\x -> x.id == a.id) comps

        cb =
            findFirst (\x -> x.id == b.id) comps
    in
    case ca of
        Nothing ->
            EQ

        Just aComp ->
            case cb of
                Nothing ->
                    EQ

                Just bComp ->
                    componentSort aComp bComp


{-| Total weight of planned components
-}
sumOfDesignTonnage : List Component -> Design -> Weight
sumOfDesignTonnage components design =
    Weight <| List.foldl (\a b -> (unWeight (plannedComponentWeight components a) * unComponentAmount a.amount) + b) 0 design.components


{-| Weight of planned component
-}
plannedComponentWeight : List Component -> PlannedComponent -> Weight
plannedComponentWeight components component =
    let
        match =
            findFirst (\x -> x.id == component.id) components
    in
    case match of
        Nothing ->
            Weight 0

        Just c ->
            c.weight


{-| Single entry in list of selected components
-}
designPanelEntry : WebData (List Component) -> PlannedComponent -> Html Msg
designPanelEntry components component =
    let
        comp =
            findFirst (\x -> x.id == component.id) (RemoteData.withDefault [] components)

        name =
            case comp of
                Nothing ->
                    ""

                Just c ->
                    unComponentName c.name

        amount =
            String.fromInt <| unComponentAmount component.amount
    in
    div [ class "row selected-component", onClick (DesignerMessage <| ComponentRemoved component) ]
        [ div [ class "col-lg-12" ]
            [ div [ class "row" ]
                [ div [ class "col-lg-1" ] [ text amount ]
                , div [ class "col-lg-11" ] [ text name ]
                ]
            ]
        ]


{-| Help text for design panel
-}
designPanelHelp : Maybe Design -> List (Html Msg)
designPanelHelp design =
    case design of
        Nothing ->
            [ div [ class "row" ]
                [ div [ class "col-lg-12" ]
                    [ text "" ]
                ]
            ]

        Just d ->
            [ div [ class "row" ]
                [ div [ class "col-lg-12" ]
                    [ text "List above shows components that are currently selected in the design. "
                    , text "Selecting a component in list will decrease planned amount. When amount "
                    , text "reaches zero, entry is removed."
                    ]
                ]
            ]
                ++ (if unDesignName d.name == "" then
                        [ div [ class "row" ]
                            [ div [ class "col-lg-12" ] [ text "Remember to give your design a name." ] ]
                        ]

                    else
                        []
                   )


{-| Display right panel
-}
rightPanel : Model -> List (Html Msg)
rightPanel model =
    commands model
        ++ messages model
        ++ briefStats model


{-| Display commands section
-}
commands : Model -> List (Html Msg)
commands model =
    infoPanel
        { title = "Commands"
        , currentStatus = model.designerR.commandsStatus
        , openingMessage = DesignerMessage <| CommandsStatusChanged InfoPanelOpen
        , closingMessage = DesignerMessage <| CommandsStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        commandsContent
        model


{-| Content of commands section
-}
commandsContent : Model -> List (Html Msg)
commandsContent model =
    let
        newClass =
            case model.designerR.currentDesign of
                Nothing ->
                    "disabled"

                Just _ ->
                    ""

        saveAttributes =
            case model.designerR.currentDesign of
                Nothing ->
                    [ id "save-button"
                    , class "btn btn-primary btn-sm command-button disabled"
                    ]

                Just design ->
                    let
                        validationMessages =
                            Maybe.map
                                (validateDesign (RemoteData.withDefault [] model.designerR.availableComponents)
                                    (RemoteData.withDefault [] model.designerR.availableChassis)
                                    (RemoteData.toMaybe model.designerR.designStats)
                                )
                                model.designerR.currentDesign
                                |> Maybe.withDefault []
                    in
                    if List.isEmpty validationMessages then
                        [ id "save-button"
                        , class "btn btn-primary btn-sm command-button"
                        , onClick <| DesignerMessage (SaveDesignRequested design)
                        ]

                    else
                        [ id "save-button"
                        , class "btn btn-primary btn-sm command-button disabled"
                        ]
    in
    [ div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ div [ id "clear-button", class ("btn btn-primary btn-sm command-button " ++ newClass), onClick <| DesignerMessage NewDesignStarted ] [ text "Clear" ]
            , div saveAttributes [ text "Save" ]
            ]
        ]
    ]


{-| Display messages section
-}
messages : Model -> List (Html Msg)
messages model =
    infoPanel
        { title = "Messages"
        , currentStatus = model.designerR.messagesStatus
        , openingMessage = DesignerMessage <| MessagesStatusChanged InfoPanelOpen
        , closingMessage = DesignerMessage <| MessagesStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        messagesContent
        model


{-| Content of messages section
-}
messagesContent : Model -> List (Html Msg)
messagesContent model =
    case model.designerR.currentDesign of
        Just _ ->
            let
                validationMessages =
                    Maybe.map
                        (validateDesign (RemoteData.withDefault [] model.designerR.availableComponents)
                            (RemoteData.withDefault [] model.designerR.availableChassis)
                            (RemoteData.toMaybe model.designerR.designStats)
                        )
                        model.designerR.currentDesign
                        |> withDefault []
            in
            if List.isEmpty validationMessages then
                [ div [ class "row" ]
                    [ div [ class "col-lg-12" ] [ text "Design ok" ] ]
                ]

            else
                List.map messageEntry validationMessages

        Nothing ->
            []


{-| Single entry in messages
-}
messageEntry : ValidationMessage -> Html Msg
messageEntry (ValidationMessage message) =
    div [ class "row" ]
        [ div [ class "col-lg-12" ] [ text message ] ]


{-| Display short summary of design's expected or known stats
-}
briefStats : Model -> List (Html Msg)
briefStats model =
    infoPanel
        { title = "Stats"
        , currentStatus = model.designerR.statsStatus
        , openingMessage = DesignerMessage <| StatsStatusChanged InfoPanelOpen
        , closingMessage = DesignerMessage <| StatsStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        briefStatsContent
        model


{-| Content of stats panel
-}
briefStatsContent : Model -> List (Html Msg)
briefStatsContent model =
    [ div []
        [ div [ class "row" ]
            [ div [ class "col-lg-3 panel-sub-title" ] [ text "Crew:" ]
            , div [ class "col-lg-9" ]
                [ RemoteData.map displayCrew model.designerR.designStats
                    |> RemoteData.withDefault "0 / 0"
                    |> text
                ]
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-3 panel-sub-title" ] [ text "Quarters:" ]
        , div [ class "col-lg-9" ]
            [ RemoteData.map displayCrewQuarters model.designerR.designStats
                |> RemoteData.withDefault "0 / 0 / 0"
                |> text
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-3" ] [ text " " ]
        , div [ class "col-lg-9" ]
            [ RemoteData.map displayQuartersRequirement model.designerR.designStats
                |> RemoteData.withDefault "-"
                |> text
            ]
        ]
    ]


{-| Block of text displaying crew space of unit stats
-}
displayCrewQuarters : UnitStats -> String
displayCrewQuarters stats =
    let
        steerage =
            Maybe.map (String.fromInt << unCrewSpace << .steerageSpace) stats.crewSpace
                |> Maybe.withDefault "unknown"

        standard =
            Maybe.map (String.fromInt << unCrewSpace << .standardSpace) stats.crewSpace
                |> Maybe.withDefault "unknown"

        luxury =
            Maybe.map (String.fromInt << unCrewSpace << .luxurySpace) stats.crewSpace
                |> Maybe.withDefault "unknown"
    in
    steerage ++ " / " ++ standard ++ " / " ++ luxury


{-| Text block for info if quarters are mandatory or not
-}
displayQuartersRequirement : UnitStats -> String
displayQuartersRequirement stats =
    case stats.crewSpaceRequired of
        Just CrewSpaceRequired ->
            "Quarters are mandatory"

        Just CrewSpaceOptional ->
            "Quarters are optional"

        Nothing ->
            "Quarters requirements are unknown"


{-| Block of text displaying crew space
-}
displayCrew : UnitStats -> String
displayCrew stats =
    let
        minimum =
            Maybe.map (String.fromInt << unCrewAmount << crewCount) stats.minimumCrew
                |> Maybe.withDefault "unknown"

        nominal =
            Maybe.map (String.fromInt << unCrewAmount << crewCount) stats.nominalCrew
                |> Maybe.withDefault "unknown"
    in
    minimum ++ " / " ++ nominal


{-| Request data needed by designer page from server
-}
init : Model -> Cmd Msg
init _ =
    Cmd.batch
        [ getAvailableComponents (DesignerMessage << ComponentsReceived)
        , getAvailableChassis (DesignerMessage << ChassisReceived)
        , getAvailableDesigns (DesignerMessage << DesignsReceived)
        ]


{-| Handle incoming messages
-}
update : DesignerRMsg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ChassisListStatusChanged status ->
            ( set (designerRA << chassisListStatusA) status model
            , Cmd.none
            )

        ChassisListUpdatedRequested ->
            ( set (designerRA << availableChassisA) Loading model
            , getAvailableChassis (DesignerMessage << ChassisReceived)
            )

        ChassisListPageChanged n ->
            ( set (designerRA << chassisCurrentPageA) n model
            , Cmd.none
            )

        ComponentListStatusChanged status ->
            ( set (designerRA << componentListStatusA) status model
            , Cmd.none
            )

        ComponentListUpdatedRequested ->
            ( set (designerRA << availableComponentsA) Loading model
            , getAvailableComponents (DesignerMessage << ComponentsReceived)
            )

        ComponentListPageChanged n ->
            ( set (designerRA << componentsCurrentPageA) n model
            , Cmd.none
            )

        DesignsPanelStatusChanged status ->
            ( set (designerRA << designsPanelStatusA) status model
            , Cmd.none
            )

        DesignsPanelUpdatedRequested ->
            ( set (designerRA << designsA) (RData Loading) model
            , getAvailableDesigns (DesignerMessage << DesignsReceived)
            )

        DesignsPanelPageChanged n ->
            ( set (designerRA << designsCurrentPageA) n model
            , Cmd.none
            )

        CommandsStatusChanged status ->
            ( set (designerRA << commandsStatusA) status model
            , Cmd.none
            )

        MessagesStatusChanged status ->
            ( set (designerRA << messagesStatusA) status model
            , Cmd.none
            )

        ChassisSelected chassis ->
            let
                newDesign =
                    { id = Nothing
                    , chassis =
                        { id = chassis.id
                        , level = ChassisLevel 1
                        }
                    , name = DesignName ""
                    , components = []
                    }
            in
            ( set (designerRA << currentDesignA) (Just newDesign) model
                |> set (designerRA << designStatsA) Loading
            , estimateDesign (DesignerMessage << DesignEstimated) newDesign
            )

        ComponentAdded component ->
            let
                newDesign =
                    addComponent component model.designerR.availableComponents model.designerR.currentDesign
            in
            case newDesign of
                Just design ->
                    ( set (designerRA << currentDesignA) newDesign model
                        |> set (designerRA << designStatsA) Loading
                    , estimateDesign (DesignerMessage << DesignEstimated) design
                    )

                Nothing ->
                    ( set (designerRA << currentDesignA) newDesign model
                    , Cmd.none
                    )

        ComponentRemoved component ->
            let
                newDesign =
                    over (try << componentsA << onEach) (removeComponent component.id) model.designerR.currentDesign
            in
            case newDesign of
                Just design ->
                    ( set (designerRA << currentDesignA) newDesign model
                        |> set (designerRA << designStatsA) Loading
                    , estimateDesign (DesignerMessage << DesignEstimated) design
                    )

                Nothing ->
                    ( set (designerRA << currentDesignA) newDesign model
                    , Cmd.none
                    )

        DesignPanelStatusChanged status ->
            ( set (designerRA << designPanelStatusA) status model
            , Cmd.none
            )

        ShipNameChanged name ->
            ( set (designerRA << currentDesignA << try << nameA) (DesignName name) model
            , Cmd.none
            )

        NewDesignStarted ->
            ( set (designerRA << currentDesignA) Nothing model
                |> set (designerRA << designStatsA) NotAsked
            , Cmd.none
            )

        SaveDesignRequested design ->
            ( model
            , saveDesign (DesignerMessage << DesignSaved) design
            )

        DesignSelected design ->
            ( set (designerRA << currentDesignA) (Just design) model
                |> set (designerRA << designStatsA) Loading
            , estimateDesign (DesignerMessage << DesignEstimated) design
              -- TODO: here we should be loading pre-existing stats
            )

        DesignCopied design ->
            ( set (designerRA << currentDesignA) (Just design) model
                |> set (designerRA << currentDesignA << try << idA) Nothing
                |> set (designerRA << currentDesignA << try << nameA) (DesignName "")
                |> set (designerRA << designStatsA) Loading
            , estimateDesign (DesignerMessage << DesignEstimated) design
              -- TODO: here we should be loading pre-existing stats
            )

        DesignDeleted design ->
            let
                designs =
                    case model.designerR.designs of
                        RData NotAsked ->
                            RData NotAsked

                        RData Loading ->
                            RData Loading

                        RData (Success d) ->
                            Saving d

                        RData (Failure err) ->
                            RData (Failure err)

                        Saving d ->
                            Saving d
            in
            ( set (designerRA << designsA) designs model
            , deleteDesign (DesignerMessage << DesignsReceived) design
            )

        StatsStatusChanged status ->
            ( set (designerRA << statsStatusA) status model
            , Cmd.none
            )

        ComponentsReceived NotAsked ->
            ( model
            , Cmd.none
            )

        ComponentsReceived Loading ->
            ( model
            , Cmd.none
            )

        ComponentsReceived (Success res) ->
            ( set (designerRA << availableComponentsA) (Success res) model
            , Cmd.none
            )

        ComponentsReceived (Failure err) ->
            ( over errorsA (\errors -> error err "Failed to load components" :: errors) model
                |> set (designerRA << availableComponentsA) (Failure err)
            , Cmd.none
            )

        ChassisReceived NotAsked ->
            ( model
            , Cmd.none
            )

        ChassisReceived Loading ->
            ( model
            , Cmd.none
            )

        ChassisReceived (Success res) ->
            ( set (designerRA << availableChassisA) (Success res) model
            , Cmd.none
            )

        ChassisReceived (Failure err) ->
            ( over errorsA (\errors -> error err "Failed to load chassis" :: errors) model
                |> set (designerRA << availableChassisA) (Failure err)
            , Cmd.none
            )

        DesignsReceived (RData NotAsked) ->
            ( model
            , Cmd.none
            )

        DesignsReceived (RData Loading) ->
            ( model
            , Cmd.none
            )

        DesignsReceived (RData (Success res)) ->
            ( set (designerRA << designsA) (RData (Success res)) model
            , Cmd.none
            )

        DesignsReceived (RData (Failure err)) ->
            ( over errorsA (\errors -> error err "Failed to load designs" :: errors) model
                |> set (designerRA << designsA) (RData (Failure err))
            , Cmd.none
            )

        DesignsReceived (Saving _) ->
            ( model
            , Cmd.none
            )

        DesignEstimated NotAsked ->
            ( model
            , Cmd.none
            )

        DesignEstimated Loading ->
            ( model
            , Cmd.none
            )

        DesignEstimated (Success res) ->
            ( set (designerRA << designStatsA) (Success res) model
            , Cmd.none
            )

        DesignEstimated (Failure err) ->
            ( over errorsA (\errors -> error err "Failed to estimate design stats" :: errors) model
                |> set (designerRA << designStatsA) (Failure err)
            , Cmd.none
            )

        DesignSaved NotAsked ->
            ( model
            , Cmd.none
            )

        DesignSaved Loading ->
            ( model
            , Cmd.none
            )

        DesignSaved (Success res) ->
            ( designSaveOk model res
            , Cmd.none
            )

        DesignSaved (Failure err) ->
            ( desginSaveFailure model err
            , Cmd.none
            )


{-| Add component to list of planned components of a design
-}
addComponent : Component -> WebData (List Component) -> Maybe Design -> Maybe Design
addComponent component components design =
    case components of
        NotAsked ->
            design

        Loading ->
            design

        Failure _ ->
            design

        Success _ ->
            case design of
                Nothing ->
                    design

                Just d ->
                    case findFirst (\x -> x.id == component.id) d.components of
                        Nothing ->
                            over (try << componentsA)
                                (\x ->
                                    List.append x
                                        [ { id = component.id
                                          , level = component.level
                                          , amount = ComponentAmount 1
                                          }
                                        ]
                                )
                                design

                        Just _ ->
                            over (try << componentsA << onEach)
                                (\x ->
                                    if x.id == component.id then
                                        { x | amount = ComponentAmount (1 + unComponentAmount x.amount) }

                                    else
                                        x
                                )
                                design


{-| Remove planned component from design
-}
removeComponent : ComponentId -> PlannedComponent -> PlannedComponent
removeComponent cId component =
    if cId == component.id then
        set amountA (ComponentAmount (Basics.max 0 (unComponentAmount component.amount - 1))) component

    else
        component


{-| There was failure while saving design, display an error message
-}
desginSaveFailure : Model -> Http.Error -> Model
desginSaveFailure model failure =
    over errorsA (\errors -> error failure "Failed to save design" :: errors) model


{-| Update model with saved design
-}
designSaveOk : Model -> Design -> Model
designSaveOk model design =
    case Maybe.map (findFirst (\x -> x.id == design.id)) (SaveData.toMaybe model.designerR.designs) of
        Just (Just _) ->
            over (designerRA << designsA << SaveData.try << onEach) (replaceDesign design) model
                |> over (designerRA << currentDesignA << try) (replaceDesign design)

        _ ->
            over (designerRA << designsA << SaveData.try) ((::) design) model
                |> over (designerRA << currentDesignA << try) (replaceDesign design)


{-| Given new design and existing one, replace old with new if ids match
-}
replaceDesign : Design -> Design -> Design
replaceDesign new old =
    case old.id of
        Nothing ->
            new

        Just oldId ->
            case new.id of
                Nothing ->
                    old

                Just newId ->
                    if unDesignId oldId == unDesignId newId then
                        new

                    else
                        old


isLoading : Model -> Bool
isLoading model =
    let
        vm =
            model.designerR
    in
    RemoteData.isLoading vm.availableComponents
        || RemoteData.isLoading vm.availableChassis
        || SaveData.isLoading vm.designs
        || RemoteData.isLoading vm.designStats
