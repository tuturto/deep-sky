module Render exposing (view)

import Types exposing (Model, Msg (ChassisSelected))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (..)
import Types exposing (..)
import Validation exposing (validateDesign)
import Json.Decode as Decode

statisticsPanel : Model -> Html Msg
statisticsPanel model =
  let
    cost = totalCost model.ship
    maxTonnage = case model.chassis of 
                        Just chassis ->
                          toString chassis.maxTonnage
                        Nothing -> "-"
  in      
    div [ class "design-panel" ]
    [ div [ class "row" ]
      [ div [ class "col-lg-12 title-left" ]
      [ text "Design statistics" ]
    ]
    , div [ class "row side-panel" ]
      [ div [ class "col-lg-4" ]
        [ text "Name" ]
      , div [ class "col-lg-8 editor-text" ]
        [ input [ type_ "text", placeholder "Enter name", onInput NewShipName, style [ ("width", "100%") ] ] [] ]
      ]
    , div [ class "row side-panel" ]
      [ div [ class "col-lg-4" ]
        [ text "Type" ]
      , div [ class "col-lg-8" ]
        [ select [ on "change" (Decode.map ChassisSelected targetValueMaybeInt), style [ ("width", "100%") ] ]
          <| chassisOptions model.chassisList 
        ]
      ]
    , div [ class "row side-panel" ]
      [ div [ class "col-lg-4" ]
        [ text "Tonnage" ]
      , div [ class "col-lg-8" ]
        [ text <| toString <| totalTonnage model.ship
        , text " / "
        , text maxTonnage ]
      ]
    , div [ class "row side-panel" ]
      [ div [ class "col-lg-4" ]
        [ text "Shields" ]
      , div [ class "col-lg-8" ]
        [ text "0" ]
      ]
    , div [ class "row side-panel" ]
      [ div [ class "col-lg-4" ]
        [ text "Ordnance" ]
      , div [ class "col-lg-8" ]
        [ text <| toString <| totalOrdnance model.ship ]
      ]
    , div [ class "row side-panel" ]
      [ div [ class "col-lg-4" ]
        [ text "Supply" ]
      , div [ class "col-lg-8" ]
        [ text <| toString <| totalSupply model.ship ]
      ]
    , div [ class "row side-panel" ]
      [ div [ class "col-lg-4" ]
        [ text "Cost" ]
      ]
    , div [ class "row side-panel" ]
      [ div [ class "col-lg-11 col-lg-offset-1" ]
        [ costDisplay <| totalCost model.ship ]
      ]     
    , div [ class "row side-panel-left" ]
      [ div [ class "col-lg-11" ]
        []
      ]
    ]

chassisSorter : Chassis -> Chassis -> Order
chassisSorter a b = compare a.name b.name

chassisOptions : List Chassis -> List (Html Msg)
chassisOptions chassisList =
  let 
    chassisOption x = option [ value <| toString x.id ] [ text x.name ]
  in
    List.append [ option [] [] ]
    <| List.map chassisOption <| List.sortWith chassisSorter chassisList

selectableComponent : Component -> Html Msg
selectableComponent component =
  div [ onClick <| AddComponent component ] 
  [ div [ class "row side-panel" ]
    [ div [ class "col-lg-12 component-title" ]
      [ text component.name ]
    ]
  , div [ class "row side-panel" ]
    [ div [ class "col-lg-1 col-lg-offset-1" ]
      [ text <| toString component.weight ]
    , div [ class "col-lg-2" ]
      [ equipmentSlotIndicator component.slots ]
    ]
  ]

selectedComponent : InstalledComponent -> Html Msg
selectedComponent (InstalledComponent component amount) =
  div [] 
  [ div [ class "row" ]
    [ div [ class "col-lg-12 component-title" ]
      [ text component.name
      , div [ class "btn btn-outline-dark btn-sm"
               , onClick <| RemoveComponent component ] 
        [ text " - "]
      , text <| toString amount
      , div [ class "btn btn-outline-dark btn-sm"
            , onClick <| AddComponent component ] 
        [ text " + "]
      ]
    ]
  , div [class "row" ]
    [ div [ class "col-lg-11 col-lg-offset-1" ]
      [ text component.description ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-offset-1 col-lg-10" ]
      [ i [] <| List.intersperse (text ",") <| List.map componentTypes component.types ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-1 col-lg-offset-1" ]
      [ text <| toString component.weight ]
    , div [ class "col-lg-2" ]
        [ equipmentSlotIndicator component.slots ]
    , div [ class "col-lg-6" ]
        [ costDisplay component.cost ]
    ]
  ]

costDisplay : Cost -> Html Msg
costDisplay cost =
  div [ ]
  [ i [ class "fas fa-leaf" ] []
  , text <| " " ++ (toString <| cost.biological) ++ " "
  , i [ class "fas fa-cogs" ] []
  , text <| " " ++ (toString <| cost.mechanical) ++ " "
  , i [ class "fas fa-flask" ] []
  , text <| " " ++ (toString <| cost.chemical) ++ " "
  ]

componentTypes : EquipmentLevel -> Html Msg
componentTypes (EquipmentLevel lvl eqType) =
    text <| eqTypeToString eqType ++ " (" ++ toString lvl ++ ")"

eqTypeToString : EquipmentType -> String
eqTypeToString eqt =
  case eqt of
    BridgeEquipment -> "Bridge"
    SensorEquipment -> "Sensors"
    EngineEquipment -> "Engines"

equipmentSlotIndicator : List EquipmentSlot -> Html Msg
equipmentSlotIndicator slots =
  let slotToString s = 
    case s of
      InnerSlot -> "I"
      OuterSlot -> "O"
      ArmourSlot -> "A"
  in
    text <| List.foldr (++) "" <| List.intersperse "," <| List.map slotToString slots

componentList : Model -> Html Msg
componentList model =
  div [ class "design-panel" ]
  [ div [ class "row" ]
    [ div [ class "col-lg-12 title-left" ]
      [ text "Components" ]
    ]
  , div [] <| List.map selectableComponent <| List.sortWith sortComponentByAlpha model.components     
  , div [ class "row side-panel-left" ]
    [ div [ class "col-lg-11" ]
      []
    ]
  ]
      
leftPanel : Model -> Html Msg
leftPanel model =
  div []
  [ statisticsPanel model
  , componentList model
  ]

middlePanel : Model -> Html Msg
middlePanel model =
  div []
  [ div [ class "row design-panel middle-panel" ]
    [ div [ class "row" ]
      [ div [ class "col-lg-12 design-panel-title" ]
        [ text "Selected components" ]
      ]        
    ]
  , div [] <| List.map selectedComponent <| List.sortWith sortInstalledByAlpha model.ship.components    
  ]

warningMessages : List String -> List (Html Msg)
warningMessages s =
  let 
    mapper err = div [ class "row side-panel" ]
                 [ div [ class "col-lg-12" ]
                   [ text err]
                 ]
  in
    List.map mapper s


rightPanel : Model -> Html Msg
rightPanel model =
  div []
  [ div [ class "design-panel" ]
    [ div [ class "row" ]
      [ div [ class "col-lg-12 title-right" ]
        [ text "Warnings" ]
      ]
    ]  
  , div [] <| warningMessages <| validateDesign model       
  ]

createErrorEntry : String -> Html Msg
createErrorEntry err =
  div [ class "row" ]
  [ div [ class "col-lg-12" ]
    [ text err]
  ]

view : Model -> Html Msg
view model =
  div []
  [
    div [ class "row error-bar" ] 
    [ div [ class "col-lg-12" ]
      <| List.map createErrorEntry model.errors
    ]
  , div [ class "row" ] 
    [ div [ class "col-lg-3" ]
      [ leftPanel model ]
    , div [ class "col-lg-5" ]
      [ middlePanel model ]
    , div [ class "col-lg-4" ]
      [ rightPanel model ]
    ]
  ]