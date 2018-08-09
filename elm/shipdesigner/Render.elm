module Render exposing (view)

import Types exposing (Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)
import Validation exposing (validateDesign)

statisticsPanel : Model -> Html Msg
statisticsPanel model =
  let
    cost = totalCost model.ship
  in      
    div [ class "design-panel" ]
    [ div [ class "row" ]
      [ div [ class "col-lg-12 design-panel-title" ]
      [ text "Design statistics" ]
    ]
    , div [ class "row" ]
      [ div [ class "col-lg-4" ]
        [ text "Name" ]
      , div [ class "col-lg-8" ]
        [ text "S.S.S. Kickstart" ]
      ]
    , div [ class "row" ]
      [ div [ class "col-lg-4" ]
        [ text "Type" ]
      , div [ class "col-lg-8" ]
        [ text model.chassis.name ]
      ]
    , div [ class "row" ]
      [ div [ class "col-lg-4" ]
        [ text "Tonnage" ]
      , div [ class "col-lg-8" ]
        [ text <| toString <| totalTonnage model.ship
        , text " / "
        , text <| toString model.chassis.maxTonnage ]
      ]
    , div [ class "row" ]
      [ div [ class "col-lg-4" ]
        [ text "Shields" ]
      , div [ class "col-lg-8" ]
        [ text "0" ]
      ]
    , div [ class "row" ]
      [ div [ class "col-lg-4" ]
        [ text "Ordnance" ]
      , div [ class "col-lg-8" ]
        [ text "15" ]
      ]
    , div [ class "row" ]
      [ div [ class "col-lg-4" ]
        [ text "Supply" ]
      , div [ class "col-lg-8" ]
        [ text "100" ]
      ]
    , div [ class "row" ]
      [ div [ class "col-lg-4" ]
        [ text "Cost" ]
      ]
    , div [ class "row" ]
      [ div [ class "col-lg-11 col-lg-offset-1" ]
        [ costDisplay <| totalCost model.ship ]
      ]
    ]

selectableComponent : Component -> Html Msg
selectableComponent component =
  div [ onClick <| AddComponent component ] 
  [ div [ class "row" ]
    [ div [ class "col-lg-12" ]
      [ text component.name ]
    ]
  , div [ class "row" ]
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
    [ div [ class "col-lg-12" ]
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
    <| List.append 
      [ div [ class "row" ]
        [ div [ class "col-lg-12 design-panel-title" ]
          [ text "Components" ]
        ]     
      ]
      <| List.map selectableComponent <| List.sortWith sortComponentByAlpha model.components

leftPanel : Model -> Html Msg
leftPanel model =
  div []
  [ statisticsPanel model
  , componentList model
  ]

middlePanel : Model -> Html Msg
middlePanel model =
  div []
  [ div [ class "row design-panel" ]
    <| List.append
    [ div [ class "row" ]
      [ div [ class "col-lg-12 design-panel-title" ]
        [ text "Selected components" ]
      ]        
    ]
    <| List.map selectedComponent <| List.sortWith sortInstalledByAlpha model.ship.components
  ]

warningMessages : List String -> List (Html Msg)
warningMessages s =
  let 
    mapper err = div [ class "row" ]
                 [ div [ class "col-lg-12" ]
                   [ text err]
                 ]
  in
    List.map mapper s


rightPanel : Model -> Html Msg
rightPanel model =
  div []
  <| List.append
    [ div [ class "design-panel" ]
      [ div [ class "row" ]
        [ div [ class "col-lg-12 design-panel-title" ]
          [ text "Warnings" ]
        ]
      ]      
    ]
    <| warningMessages <| validateDesign model   

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ div [ class "row" ] 
      [ div [ class "col-lg-3" ]
        [ leftPanel model ]
      , div [ class "col-lg-5" ]
        [ middlePanel model ]
      , div [ class "col-lg-4" ]
        [ rightPanel model ]
      ]
    ]
