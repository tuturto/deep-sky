module Render exposing (view)

import Types exposing (Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)
import Validation exposing (validateDesign)

statisticsPanel : Model -> Html Msg
statisticsPanel model =
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
      [ i [ class "fas fa-leaf" ] []
      , text "150 "
      , i [ class "fas fa-cogs" ] []
      , text "150 "
      , i [ class "fas fa-flask" ] []
      , text "150"
      ]
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
    , div [ class "col-lg-1" ]
        <| List.map equipmentSlotIndicator component.slots
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
    [ div [ class "col-lg-1 col-lg-offset-1" ]
      [ text <| toString component.weight ]
    , div [ class "col-lg-1" ]
        <| List.map equipmentSlotIndicator component.slots
    ]
  ]

equipmentSlotIndicator : EquipmentSlot -> Html Msg
equipmentSlotIndicator slot =
  case slot of
    InnerSlot -> text "I"
    OuterSlot -> text "O"
    ArmourSlot -> text "A"

componentList : Model -> Html Msg
componentList model =
  div [ class "design-panel" ]
    <| List.append 
      [ div [ class "row" ]
        [ div [ class "col-lg-12 design-panel-title" ]
          [ text "Components" ]
        ]     
      ]
      <| List.map selectableComponent model.components

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
    <| List.map selectedComponent model.ship.components
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
