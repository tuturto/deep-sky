module Validation exposing (validateDesign)

import Types exposing (..)

validateDesign : Model -> List String
validateDesign model =
  List.filterMap identity <| List.concatMap (\x -> x model) validators

type alias ShipValidator = Model -> List (Maybe String)

tonnageCheck : ShipValidator
tonnageCheck model =
  if totalTonnage model.ship > model.chassis.maxTonnage
  then [ Just "Ship design exceeds max tonnage" ]
  else [ Nothing ]

equipmentRequirementToString : EquipmentLevel -> String
equipmentRequirementToString (EquipmentLevel lvl eType) =
  case eType of
    BridgeEquipment ->
      "at least " ++ (toString lvl) ++ " points worth of bridges is required"
    SensorEquipment ->
      "at least " ++ (toString lvl) ++ " points worth of sensors is required"
    EngineEquipment ->
      "at least " ++ (toString lvl) ++ " points worth of engines is required"

componentCheck : ShipValidator
componentCheck model =
  let 
    checkSingle (EquipmentLevel n equipment) = 
      let 
        matching = List.filter (\(InstalledComponent comp level) -> 
          List.any (\(EquipmentLevel _ eType) -> eType == equipment) comp.types) model.ship.components
        total = List.foldr (\(InstalledComponent _ lvl) acc -> acc + lvl) 0 matching
      in
        if total >= n
        then Nothing
        else Just <| equipmentRequirementToString (EquipmentLevel n equipment)
  in
    List.map checkSingle model.chassis.requiredTypes

validators : List ShipValidator
validators = [ tonnageCheck 
             , componentCheck ]
