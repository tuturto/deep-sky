module Validation exposing (validateDesign)

import Types exposing (..)

validateDesign : Model -> List String
validateDesign model =
  List.filterMap identity <| List.concatMap (\x -> x model) validators

type alias ShipValidator = Model -> List (Maybe String)

tonnageCheck : ShipValidator
tonnageCheck model =
  case model.chassis of
    Just chassis ->
      if totalTonnage model.ship > chassis.maxTonnage
      then [ Just "Ship design exceeds max tonnage" ]
      else []
    Nothing -> []

equipmentRequirementToString : ComponentLevel -> String
equipmentRequirementToString (ComponentLevel lvl eType) =
  case eType of
    BridgeComponent ->
      "at least " ++ (toString lvl) ++ " points worth of bridges is required"
    SensorComponent ->
      "at least " ++ (toString lvl) ++ " points worth of sensors is required"
    EngineComponent ->
      "at least " ++ (toString lvl) ++ " points worth of engines is required"

componentCheck : ShipValidator
componentCheck model =
  let 
    checkSingle (ComponentLevel n equipment) = 
      let 
        matching = List.filter (\(InstalledComponent comp level) -> 
          List.any (\(ComponentLevel _ eType) -> eType == equipment) comp.types) model.ship.components
        total = List.foldr (\(InstalledComponent _ lvl) acc -> acc + lvl) 0 matching
      in
        if total >= n
        then Nothing
        else Just <| equipmentRequirementToString (ComponentLevel n equipment)
  in
    case model.chassis of
      Just chassis -> 
        List.map checkSingle chassis.requiredTypes
      Nothing -> []

chassisSelectedCheck : ShipValidator
chassisSelectedCheck model =
  case model.chassis of
    Just x -> []
    Nothing -> [ Just "Chassis has not been selected" ]

validators : List ShipValidator
validators = [ tonnageCheck 
             , componentCheck
             , chassisSelectedCheck ]
