module Types exposing (..)

import Http
import Focus exposing (Setter)

type Msg = AvailableComponents (Result Http.Error (List Component))
         | AddComponent Component
         | RemoveComponent Component

type alias Component = 
  { id : Int
  , name : String
  , description : String
  , weight : Int
  , slots : List EquipmentSlot
  , types : List EquipmentLevel
  , cost : Cost
  }

type EquipmentSlot = InnerSlot
                   | OuterSlot
                   | ArmourSlot

type EquipmentType = BridgeEquipment
                   | SensorEquipment
                   | EngineEquipment                 

type EquipmentLevel = EquipmentLevel Int EquipmentType

type InstalledComponent = InstalledComponent Component Int

type alias Cost =
  { mechanical : Int
  , biological : Int
  , chemical : Int }

type alias Ship =
  { components : List InstalledComponent }

type alias Chassis =
  { name : String
  , maxTonnage : Int 
  , requiredTypes : List EquipmentLevel }

type alias Model =
  { components : List Component
  , ship : Ship
  , chassis : Chassis
  }

modelComponents : Setter Model Model (List Component) (List Component)
modelComponents f model = { model | components = f model.components }

modelShipF : Setter Model Model Ship Ship
modelShipF f model = { model | ship = f model.ship }

shipComponentsF : Setter Ship Ship (List InstalledComponent) (List InstalledComponent)
shipComponentsF f ship = { ship | components = f ship.components }

totalTonnage : Ship -> Int
totalTonnage ship =
  List.foldr (\(InstalledComponent component amount) acc -> component.weight * amount + acc) 0 ship.components

totalCost : Ship -> Cost
totalCost ship =
  let
    sumCost (InstalledComponent component amount) acc = 
      Cost (component.cost.mechanical * amount + acc.mechanical) 
           (component.cost.biological * amount + acc.biological) 
           (component.cost.chemical * amount + acc.chemical)
  in
    List.foldr sumCost (Cost 0 0 0) ship.components

sortInstalledByAlpha : InstalledComponent -> InstalledComponent -> Order
sortInstalledByAlpha (InstalledComponent a _) (InstalledComponent b _) =
  compare a.name b.name

sortComponentByAlpha : Component -> Component -> Order
sortComponentByAlpha a b =
  compare a.name b.name
