module Types exposing (..)

import Http
import Focus exposing (Setter)

type Msg = AvailableComponents (Result Http.Error (List Component))
         | AddComponent Component
         | RemoveComponent Component
         | NewShipName String
         | ChassisSelected (Maybe Int)

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
  { components : List InstalledComponent
  , name : String }

type alias Chassis =
  { id : Int
  , name : String
  , maxTonnage : Int 
  , requiredTypes : List EquipmentLevel }

type alias Model =
  { components : List Component
  , chassisList : List Chassis
  , ship : Ship
  , chassis : Maybe Chassis
  }

modelComponentsF : Setter Model Model (List Component) (List Component)
modelComponentsF f model = { model | components = f model.components }

modelShipF : Setter Model Model Ship Ship
modelShipF f model = { model | ship = f model.ship }

shipComponentsF : Setter Ship Ship (List InstalledComponent) (List InstalledComponent)
shipComponentsF f ship = { ship | components = f ship.components }

shipNameF : Setter Ship Ship String String
shipNameF f ship = { ship | name = f ship.name }

modelChassisF : Setter Model Model (Maybe Chassis) (Maybe Chassis)
modelChassisF f model = { model | chassis = f model.chassis }

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

totalOrdnance : Ship -> Int
totalOrdnance ship = 0

totalSupply : Ship -> Int
totalSupply ship = 0

sortInstalledByAlpha : InstalledComponent -> InstalledComponent -> Order
sortInstalledByAlpha (InstalledComponent a _) (InstalledComponent b _) =
  compare a.name b.name

sortComponentByAlpha : Component -> Component -> Order
sortComponentByAlpha a b =
  compare a.name b.name
