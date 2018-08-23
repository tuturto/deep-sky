module Types exposing (..)

import Http
import Focus exposing (Setter)

type Msg = AvailableComponents (Result Http.Error (List Component))
         | AvailableChassis (Result Http.Error (List Chassis))
         | AvailableDesigns (Result Http.Error (List ShipDto))
         | AddComponent Component
         | RemoveComponent Component
         | NewShipName String
         | ChassisSelected (Maybe Int)
         | SaveDesign
         | DesignSaved (Result Http.Error ShipDto)
         | ShowLoadPanel
         | LoadDesign Ship
         | CancelLoad
         | ResetDesign
         | NewDesign
         | CopyDesign
         | DeleteDesign Ship
         | DesignDeleted (Result Http.Error Int)

type alias Component = 
  { id : ComponentId
  , name : String
  , description : String
  , weight : Int
  , slot : ComponentSlot
  , types : List ComponentLevel
  , cost : Cost
  , level : Int
  }

type alias ComponentDto = 
  { id : ComponentId
  , level : Int
  }

type ComponentSlot = InnerSlot
                   | OuterSlot
                   | ArmourSlot
                   | SensorSlot
                   | WeaponSlot
                   | EngineSlot

type ComponentType = BridgeComponent
                   | SensorComponent
                   | EngineComponent
                   | SupplyComponent

type ComponentLevel = ComponentLevel Int ComponentType

type InstalledComponent = InstalledComponent Component Int

type InstalledComponentDto = InstalledComponentDto ComponentDto Int

type ComponentId = CidLongRangeSensors
  | CidArmour
  | CidBridge
  | CidEngine
  | CidSupplyPod

type alias Cost =
  { mechanical : Int
  , biological : Int
  , chemical : Int 
  }

type alias Ship =
  { components : List InstalledComponent
  , name : String 
  , chassis : Maybe Chassis
  , id : Maybe Int 
  }

type alias ShipDto =
  { components : List InstalledComponentDto
  , name : String 
  , id : Maybe Int 
  , chassis : Int
  }

type alias Chassis =
  { id : Int
  , name : String
  , maxTonnage : Int 
  , requiredTypes : List ComponentLevel 
  }

type ProgramMode = EditMode
  | LoadMode

type alias Model =
  { components : List Component
  , chassisList : List Chassis
  , ship : Ship  
  , errors : List String
  , mode : ProgramMode
  , designList : List Ship
  }

modelErrorsF : Setter Model Model (List String) (List String)
modelErrorsF f model = { model | errors = f model.errors }

modelComponentsF : Setter Model Model (List Component) (List Component)
modelComponentsF f model = { model | components = f model.components }

modelChassisListF : Setter Model Model (List Chassis) (List Chassis)
modelChassisListF f model = { model | chassisList = f model.chassisList }

modelDesignsF : Setter Model Model (List Ship) (List Ship)
modelDesignsF f model = { model | designList = f model.designList }

modelShipF : Setter Model Model Ship Ship
modelShipF f model = { model | ship = f model.ship }

shipComponentsF : Setter Ship Ship (List InstalledComponent) (List InstalledComponent)
shipComponentsF f ship = { ship | components = f ship.components }

shipNameF : Setter Ship Ship String String
shipNameF f ship = { ship | name = f ship.name }

shipChassisF : Setter Ship Ship (Maybe Chassis) (Maybe Chassis)
shipChassisF f ship = { ship | chassis = f ship.chassis }

shipIdF : Setter Ship Ship (Maybe Int) (Maybe Int)
shipIdF f ship = { ship | id = f ship.id }

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

levelSupply : Int -> ComponentLevel -> Int
levelSupply amount (ComponentLevel supply componentType) =
  case componentType of
    SupplyComponent -> amount * supply
    _ -> 0

componentSupply : Int -> Component -> Int
componentSupply amount component = 
  List.map (levelSupply amount) component.types
  |> List.sum

totalOrdnance : Ship -> Int
totalOrdnance ship = 0

totalSupply : Ship -> Int
totalSupply ship =
  List.map (\(InstalledComponent component amount) -> componentSupply amount component) ship.components
  |> List.sum

sortInstalledByAlpha : InstalledComponent -> InstalledComponent -> Order
sortInstalledByAlpha (InstalledComponent a _) (InstalledComponent b _) =
  compare a.name b.name

sortComponentByAlpha : Component -> Component -> Order
sortComponentByAlpha a b =
  compare a.name b.name

sortDesignsByTypeAndAlpha : Ship -> Ship -> Order
sortDesignsByTypeAndAlpha a b =
  case a.chassis of
    Nothing -> 
      case b.chassis of
        Nothing -> compare a.name b.name
        Just chb -> LT
    Just cha -> 
      case b.chassis of
        Nothing -> GT
        Just chb -> 
          case compare cha.name chb.name of
            GT -> GT
            LT -> LT
            EQ -> compare a.name b.name
