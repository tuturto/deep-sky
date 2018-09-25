module Types exposing (..)

import Http

type Msg = TextSearch String
  | NetworkMsg ApiMsg
  | UiMsg Action

type ApiMsg = BuildingInfoLoaded (Result Http.Error (List BuildingInfo))
  | BuildingsLoaded (Result Http.Error (List Building))
  | PopulationLoaded (Result Http.Error (List Population))
  | PlanetDetailsLoaded (Result Http.Error PlanetDetails)
  | ConstructionsLoaded (Result Http.Error (List Construction))

type Action = AddBuildingIntoQueue BuildingInfo
  | DeleteBuildingFromQueue BuildingConstructionData
  | MoveBuilding BuildingConstructionData Int

type alias Model = 
  { searchText : String
  , buildings : List Building
  , availableBuildings : List BuildingInfo
  , messages : List String
  , planetId : Int
  , population : List Population
  , planetDetails : Maybe PlanetDetails
  , constructionQueue : List Construction
  }

type alias Cost =
  { mechanical : Int
  , biological : Int
  , chemical : Int 
  }

type alias BuildingInfo =
  { buildingType : BuildingType
  , level : Int
  , name : String 
  , cost : Cost
  , description : String
  }

type BuildingType = SensorStation
  | ResearchComplex
  | Farm
  | ParticleAccelerator
  | NeutronDetector
  | BlackMatterScanner
  | GravityWaveSensor

type alias Building = 
  { id: Int
  , buildingType : BuildingType
  , damage : Float
  , updated : Int
  , level : Int
  }

type alias Population =
  { race : String
  , population : Int
  , updated : Int
  }

type alias PlanetDetails =
  { id : Int
  , systemId : Int
  , name : String
  , position : Int
  , gravity : Float
  , updated : Int
  }

type Construction = BuildingConstruction BuildingConstructionData
  | ShipConstruction ShipConstructionData

type alias BuildingConstructionData =
  { id : Int
  , name : String
  , index : Int
  , level : Int
  , buildingType : BuildingType
  , planet : Int
  }

-- TODO: ship type?
type alias ShipConstructionData =
  { id : Int
  , name : String
  , shipType : String
  , index : Int
  }

constructionIndex : Construction -> Int
constructionIndex c =
  case c of
    BuildingConstruction x -> x.index
    ShipConstruction x -> x.index

constructionIndexSorter : Construction -> Construction -> Order
constructionIndexSorter a b =
  let
    index_a = constructionIndex a
    index_b = constructionIndex b
  in
    compare index_a index_b
