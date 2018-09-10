module Types exposing (..)

import Http

type Msg = TextSearch String
  | NetworkMsg ApiMsg

type ApiMsg = BuildingInfoLoaded (Result Http.Error (List BuildingInfo))
  | BuildingsLoaded (Result Http.Error (List Building))
  | PopulationLoaded (Result Http.Error (List Population))

type alias Model = 
  { searchText : String
  , buildings : List Building
  , availableBuildings : List BuildingInfo
  , messages : List String
  , planetId : Int
  , population : List Population
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
