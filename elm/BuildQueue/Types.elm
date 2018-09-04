module Types exposing (..)

import Http

type Msg = TextSearch String
  | NetworkMsg ApiMsg

type ApiMsg = BuildingsAvailable (Result Http.Error (List BuildingInfo))

type alias Model = 
  { searchText : String
  , availableBuildings : List BuildingInfo
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
