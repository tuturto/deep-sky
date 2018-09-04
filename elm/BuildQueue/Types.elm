module Types exposing (..)

type Msg = 
  TextSearch String

type alias Model = 
  { searchText : String
  }

type alias Cost =
  { mechanical : Int
  , biological : Int
  , chemical : Int 
  }

type alias Building =
  { buildingType : BuildingType
  , name : String
  , level : Int
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
