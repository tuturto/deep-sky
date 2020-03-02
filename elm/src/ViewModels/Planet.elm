module ViewModels.Planet exposing
    ( PlanetRMsg(..)
    , PlanetViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.Construction
    exposing
        ( Building
        , BuildingInfo
        , Construction
        , ConstructionIndex
        )
import Data.StarSystem
    exposing
        ( Planet
        , PlanetStatus
        , Population
        )
import RemoteData exposing (RemoteData(..), WebData)


{-| Messages that planet view might create
-}
type PlanetRMsg
    = PlanetDetailsStatusChanged InfoPanelStatus
    | PopulationStatusChanged InfoPanelStatus
    | BuildingsStatusChanged InfoPanelStatus
    | LandedShipsStatusChanged InfoPanelStatus
    | OrbitingShipsStatusChanged InfoPanelStatus
    | ConstructionStatusChanged InfoPanelStatus
    | PlanetStatusesStatusChanged InfoPanelStatus
    | MoveConstruction Construction ConstructionIndex
    | DeleteConstructionFromQueue Construction
    | BuildingSearch String
    | ClearBuildingSearch
    | QueueConstruction BuildingInfo
    | PlanetDetailsReceived (WebData Planet)
    | PopulationReceived (WebData (List Population))
    | BuildingsReceived (WebData (List Building))
    | AvailableBuildingsReceived (WebData (List BuildingInfo))
    | PlanetStatusReceived (WebData PlanetStatus)
    | ConstructionsReceived (WebData (List Construction))


{-| Record that holds information regarding to planet view.
This is used to hold state of the planet view, for example is
certain info panel open or closed.
-}
type alias PlanetViewModel =
    { planetDetailsStatus : InfoPanelStatus
    , populationStatus : InfoPanelStatus
    , buildingsStatus : InfoPanelStatus
    , landedShipsStatus : InfoPanelStatus
    , orbitingShipsStatus : InfoPanelStatus
    , constructionStatus : InfoPanelStatus
    , buildingSearchText : String
    , planetStatusesStatus : InfoPanelStatus
    , planet : WebData Planet
    , planetStatus : WebData PlanetStatus
    , populations : WebData (List Population)
    , buildings : WebData (List Building)
    , availableBuildings : WebData (List BuildingInfo)
    , constructions : WebData (List Construction)
    }


{-| Create initial planet view model
-}
init : PlanetViewModel
init =
    { planetDetailsStatus = InfoPanelOpen
    , populationStatus = InfoPanelOpen
    , buildingsStatus = InfoPanelOpen
    , landedShipsStatus = InfoPanelOpen
    , orbitingShipsStatus = InfoPanelOpen
    , constructionStatus = InfoPanelOpen
    , buildingSearchText = ""
    , planetStatusesStatus = InfoPanelOpen
    , planet = Loading
    , planetStatus = Loading
    , populations = Loading
    , buildings = Loading
    , availableBuildings = Loading
    , constructions = Loading
    }
