module ViewModels.Planet exposing
    ( PlanetRMsg(..)
    , PlanetViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.Construction
    exposing
        ( BuildingInfo
        , Construction
        , ConstructionIndex
        )


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
    }
