module Api.Endpoints exposing (Endpoint(..), endpointToString)

import Data.Common
    exposing
        ( PlanetId
        , planetIdToString
        , unConstructionId
        )
import Data.Construction exposing (Construction(..))


type Endpoint
    = ApiStarDate
    | ApiResources
    | ApiStarSystem
    | ApiStar
    | ApiPlanet
    | ApiPopulation PlanetId
    | ApiBuilding PlanetId
    | ApiConstructionQueue PlanetId
    | ApiConstruction Construction
    | ApiBuildingConstruction
    | ApiAvailableBuildings


endpointToString : Endpoint -> String
endpointToString endpoint =
    case endpoint of
        ApiStarDate ->
            "/api/stardate"

        ApiResources ->
            "/api/resources"

        ApiStarSystem ->
            "/api/starsystem"

        ApiStar ->
            "/api/star"

        ApiPlanet ->
            "/api/planet/"

        ApiPopulation planetId ->
            "/api/planet/" ++ planetIdToString planetId ++ "/population"

        ApiBuilding planetId ->
            "/api/planet/" ++ planetIdToString planetId ++ "/buildings"

        ApiConstructionQueue planetId ->
            "/api/construction/planet/" ++ planetIdToString planetId

        ApiConstruction construction ->
            case construction of
                BuildingConstruction data ->
                    "/api/construction/building/" ++ (String.fromInt <| unConstructionId data.id)

                ShipConstruction data ->
                    "/api/construction/ship/" ++ (String.fromInt <| unConstructionId data.id)

        ApiBuildingConstruction ->
            "/api/construction/building"

        ApiAvailableBuildings ->
            "/api/construction/buildings"
