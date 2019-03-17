module Api.Endpoints exposing (Endpoint(..), endpointToString)

import Data.Common
    exposing
        ( MessageId
        , PlanetId
        , constructionIdToString
        , messageIdToString
        , planetIdToString
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
    | ApiMessageList
    | ApiSingleMessage MessageId
    | ApiIcon
    | ApiPlanetStatus PlanetId
    | ApiAvailableResearch
    | ApiCurrentResearch
    | ApiResearchProduction


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
                    "/api/construction/building/" ++ constructionIdToString data.id

                ShipConstruction data ->
                    "/api/construction/ship/" ++ constructionIdToString data.id

        ApiBuildingConstruction ->
            "/api/construction/building"

        ApiAvailableBuildings ->
            "/api/construction/buildings"

        ApiMessageList ->
            "/api/message"

        ApiSingleMessage messageId ->
            "/api/message/" ++ messageIdToString messageId

        ApiIcon ->
            "/api/icon"

        ApiPlanetStatus planetId ->
            "/api/planet/" ++ planetIdToString planetId ++ "/status"

        ApiAvailableResearch ->
            "/api/research/available"

        ApiCurrentResearch ->
            "/api/research/current"

        ApiResearchProduction ->
            "/api/research/production"
