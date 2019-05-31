module Api.Endpoints exposing (Endpoint(..), endpointToString)

import Data.Common
    exposing
        ( DesignId
        , MessageId
        , PersonId
        , PlanetId
        , StarSystemId
        , constructionIdToString
        , designIdToString
        , messageIdToString
        , personIdToString
        , planetIdToString
        , starSystemIdToString
        )
import Data.Construction exposing (Construction(..))


type Endpoint
    = ApiStarDate
    | ApiResources
    | ApiStarSystem
    | ApiSingleStarSystem StarSystemId
    | ApiStar
    | ApiPlanet
    | ApiSinglePlanet PlanetId
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
    | ApiAvailableComponents
    | ApiAvailableChassis
    | ApiAllDesigns
    | ApiSingleDesign DesignId
    | ApiSinglePerson PersonId


endpointToString : Endpoint -> String
endpointToString endpoint =
    case endpoint of
        ApiStarDate ->
            "/api/stardate"

        ApiResources ->
            "/api/resources"

        ApiStarSystem ->
            "/api/starsystem"

        ApiSingleStarSystem systemId ->
            "/api/starsystem/" ++ starSystemIdToString systemId

        ApiStar ->
            "/api/star"

        ApiPlanet ->
            "/api/planet/"

        ApiSinglePlanet planetId ->
            "/api/planet/" ++ planetIdToString planetId

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

        ApiAvailableComponents ->
            "/api/components"

        ApiAvailableChassis ->
            "api/chassis"

        ApiAllDesigns ->
            "api/design"

        ApiSingleDesign designId ->
            "api/design/" ++ designIdToString designId

        ApiSinglePerson personId ->
            "/api/person/" ++ personIdToString personId
