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
import Maybe
import Maybe.Extra exposing (or, values)
import Url.Builder exposing (absolute, int)


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
    | ApiDesignEstimate
    | ApiSinglePerson PersonId
    | ApiDemesne PersonId
    | ApiAdminSimulationStatus
    | ApiAdminPeople (Maybe Int) (Maybe Int)
    | ApiAdminPerson PersonId
    | ApiAdminAddPerson


{-| Map type safe Endpoint into String that can be used in HTTP requests
-}
endpointToString : Endpoint -> String
endpointToString endpoint =
    case endpoint of
        ApiStarDate ->
            absolute [ "api", "stardate" ] []

        ApiResources ->
            absolute [ "api", "resources" ] []

        ApiStarSystem ->
            absolute [ "api", "starsystem" ] []

        ApiSingleStarSystem systemId ->
            absolute [ "api", "starsystem", starSystemIdToString systemId ] []

        ApiStar ->
            absolute [ "api", "star" ] []

        ApiPlanet ->
            absolute [ "api", "planet" ] []

        ApiSinglePlanet planetId ->
            absolute [ "api", "planet", planetIdToString planetId ] []

        ApiPopulation planetId ->
            absolute [ "api", "planet", planetIdToString planetId, "population" ] []

        ApiBuilding planetId ->
            absolute [ "api", "planet", planetIdToString planetId, "buildings" ] []

        ApiConstructionQueue planetId ->
            absolute [ "api", "construction", "planet", planetIdToString planetId ] []

        ApiConstruction construction ->
            case construction of
                BuildingConstruction data ->
                    absolute [ "api", "construction", "building", constructionIdToString data.id ] []

                ShipConstruction data ->
                    absolute [ "api", "construction", "ship", constructionIdToString data.id ] []

        ApiBuildingConstruction ->
            absolute [ "api", "construction", "building" ] []

        ApiAvailableBuildings ->
            absolute [ "api", "construction", "buildings" ] []

        ApiMessageList ->
            absolute [ "api", "message" ] []

        ApiSingleMessage messageId ->
            absolute [ "api", "message", messageIdToString messageId ] []

        ApiIcon ->
            absolute [ "api", "icon" ] []

        ApiPlanetStatus planetId ->
            absolute [ "api", "planet", planetIdToString planetId, "status" ] []

        ApiAvailableResearch ->
            absolute [ "api", "research", "available" ] []

        ApiCurrentResearch ->
            absolute [ "api", "research", "current" ] []

        ApiResearchProduction ->
            absolute [ "api", "research", "production" ] []

        ApiAvailableComponents ->
            absolute [ "api", "components" ] []

        ApiAvailableChassis ->
            absolute [ "api", "chassis" ] []

        ApiAllDesigns ->
            absolute [ "api", "design" ] []

        ApiSingleDesign designId ->
            absolute [ "api", "design", designIdToString designId ] []

        ApiDesignEstimate ->
            absolute [ "api", "designestimate" ] []

        ApiSinglePerson personId ->
            absolute [ "api", "person", personIdToString personId ] []

        ApiDemesne personId ->
            absolute [ "api", "person", personIdToString personId, "demesne" ] []

        ApiAdminSimulationStatus ->
            absolute [ "api", "admin", "simulation" ] []

        ApiAdminPeople skip take ->
            absolute [ "api", "admin", "people" ] <|
                values
                    [ Maybe.map (int "skip") skip
                    , Maybe.map (int "take") take
                    ]

        ApiAdminPerson personId ->
            absolute [ "api", "admin", "people", personIdToString personId ] []

        ApiAdminAddPerson ->
            absolute [ "api", "admin", "addPerson" ] []
