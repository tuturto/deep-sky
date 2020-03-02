module Api.Endpoints exposing (Endpoint(..), endpointToString)

import Data.Common
    exposing
        ( DesignId
        , MessageId
        , PersonId
        , PlanetId
        , StarSystemId
        , UnitId
        , constructionIdToString
        , designIdToString
        , messageIdToString
        , personIdToString
        , planetIdToString
        , starSystemIdToString
        , unStarSystemId
        , unitIdToString
        )
import Data.Construction exposing (Construction(..))
import Maybe
import Maybe.Extra exposing (values)
import Url.Builder exposing (absolute, int)


type Endpoint
    = ApiStarDate
    | ApiResources
    | ApiStarSystem
    | ApiSingleStarSystem StarSystemId
    | ApiStar (Maybe StarSystemId)
    | ApiPlanet (Maybe StarSystemId)
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
    | ApiSingleUnit UnitId
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

        ApiStar systemId ->
            absolute [ "api", "star" ] <|
                values
                    [ Maybe.map (int "systemId" << unStarSystemId) systemId
                    ]

        ApiPlanet systemId ->
            absolute [ "api", "planet" ] <|
                values
                    [ Maybe.map (int "systemId" << unStarSystemId) systemId
                    ]

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

        ApiSingleUnit unitId ->
            absolute [ "api", "unit", unitIdToString unitId ] []

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
