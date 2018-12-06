module Api.StarSystem exposing
    ( buildingIdDecoder
    , buildingLevelDecoder
    , buildingLevelEncoder
    , buildingTypeDecoder
    , buildingTypeEncoder
    , buildingsCmd
    , getBuildingsCmd
    , getPlanetsCmd
    , getPopulationsCmd
    , getStarSystemsCmd
    , getStarsCmd
    , gravityDecoder
    , planetDecoder
    , planetIdDecoder
    , planetIdEncoder
    , planetPositionDecoder
    , planetsCmd
    , starDecoder
    , starSystemIdDecoder
    , starSystemsCmd
    , starsCmd
    )

import Api.Common exposing (get, locationDecoder, starDateDecoder)
import Api.Endpoints exposing (Endpoint(..))
import Api.User exposing (factionIdDecoder)
import Data.Common
    exposing
        ( BuildingId(..)
        , FactionId(..)
        , PlanetId(..)
        , StarId(..)
        , StarSystemId(..)
        )
import Data.Construction
    exposing
        ( Building
        , BuildingDamage(..)
        , BuildingLevel(..)
        , BuildingType(..)
        )
import Data.Model exposing (ApiMsg(..), Model, Msg(..))
import Data.StarSystem
    exposing
        ( Gravity(..)
        , Inhabitants(..)
        , LuminosityClass(..)
        , Planet
        , PlanetPosition(..)
        , Population
        , Race(..)
        , SpectralType(..)
        , Star
        , StarSystem
        )
import Http
import Json.Decode as Decode
    exposing
        ( andThen
        , fail
        , field
        , float
        , int
        , list
        , maybe
        , string
        , succeed
        )
import Json.Decode.Extra exposing (andMap)
import Json.Encode as Encode
import Maybe.Extra exposing (isNothing)


starSystemsCmd : Cmd Msg
starSystemsCmd =
    Http.send (ApiMsgCompleted << StarSystemsReceived) (get ApiStarSystem (list starSystemDecoder))


getStarSystemsCmd : Model -> Cmd Msg
getStarSystemsCmd model =
    if isNothing model.starSystems then
        starSystemsCmd

    else
        Cmd.none


starsCmd : Cmd Msg
starsCmd =
    Http.send (ApiMsgCompleted << StarsReceived) (get ApiStar (list starDecoder))


getStarsCmd : Model -> Cmd Msg
getStarsCmd model =
    if isNothing model.stars then
        starsCmd

    else
        Cmd.none


planetsCmd : Cmd Msg
planetsCmd =
    Http.send (ApiMsgCompleted << PlanetsReceived) (get ApiPlanet (list planetDecoder))


getPlanetsCmd : Model -> Cmd Msg
getPlanetsCmd model =
    if isNothing model.planets then
        planetsCmd

    else
        Cmd.none


getPopulationsCmd : Model -> PlanetId -> Cmd Msg
getPopulationsCmd model planetId =
    populationCmd planetId


populationCmd : PlanetId -> Cmd Msg
populationCmd planetId =
    Http.send (ApiMsgCompleted << PopulationReceived) (get (ApiPopulation planetId) (list populationDecoder))


getBuildingsCmd : Model -> PlanetId -> Cmd Msg
getBuildingsCmd model planetId =
    buildingsCmd planetId


buildingsCmd : PlanetId -> Cmd Msg
buildingsCmd planetId =
    Http.send (ApiMsgCompleted << BuildingsReceived) (get (ApiBuilding planetId) (list buildingDecoder))


starSystemIdDecoder : Decode.Decoder StarSystemId
starSystemIdDecoder =
    succeed StarSystemId
        |> andMap int


starSystemDecoder : Decode.Decoder StarSystem
starSystemDecoder =
    succeed StarSystem
        |> andMap (field "id" starSystemIdDecoder)
        |> andMap (field "name" string)
        |> andMap (field "location" locationDecoder)
        |> andMap (field "date" starDateDecoder)


starDecoder : Decode.Decoder Star
starDecoder =
    succeed Star
        |> andMap (field "id" starIdDecoder)
        |> andMap (field "systemId" starSystemIdDecoder)
        |> andMap (field "name" string)
        |> andMap (field "spectralType" (maybe spectralTypeDecoder))
        |> andMap (field "luminosityClass" (maybe luminosityClassDecoder))
        |> andMap (field "date" starDateDecoder)


starIdDecoder : Decode.Decoder StarId
starIdDecoder =
    succeed StarId
        |> andMap int


spectralTypeDecoder : Decode.Decoder SpectralType
spectralTypeDecoder =
    string |> andThen stringToSpectral


stringToSpectral : String -> Decode.Decoder SpectralType
stringToSpectral s =
    case s of
        "O" ->
            succeed O

        "B" ->
            succeed B

        "A" ->
            succeed A

        "F" ->
            succeed F

        "G" ->
            succeed G

        "K" ->
            succeed K

        "M" ->
            succeed M

        "L" ->
            succeed L

        "T" ->
            succeed T

        _ ->
            fail "unknown type"


luminosityClassDecoder : Decode.Decoder LuminosityClass
luminosityClassDecoder =
    string |> andThen stringToLuminosity


stringToLuminosity : String -> Decode.Decoder LuminosityClass
stringToLuminosity s =
    case s of
        "Iap" ->
            succeed Iap

        "Ia" ->
            succeed Ia

        "Iab" ->
            succeed Iab

        "Ib" ->
            succeed Ib

        "II" ->
            succeed II

        "III" ->
            succeed III

        "IV" ->
            succeed IV

        "V" ->
            succeed V

        "VI" ->
            succeed VI

        "VII" ->
            succeed VII

        _ ->
            fail "unknown type"


planetIdDecoder : Decode.Decoder PlanetId
planetIdDecoder =
    succeed PlanetId
        |> andMap int


planetIdEncoder : PlanetId -> Encode.Value
planetIdEncoder (PlanetId x) =
    Encode.int x


gravityDecoder : Decode.Decoder Gravity
gravityDecoder =
    succeed Gravity
        |> andMap float


planetPositionDecoder : Decode.Decoder PlanetPosition
planetPositionDecoder =
    succeed PlanetPosition
        |> andMap int


planetDecoder : Decode.Decoder Planet
planetDecoder =
    succeed Planet
        |> andMap (field "id" planetIdDecoder)
        |> andMap (field "systemId" starSystemIdDecoder)
        |> andMap (field "name" string)
        |> andMap (field "position" (maybe planetPositionDecoder))
        |> andMap (field "gravity" (maybe gravityDecoder))
        |> andMap (field "ownerId" (maybe factionIdDecoder))
        |> andMap (field "date" starDateDecoder)


populationDecoder : Decode.Decoder Population
populationDecoder =
    succeed Population
        |> andMap (field "planetId" planetIdDecoder)
        |> andMap (field "race" raceDecoder)
        |> andMap (field "inhabitants" inhabitantsDecoder)
        |> andMap (field "date" starDateDecoder)


raceDecoder : Decode.Decoder Race
raceDecoder =
    succeed Race
        |> andMap string


inhabitantsDecoder : Decode.Decoder Inhabitants
inhabitantsDecoder =
    succeed Inhabitants
        |> andMap int


buildingDecoder : Decode.Decoder Building
buildingDecoder =
    succeed Building
        |> andMap (field "id" buildingIdDecoder)
        |> andMap (field "planetId" planetIdDecoder)
        |> andMap (field "type" buildingTypeDecoder)
        |> andMap (field "level" buildingLevelDecoder)
        |> andMap (field "damage" buildingDamageDecoder)
        |> andMap (field "date" starDateDecoder)


buildingIdDecoder : Decode.Decoder BuildingId
buildingIdDecoder =
    succeed BuildingId
        |> andMap int


buildingTypeDecoder : Decode.Decoder BuildingType
buildingTypeDecoder =
    string |> andThen stringToBuildingType


stringToBuildingType : String -> Decode.Decoder BuildingType
stringToBuildingType s =
    case s of
        "SensorStation" ->
            succeed SensorStation

        "ResearchComplex" ->
            succeed ResearchComplex

        "Farm" ->
            succeed Farm

        "ParticleAccelerator" ->
            succeed ParticleAccelerator

        "NeutronDetector" ->
            succeed NeutronDetector

        "BlackMatterScanner" ->
            succeed BlackMatterScanner

        "GravityWaveSensor" ->
            succeed GravityWaveSensor

        _ ->
            fail "unknown building type"


buildingTypeEncoder : BuildingType -> Encode.Value
buildingTypeEncoder buildingType =
    Encode.string <|
        case buildingType of
            SensorStation ->
                "SensorStation"

            ResearchComplex ->
                "ResearchComplex"

            Farm ->
                "Farm"

            ParticleAccelerator ->
                "ParticleAccelerator"

            NeutronDetector ->
                "NeutronDetector"

            BlackMatterScanner ->
                "BlackMatterScanner"

            GravityWaveSensor ->
                "GravityWaveSensor"


buildingLevelDecoder : Decode.Decoder BuildingLevel
buildingLevelDecoder =
    succeed BuildingLevel
        |> andMap int


buildingLevelEncoder : BuildingLevel -> Encode.Value
buildingLevelEncoder (BuildingLevel x) =
    Encode.int x


buildingDamageDecoder : Decode.Decoder BuildingDamage
buildingDamageDecoder =
    succeed BuildingDamage
        |> andMap float
