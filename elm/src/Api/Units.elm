module Api.Units exposing (getUnitDetails)

import Api.Common
    exposing
        ( get
        , is
        , planetIdDecoder
        , starSystemIdDecoder
        )
import Api.Designer
    exposing
        ( crewRankDecoder
        , designNameDecoder
        , unitStatsDecoder
        )
import Api.Endpoints exposing (Endpoint(..))
import Api.People
    exposing
        ( crewPositionDecoder
        , personIdDecoder
        , personNameDecoder
        )
import Data.Common exposing (UnitId(..))
import Data.Model exposing (Msg(..))
import Data.Vehicles
    exposing
        ( Band(..)
        , CrewReport
        , ShipDetails
        , ShipLocation(..)
        , ShipName(..)
        , Unit(..)
        , UnitLocation(..)
        , VehicleDetails
        , VehicleLocation(..)
        , VehicleName(..)
        )
import Http
import Json.Decode as Decode
    exposing
        ( andThen
        , fail
        , field
        , list
        , maybe
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Extra exposing (andMap, when)


getUnitDetails : (Result Http.Error Unit -> Msg) -> UnitId -> Cmd Msg
getUnitDetails msg uId =
    Http.send msg (get (ApiSingleUnit uId) unitDecoder)


unitDecoder : Decode.Decoder Unit
unitDecoder =
    oneOf
        [ when unitType
            (is "VehicleReport")
            (succeed Vehicle
                |> andMap (field "Contents" vehicleDetailsDecoder)
            )
        , when unitType
            (is "ShipReport")
            (succeed Ship
                |> andMap (field "Contents" shipDetailsDecoder)
            )
        ]


unitType : Decode.Decoder String
unitType =
    field "Tag" string


vehicleDetailsDecoder : Decode.Decoder VehicleDetails
vehicleDetailsDecoder =
    succeed VehicleDetails
        |> andMap (field "Name" vehicleNameDecoder)
        |> andMap (field "DesignName" designNameDecoder)
        |> andMap (field "Stats" unitStatsDecoder)
        |> andMap (field "Location" (maybe vehicleLocationDecoder))
        |> andMap (field "Crew" (list crewReportDecoder))


shipDetailsDecoder : Decode.Decoder ShipDetails
shipDetailsDecoder =
    succeed ShipDetails
        |> andMap (field "Name" shipNameDecoder)
        |> andMap (field "DesignName" designNameDecoder)
        |> andMap (field "Stats" unitStatsDecoder)
        |> andMap (field "Location" (maybe shipLocationDecoder))
        |> andMap (field "Crew" (list crewReportDecoder))


vehicleNameDecoder : Decode.Decoder VehicleName
vehicleNameDecoder =
    Decode.map VehicleName string


shipNameDecoder : Decode.Decoder ShipName
shipNameDecoder =
    Decode.map ShipName string


bandDecoder : Decode.Decoder Band
bandDecoder =
    string
        |> andThen stringToBand


stringToBand : String -> Decode.Decoder Band
stringToBand s =
    case s of
        "BandSurface" ->
            succeed BandSurface

        "BandOrbit" ->
            succeed BandOrbit

        "BandA" ->
            succeed BandA

        "BandB" ->
            succeed BandB

        "BandC" ->
            succeed BandC

        "BandD" ->
            succeed BandD

        "BandE" ->
            succeed BandE

        "BandF" ->
            succeed BandF

        "BandG" ->
            succeed BandG

        _ ->
            fail "failed to deserialize"


vehicleLocationDecoder : Decode.Decoder VehicleLocation
vehicleLocationDecoder =
    oneOf
        [ when locationType
            (is "VehicleOnPlanet")
            (succeed VehicleOnPlanet
                |> andMap (field "PlanetId" planetIdDecoder)
            )
        ]


shipLocationDecoder : Decode.Decoder ShipLocation
shipLocationDecoder =
    oneOf
        [ when locationType
            (is "PlanetarySpace")
            (succeed PlanetarySpace
                |> andMap (field "PlanetId" planetIdDecoder)
                |> andMap (field "Band" bandDecoder)
            )
        , when locationType
            (is "SystemSpace")
            (succeed SystemSpace
                |> andMap (field "SystemId" starSystemIdDecoder)
                |> andMap (field "Band" bandDecoder)
            )
        ]


unitLocationDecoder : Decode.Decoder UnitLocation
unitLocationDecoder =
    oneOf
        [ when locationType
            (is "ShipLocation")
            (succeed ShipLocation
                |> andMap (field "Contents" shipLocationDecoder)
            )
        , when locationType
            (is "VehicleLocation")
            (succeed VehicleLocation
                |> andMap (field "Contents" vehicleLocationDecoder)
            )
        ]


locationType : Decode.Decoder String
locationType =
    field "Tag" string


crewReportDecoder : Decode.Decoder CrewReport
crewReportDecoder =
    succeed CrewReport
        |> andMap (field "PersonId" personIdDecoder)
        |> andMap (field "Name" personNameDecoder)
        |> andMap (field "Position" crewPositionDecoder)
        |> andMap (field "Rank" crewRankDecoder)
