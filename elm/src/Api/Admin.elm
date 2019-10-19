module Api.Admin exposing (getSimulationStatus, putSimulationStatus)

import Api.Common
    exposing
        ( get
        , put
        , starDateDecoder
        , starDateEncoder
        )
import Api.Endpoints exposing (Endpoint(..))
import Data.Admin exposing (Simulation)
import Data.Common exposing (StarDate(..))
import Data.Model exposing (ApiMsg(..), Model, Msg(..))
import Http
import Json.Decode as Decode exposing (field, succeed)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as Encode


{-| Command for loading current simulation status
-}
getSimulationStatus : (Result Http.Error Simulation -> Msg) -> Cmd Msg
getSimulationStatus msg =
    Http.send msg (get AdminSimulationStatus simulationDecoder)


{-| Update simulation status
-}
putSimulationStatus : (Result Http.Error Simulation -> Msg) -> Simulation -> Cmd Msg
putSimulationStatus msg simulation =
    Http.send msg (put AdminSimulationStatus (simulationEncoder simulation) simulationDecoder)


{-| Decoder for Simulation
-}
simulationDecoder : Decode.Decoder Simulation
simulationDecoder =
    succeed Simulation
        |> andMap (field "currentTime" starDateDecoder)


{-| Encoder for Simulation
-}
simulationEncoder : Simulation -> Encode.Value
simulationEncoder simulation =
    Encode.object
        [ ( "currentTime", starDateEncoder simulation.time ) ]
