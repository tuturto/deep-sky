module Api.User exposing (factionIdDecoder)

import Data.Common exposing (FactionId(..))
import Json.Decode as Decode
    exposing
        ( int
        , succeed
        )
import Json.Decode.Extra exposing (andMap)


factionIdDecoder : Decode.Decoder FactionId
factionIdDecoder =
    succeed FactionId
        |> andMap int
