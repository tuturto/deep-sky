module Api.User exposing (factionIdDecoder, userNameDecoder)

import Data.Common exposing (FactionId(..))
import Data.User exposing (UserName(..))
import Json.Decode as Decode
    exposing
        ( int
        , string
        , succeed
        )
import Json.Decode.Extra exposing (andMap)


factionIdDecoder : Decode.Decoder FactionId
factionIdDecoder =
    succeed FactionId
        |> andMap int


userNameDecoder : Decode.Decoder UserName
userNameDecoder =
    succeed UserName
        |> andMap string
