module Api.User exposing
    ( factionIdDecoder
    , factionIdEncoder
    , userNameDecoder
    )

import Data.Common exposing (FactionId(..))
import Data.User exposing (UserName(..))
import Json.Decode as Decode
    exposing
        ( int
        , string
        , succeed
        )
import Json.Decode.Extra exposing (andMap)
import Json.Encode as Encode


factionIdDecoder : Decode.Decoder FactionId
factionIdDecoder =
    succeed FactionId
        |> andMap int


factionIdEncoder : FactionId -> Encode.Value
factionIdEncoder (FactionId x) =
    Encode.int x


userNameDecoder : Decode.Decoder UserName
userNameDecoder =
    succeed UserName
        |> andMap string
