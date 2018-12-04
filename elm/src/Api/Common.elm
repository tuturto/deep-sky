module Api.Common exposing
    ( delete
    , get
    , getResourcesCmd
    , getStarDateCmd
    , locationDecoder
    , post
    , put
    , resourcesCmd
    , resourcesDecoder
    , resourcesEncoder
    , starDateCmd
    , starDateDecoder
    )

{-| Basic building blocs for accessing server API
-}

import Api.Endpoints exposing (Endpoint(..), endpointToString)
import Data.Common
    exposing
        ( BioResource(..)
        , ChemResource(..)
        , Location(..)
        , MechResource(..)
        , Resources
        , StarDate(..)
        )
import Data.Model exposing (ApiMsg(..), Model, Msg(..))
import Http
import Json.Decode as Decode exposing (field, float, index, int, map2, succeed)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as Encode
import Maybe.Extra exposing (isNothing)


{-| Send HTTP GET to specific endpoint and parse response with given decoder
-}
get : Endpoint -> Decode.Decoder a -> Http.Request a
get endpoint decoder =
    Http.get (endpointToString endpoint) decoder


{-| Send HTTP POST to specific end point. Body of the message is json created
by the supplied encoder. Response will be parsed with the given decoder.
-}
post : Endpoint -> Encode.Value -> Decode.Decoder a -> Http.Request a
post endpoint body decoder =
    send "POST" (endpointToString endpoint) (Http.jsonBody body) decoder


{-| Send HTTP PUT to specific end point. Body of the message is json created
by the supplied encoder. Response will be parsed with the given decoder.
-}
put : Endpoint -> Encode.Value -> Decode.Decoder a -> Http.Request a
put endpoint body decoder =
    send "PUT" (endpointToString endpoint) (Http.jsonBody body) decoder


{-| Send HTTP DELETE to specific end point. Response will be parsed with the
given decoder.
-}
delete : Endpoint -> Decode.Decoder a -> Http.Request a
delete endpoint decoder =
    send "DELETE" (endpointToString endpoint) Http.emptyBody decoder


{-| Perform HTTP method call. Prefer @get, @post, @put and @delete over this one
-}
send : String -> String -> Http.Body -> Decode.Decoder a -> Http.Request a
send method url body decoder =
    Http.request
        { method = method
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


{-| Command for retrieving current star date from server
-}
starDateCmd : Cmd Msg
starDateCmd =
    Http.send (ApiMsgCompleted << StarDateReceived) (get ApiStarDate currentTimeDecoder)


{-| Check if model already has star date. In case it's not present, create a command to
retrieve it.
-}
getStarDateCmd : Model -> Cmd Msg
getStarDateCmd model =
    if isNothing model.currentTime then
        starDateCmd

    else
        Cmd.none


{-| Command to retrieve raw resources at the player's disposal
-}
resourcesCmd : Cmd Msg
resourcesCmd =
    Http.send (ApiMsgCompleted << ResourcesReceived) (get ApiResources resourcesDecoder)


{-| If model does not contain raw resources information, create a command to retrieve them
from the server.
-}
getResourcesCmd : Model -> Cmd Msg
getResourcesCmd model =
    if isNothing model.resources then
        resourcesCmd

    else
        Cmd.none


{-| Decode current time response into @StarDate
-}
currentTimeDecoder : Decode.Decoder StarDate
currentTimeDecoder =
    succeed StarDate
        |> andMap (field "currentTime" int)


{-| Decode StarDate
-}
starDateDecoder : Decode.Decoder StarDate
starDateDecoder =
    succeed StarDate
        |> andMap int


bioResourceDecoder : Decode.Decoder BioResource
bioResourceDecoder =
    succeed BioResource
        |> andMap int


bioResourceEncoder : BioResource -> Encode.Value
bioResourceEncoder (BioResource x) =
    Encode.int x


mechResourceDecoder : Decode.Decoder MechResource
mechResourceDecoder =
    succeed MechResource
        |> andMap int


mechResourceEncoder : MechResource -> Encode.Value
mechResourceEncoder (MechResource x) =
    Encode.int x


chemResourceDecoder : Decode.Decoder ChemResource
chemResourceDecoder =
    succeed ChemResource
        |> andMap int


chemResourceEncoder : ChemResource -> Encode.Value
chemResourceEncoder (ChemResource x) =
    Encode.int x


resourcesDecoder : Decode.Decoder Resources
resourcesDecoder =
    succeed Resources
        |> andMap (field "biological" bioResourceDecoder)
        |> andMap (field "mechanical" mechResourceDecoder)
        |> andMap (field "chemical" chemResourceDecoder)


resourcesEncoder : Resources -> Encode.Value
resourcesEncoder item =
    Encode.object
        [ ( "biological", bioResourceEncoder item.biological )
        , ( "mechanical", mechResourceEncoder item.mechanical )
        , ( "chemical", chemResourceEncoder item.chemical )
        ]


locationDecoder : Decode.Decoder Location
locationDecoder =
    map2 Location (index 0 float) (index 1 float)