module Api.Common exposing
    ( delete
    , dynastyIdDecoder
    , dynastyIdEncoder
    , dynastyNameDecoder
    , encodeMaybe
    , get
    , getResourcesCmd
    , getStarDateCmd
    , is
    , locationDecoder
    , planetIdDecoder
    , planetIdEncoder
    , planetNameDecoder
    , planetNameEncoder
    , post
    , put
    , resourceTypeDecoder
    , resourceTypeEncoder
    , resourcesCmd
    , resourcesDecoder
    , resourcesEncoder
    , starDateCmd
    , starDateDecoder
    , starDateEncoder
    , starNameDecoder
    , starNameEncoder
    , starSystemIdDecoder
    , starSystemIdEncoder
    , starSystemNameDecoder
    , starSystemNameEncoder
    , unitIdDecoder
    )

{-| Basic building blocs for accessing server API
-}

import Api.Endpoints exposing (Endpoint(..), endpointToString)
import Data.Common
    exposing
        ( BioResource(..)
        , ChemResource(..)
        , DynastyId(..)
        , DynastyName(..)
        , Location(..)
        , MechResource(..)
        , PlanetId(..)
        , PlanetName(..)
        , ResourceType(..)
        , Resources
        , StarDate(..)
        , StarName(..)
        , StarSystemId(..)
        , StarSystemName(..)
        , UnitId(..)
        )
import Data.Model exposing (ApiMsg(..), Model, Msg(..))
import Http
import Json.Decode as Decode
    exposing
        ( andThen
        , fail
        , field
        , float
        , index
        , int
        , map2
        , string
        , succeed
        )
import Json.Decode.Extra exposing (andMap)
import Json.Encode as Encode
import Maybe.Extra exposing (isNothing)


{-| Send HTTP GET to specific endpoint and parse response with given decoder
-}
get : Endpoint -> Decode.Decoder a -> Http.Request a
get endpoint decoder =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Cache-Control" "no-cache" ]
        , url = endpointToString endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = True
        }


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
delete : Endpoint -> Maybe Encode.Value -> Decode.Decoder a -> Http.Request a
delete endpoint body decoder =
    case body of
        Nothing ->
            send "DELETE" (endpointToString endpoint) Http.emptyBody decoder

        Just msg ->
            send "DELETE" (endpointToString endpoint) (Http.jsonBody msg) decoder


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
        |> andMap (field "CurrentTime" int)


{-| Decode StarDate
-}
starDateDecoder : Decode.Decoder StarDate
starDateDecoder =
    succeed StarDate
        |> andMap int


{-| Encode StarDate
-}
starDateEncoder : StarDate -> Encode.Value
starDateEncoder (StarDate x) =
    Encode.int x


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


resourceTypeDecoder : Decode.Decoder ResourceType
resourceTypeDecoder =
    string |> andThen stringToResourceType


stringToResourceType : String -> Decode.Decoder ResourceType
stringToResourceType s =
    case s of
        "BiologicalResource" ->
            succeed BiologicalResource

        "MechanicalResource" ->
            succeed MechanicalResource

        "ChemicalResource" ->
            succeed ChemicalResource

        _ ->
            fail "unknown type"


resourceTypeEncoder : ResourceType -> Encode.Value
resourceTypeEncoder item =
    Encode.string <|
        case item of
            BiologicalResource ->
                "BiologicalResource"

            MechanicalResource ->
                "MechanicalResource"

            ChemicalResource ->
                "ChemicalResource"


{-| Helper function to write fluent looking decoders
-}
is : String -> String -> Bool
is a b =
    a == b


encodeMaybe : (b -> Encode.Value) -> Maybe b -> Encode.Value
encodeMaybe f val =
    case val of
        Just x ->
            f x

        Nothing ->
            Encode.null


starNameDecoder : Decode.Decoder StarName
starNameDecoder =
    succeed StarName
        |> andMap string


starNameEncoder : StarName -> Encode.Value
starNameEncoder (StarName s) =
    Encode.string s


starSystemNameDecoder : Decode.Decoder StarSystemName
starSystemNameDecoder =
    succeed StarSystemName
        |> andMap string


starSystemNameEncoder : StarSystemName -> Encode.Value
starSystemNameEncoder (StarSystemName s) =
    Encode.string s


planetNameDecoder : Decode.Decoder PlanetName
planetNameDecoder =
    succeed PlanetName
        |> andMap string


planetNameEncoder : PlanetName -> Encode.Value
planetNameEncoder (PlanetName s) =
    Encode.string s


planetIdDecoder : Decode.Decoder PlanetId
planetIdDecoder =
    succeed PlanetId
        |> andMap int


planetIdEncoder : PlanetId -> Encode.Value
planetIdEncoder (PlanetId x) =
    Encode.int x


starSystemIdDecoder : Decode.Decoder StarSystemId
starSystemIdDecoder =
    succeed StarSystemId
        |> andMap int


starSystemIdEncoder : StarSystemId -> Encode.Value
starSystemIdEncoder (StarSystemId x) =
    Encode.int x


dynastyIdDecoder : Decode.Decoder DynastyId
dynastyIdDecoder =
    succeed DynastyId
        |> andMap int


dynastyIdEncoder : DynastyId -> Encode.Value
dynastyIdEncoder (DynastyId n) =
    Encode.int n


dynastyNameDecoder : Decode.Decoder DynastyName
dynastyNameDecoder =
    succeed DynastyName
        |> andMap string


unitIdDecoder : Decode.Decoder UnitId
unitIdDecoder =
    succeed UnitId
        |> andMap int
