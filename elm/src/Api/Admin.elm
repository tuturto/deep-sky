module Api.Admin exposing
    ( addPerson
    , getPeople
    , getPerson
    , getSimulationStatus
    , putPerson
    , putSimulationStatus
    )

import Api.Common
    exposing
        ( dynastyIdDecoder
        , dynastyIdEncoder
        , encodeMaybe
        , get
        , planetIdDecoder
        , planetIdEncoder
        , post
        , put
        , starDateDecoder
        , starDateEncoder
        , starSystemIdDecoder
        , starSystemIdEncoder
        )
import Api.Endpoints exposing (Endpoint(..))
import Api.People
    exposing
        ( ageEncoder
        , genderDecoder
        , genderEncoder
        , personIdDecoder
        , personIdEncoder
        , personNameDecoder
        , personNameEncoder
        , sexDecoder
        , sexEncoder
        , statDecoder
        , statEncoder
        )
import Api.User exposing (factionIdDecoder, factionIdEncoder)
import Data.Admin
    exposing
        ( AgeOptions(..)
        , Person
        , PersonOptions
        , Simulation
        , SystemStatus(..)
        )
import Data.Common exposing (PagedResult, PersonId)
import Data.Model exposing (ApiMsg(..), Msg(..))
import Http
import Json.Decode as Decode
    exposing
        ( andThen
        , fail
        , field
        , int
        , list
        , maybe
        , string
        , succeed
        )
import Json.Decode.Extra exposing (andMap)
import Json.Encode as Encode
import RemoteData exposing (WebData)


{-| Command for loading current simulation status
-}
getSimulationStatus : (Result Http.Error Simulation -> Msg) -> Cmd Msg
getSimulationStatus msg =
    Http.send msg (get ApiAdminSimulationStatus simulationDecoder)


{-| Update simulation status
-}
putSimulationStatus : (Result Http.Error Simulation -> Msg) -> Simulation -> Cmd Msg
putSimulationStatus msg simulation =
    Http.send msg (put ApiAdminSimulationStatus (simulationEncoder simulation) simulationDecoder)


{-| Get list of people
Results are paged, first parameter indicates amount of records to skip, second parameter amount
of records to take. In case Nothing is supplied, server default is used.
-}
getPeople : (WebData (PagedResult Person) -> Msg) -> Maybe Int -> Maybe Int -> Cmd Msg
getPeople msg skip take =
    Http.send (RemoteData.fromResult >> msg) (get (ApiAdminPeople skip take) (pagedDecoder personDecoder))


{-| Get details of single person
-}
getPerson : (Result Http.Error Person -> Msg) -> PersonId -> Cmd Msg
getPerson msg personId =
    Http.send msg (get (ApiAdminPerson personId) personDecoder)


putPerson : (Result Http.Error Person -> Msg) -> PersonId -> Person -> Cmd Msg
putPerson msg pId person =
    Http.send msg (put (ApiAdminPerson pId) (personEncoder person) personDecoder)


{-| Request creation of new person
-}
addPerson : (Result Http.Error Person -> Msg) -> PersonOptions -> Cmd Msg
addPerson msg opt =
    Http.send msg (post ApiAdminAddPerson (personOptionsEncoder opt) personDecoder)


{-| Decoder for Simulation
-}
simulationDecoder : Decode.Decoder Simulation
simulationDecoder =
    succeed Simulation
        |> andMap (field "currentTime" starDateDecoder)
        |> andMap (field "status" systemStatusDecoder)


{-| Encoder for Simulation
-}
simulationEncoder : Simulation -> Encode.Value
simulationEncoder simulation =
    Encode.object
        [ ( "currentTime", starDateEncoder simulation.time )
        , ( "status", systemStatusEncoder simulation.status )
        ]


{-| Decoder for system status
-}
systemStatusDecoder : Decode.Decoder SystemStatus
systemStatusDecoder =
    string
        |> andThen stringToSystemStatus


stringToSystemStatus : String -> Decode.Decoder SystemStatus
stringToSystemStatus s =
    case s of
        "Offline" ->
            succeed Offline

        "Maintenance" ->
            succeed Maintenance

        "Online" ->
            succeed Online

        "ProcessingTurn" ->
            succeed ProcessingTurn

        _ ->
            fail "unknown value"


{-| Encoder for system status
-}
systemStatusEncoder : SystemStatus -> Encode.Value
systemStatusEncoder status =
    Encode.string <|
        case status of
            Offline ->
                "Offline"

            Maintenance ->
                "Maintenance"

            Online ->
                "Online"

            ProcessingTurn ->
                "ProcessingTurn"


pagedDecoder : Decode.Decoder a -> Decode.Decoder (PagedResult a)
pagedDecoder d =
    succeed PagedResult
        |> andMap (field "Skip" int)
        |> andMap (field "Take" int)
        |> andMap (field "Page" int)
        |> andMap (field "Contents" (list d))


personDecoder : Decode.Decoder Person
personDecoder =
    succeed Person
        |> andMap (field "id" personIdDecoder)
        |> andMap (field "name" personNameDecoder)
        |> andMap (field "sex" sexDecoder)
        |> andMap (field "gender" genderDecoder)
        |> andMap (field "dateOfBirth" starDateDecoder)
        |> andMap (field "diplomacy" statDecoder)
        |> andMap (field "learning" statDecoder)
        |> andMap (field "martial" statDecoder)
        |> andMap (field "intrique" statDecoder)
        |> andMap (field "stewardship" statDecoder)
        |> andMap (field "factionId" (maybe factionIdDecoder))
        |> andMap (field "planetTitle" (maybe planetIdDecoder))
        |> andMap (field "starSystemTitle" (maybe starSystemIdDecoder))
        |> andMap (field "dynastyId" (maybe dynastyIdDecoder))


personEncoder : Person -> Encode.Value
personEncoder person =
    Encode.object
        [ ( "id", personIdEncoder person.id )
        , ( "name", personNameEncoder person.name )
        , ( "sex", sexEncoder person.sex )
        , ( "gender", genderEncoder person.gender )
        , ( "dateOfBirth", starDateEncoder person.dateOfBirth )
        , ( "diplomacy", statEncoder person.diplomacy )
        , ( "learning", statEncoder person.learning )
        , ( "martial", statEncoder person.martial )
        , ( "intrique", statEncoder person.intrique )
        , ( "stewardship", statEncoder person.stewardship )
        , ( "factionId", encodeMaybe factionIdEncoder person.factionId )
        , ( "planetTitle", encodeMaybe planetIdEncoder person.planetTitle )
        , ( "starSystemTitle", encodeMaybe starSystemIdEncoder person.starSystemTitle )
        , ( "dynastyId", encodeMaybe dynastyIdEncoder person.dynastyId )
        ]


personOptionsEncoder : PersonOptions -> Encode.Value
personOptionsEncoder opt =
    Encode.object
        [ ( "Age", encodeMaybe ageOptionsEncoder opt.age ) ]


ageOptionsEncoder : AgeOptions -> Encode.Value
ageOptionsEncoder opt =
    case opt of
        AgeBracket start end ->
            Encode.object
                [ ( "tag", Encode.string "AgeBracket" )
                , ( "contents", Encode.list ageEncoder [ start, end ] )
                ]

        ExactAge age ->
            Encode.object
                [ ( "tag", Encode.string "ExactAge" )
                , ( "contents", ageEncoder age )
                ]
