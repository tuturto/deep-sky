module Api.People exposing
    ( personDetails
    , personIdDecoder
    , personNameDecoder
    )

import Api.Common exposing (get, is)
import Api.Endpoints exposing (Endpoint(..))
import Data.Common exposing (PersonId(..))
import Data.Model exposing (Msg(..))
import Data.People
    exposing
        ( Cognomen(..)
        , FamilyName(..)
        , FirstName(..)
        , Gender(..)
        , Person
        , PersonName(..)
        , RegnalNumber(..)
        , Sex(..)
        , StatValue(..)
        , StatValues
        )
import Http
import Json.Decode as Decode
    exposing
        ( andThen
        , fail
        , field
        , int
        , maybe
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Extra exposing (andMap, when)


personDetails : (Result Http.Error Person -> Msg) -> PersonId -> Cmd Msg
personDetails msg pId =
    Http.send msg (get (ApiSinglePerson pId) personDecoder)


personNameDecoder : Decode.Decoder PersonName
personNameDecoder =
    oneOf
        [ when nameType (is "RegularName") regularNameDecoder
        , when nameType (is "SimpleName") simpleNameDecoder
        , when nameType (is "RegalName") regalNameDecoder
        ]


{-| Decoder for name type string
-}
nameType : Decode.Decoder String
nameType =
    field "Tag" string


regularNameDecoder : Decode.Decoder PersonName
regularNameDecoder =
    succeed RegularName
        |> andMap (field "FirstName" firstNameDecoder)
        |> andMap (field "FamilyName" familyNameDecoder)
        |> andMap (field "Cognomen" (maybe cognomenDecoder))


simpleNameDecoder : Decode.Decoder PersonName
simpleNameDecoder =
    succeed SimpleName
        |> andMap (field "FirstName" firstNameDecoder)
        |> andMap (field "Cognomen" (maybe cognomenDecoder))


regalNameDecoder : Decode.Decoder PersonName
regalNameDecoder =
    succeed RegalName
        |> andMap (field "FirstName" firstNameDecoder)
        |> andMap (field "FamilyName" familyNameDecoder)
        |> andMap (field "RegnalNumber" regnalNumberDecoder)
        |> andMap (field "Cognomen" (maybe cognomenDecoder))


personIdDecoder : Decode.Decoder PersonId
personIdDecoder =
    succeed PersonId
        |> andMap int


firstNameDecoder : Decode.Decoder FirstName
firstNameDecoder =
    succeed FirstName
        |> andMap string


familyNameDecoder : Decode.Decoder FamilyName
familyNameDecoder =
    succeed FamilyName
        |> andMap string


regnalNumberDecoder : Decode.Decoder RegnalNumber
regnalNumberDecoder =
    succeed RegnalNumber
        |> andMap int


cognomenDecoder : Decode.Decoder Cognomen
cognomenDecoder =
    succeed Cognomen
        |> andMap string


personDecoder : Decode.Decoder Person
personDecoder =
    succeed Person
        |> andMap (field "Name" personNameDecoder)
        |> andMap (field "Stats" (maybe statsDecoder))
        |> andMap (field "Sex" sexDecoder)
        |> andMap (field "Gender" genderDecoder)


statDecoder : Decode.Decoder StatValue
statDecoder =
    succeed StatValue
        |> andMap int


statsDecoder : Decode.Decoder StatValues
statsDecoder =
    succeed StatValues
        |> andMap (field "Diplomacy" statDecoder)
        |> andMap (field "Learning" statDecoder)
        |> andMap (field "Martial" statDecoder)
        |> andMap (field "Intrique" statDecoder)
        |> andMap (field "Stewardship" statDecoder)


stringToSex : String -> Decode.Decoder Sex
stringToSex s =
    case s of
        "Male" ->
            succeed Male

        "Female" ->
            succeed Female

        "Intersex" ->
            succeed Intersex

        _ ->
            fail "failed to deserialize"


sexDecoder : Decode.Decoder Sex
sexDecoder =
    string |> andThen stringToSex


stringToGender : String -> Decode.Decoder Gender
stringToGender s =
    case s of
        "Man" ->
            succeed Man

        "Woman" ->
            succeed Woman

        "Agender" ->
            succeed Agender

        "Nonbinary" ->
            succeed Nonbinary

        _ ->
            fail "Failed to deserialize"


genderDecoder : Decode.Decoder Gender
genderDecoder =
    string |> andThen stringToGender
