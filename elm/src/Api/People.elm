module Api.People exposing (personIdDecoder, personNameDecoder)

import Api.Common exposing (is)
import Data.Common exposing (PersonId(..))
import Data.People
    exposing
        ( Cognomen(..)
        , FamilyName(..)
        , FirstName(..)
        , PersonName(..)
        , RegnalNumber(..)
        )
import Json.Decode as Decode
    exposing
        ( fail
        , field
        , int
        , maybe
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Extra exposing (andMap, when)


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
