module Api.People exposing
    ( getDemesne
    , getPersonDetails
    , personIdDecoder
    , personNameDecoder
    , shortTitleDecoder
    )

import Api.Common
    exposing
        ( get
        , is
        , planetIdDecoder
        , planetNameDecoder
        , starDateDecoder
        , starSystemIdDecoder
        , starSystemNameDecoder
        )
import Api.Endpoints exposing (Endpoint(..))
import Data.Common exposing (DemesneName(..), PersonId(..))
import Data.Model exposing (Msg(..))
import Data.People
    exposing
        ( Age(..)
        , Cognomen(..)
        , DemesneShortInfo(..)
        , FamilyName(..)
        , FirstName(..)
        , Gender(..)
        , LongTitle(..)
        , Person
        , PersonIntel(..)
        , PersonName(..)
        , PlanetDemesneReportShort
        , RegnalNumber(..)
        , RelationLink
        , RelationType(..)
        , Sex(..)
        , ShortTitle(..)
        , StarSystemDemesneReportShort
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
        , list
        , maybe
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Extra exposing (andMap, when)


getPersonDetails : (Result Http.Error Person -> Msg) -> PersonId -> Cmd Msg
getPersonDetails msg pId =
    Http.send msg (get (ApiSinglePerson pId) personDecoder)


getDemesne : (Result Http.Error (List DemesneShortInfo) -> Msg) -> PersonId -> Cmd Msg
getDemesne msg pId =
    Http.send msg (get (ApiDemesne pId) (list demesneReportShortDecoder))


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
        |> andMap (field "Id" personIdDecoder)
        |> andMap (field "Name" personNameDecoder)
        |> andMap (field "ShortTitle" (maybe shortTitleDecoder))
        |> andMap (field "LongTitle" (maybe longTitleDecoder))
        |> andMap (field "Stats" (maybe statsDecoder))
        |> andMap (field "Sex" sexDecoder)
        |> andMap (field "Gender" genderDecoder)
        |> andMap (field "Age" ageDecoder)
        |> andMap (field "Relations" (list relationLinkDecoder))
        |> andMap (field "IntelTypes" (list personIntelDecoder))


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


ageDecoder : Decode.Decoder Age
ageDecoder =
    succeed Age
        |> andMap int


demesneReportShortDecoder : Decode.Decoder DemesneShortInfo
demesneReportShortDecoder =
    oneOf
        [ when demesneType (is "Planet") planetDemesneReportShortDecoder
        , when demesneType (is "StarSystem") starSystemDemesneReportShortDecoder
        ]


{-| Decoder for demesne report type string
-}
demesneType : Decode.Decoder String
demesneType =
    field "Tag" string


planetDemesneReportShortDecoder : Decode.Decoder DemesneShortInfo
planetDemesneReportShortDecoder =
    let
        decoder =
            succeed PlanetDemesneReportShort
                |> andMap (field "PlanetId" planetIdDecoder)
                |> andMap (field "StarSystemId" starSystemIdDecoder)
                |> andMap (field "Name" planetNameDecoder)
                |> andMap (field "FormalName" demesneNameDecoder)
                |> andMap (field "Date" starDateDecoder)
    in
    succeed PlanetDemesneShort
        |> andMap decoder


starSystemDemesneReportShortDecoder : Decode.Decoder DemesneShortInfo
starSystemDemesneReportShortDecoder =
    let
        decoder =
            succeed StarSystemDemesneReportShort
                |> andMap (field "StarSystemId" starSystemIdDecoder)
                |> andMap (field "Name" starSystemNameDecoder)
                |> andMap (field "FormalName" demesneNameDecoder)
                |> andMap (field "Date" starDateDecoder)
    in
    succeed StarSystemDemesneShort
        |> andMap decoder


demesneNameDecoder : Decode.Decoder DemesneName
demesneNameDecoder =
    succeed DemesneName
        |> andMap string


shortTitleDecoder : Decode.Decoder ShortTitle
shortTitleDecoder =
    succeed ShortTitle
        |> andMap string


longTitleDecoder : Decode.Decoder LongTitle
longTitleDecoder =
    succeed LongTitle
        |> andMap string


relationLinkDecoder : Decode.Decoder RelationLink
relationLinkDecoder =
    succeed RelationLink
        |> andMap (field "Id" personIdDecoder)
        |> andMap (field "Name" personNameDecoder)
        |> andMap (field "ShortTitle" (maybe shortTitleDecoder))
        |> andMap (field "LongTitle" (maybe longTitleDecoder))
        |> andMap (field "Types" (list relationTypeDecoder))


relationTypeDecoder : Decode.Decoder RelationType
relationTypeDecoder =
    string |> andThen stringToRelationType


stringToRelationType : String -> Decode.Decoder RelationType
stringToRelationType s =
    case s of
        "Parent" ->
            succeed Parent

        "Child" ->
            succeed Child

        "Sibling" ->
            succeed Sibling

        "StepParent" ->
            succeed StepParent

        "StepChild" ->
            succeed StepChild

        "StepSibling" ->
            succeed StepSibling

        "Spouse" ->
            succeed Spouse

        "ExSpouse" ->
            succeed ExSpouse

        "Lover" ->
            succeed Lover

        "ExLover" ->
            succeed ExLover

        "Friend" ->
            succeed Friend

        "Rival" ->
            succeed Rival

        _ ->
            fail "unknown type"


personIntelDecoder : Decode.Decoder PersonIntel
personIntelDecoder =
    string
        |> andThen stringToPersonIntel


stringToPersonIntel : String -> Decode.Decoder PersonIntel
stringToPersonIntel s =
    case s of
        "Stats" ->
            succeed Stats

        "Demesne" ->
            succeed Demesne

        "FamilyRelations" ->
            succeed FamilyRelations

        "SecretRelations" ->
            succeed SecretRelations

        _ ->
            fail "unknown type"
