module Api.People exposing
    ( getDemesne
    , getPersonDetails
    , personIdDecoder
    , personIdEncoder
    , personNameDecoder
    , petIdDecoder
    , petIdEncoder
    , petTypeDecoder
    , petTypeEncoder
    , shortTitleDecoder
    )

import Api.Common
    exposing
        ( dynastyIdDecoder
        , dynastyNameDecoder
        , get
        , is
        , planetIdDecoder
        , planetNameDecoder
        , starDateDecoder
        , starSystemIdDecoder
        , starSystemNameDecoder
        , unitIdDecoder
        )
import Api.Endpoints exposing (Endpoint(..))
import Data.Common
    exposing
        ( DemesneName(..)
        , PersonId(..)
        , PetId(..)
        )
import Data.Model exposing (Msg(..))
import Data.People
    exposing
        ( Age(..)
        , Cognomen(..)
        , DemesneShortInfo(..)
        , DynastyLink
        , FamilyName(..)
        , FirstName(..)
        , Gender(..)
        , LongTitle(..)
        , OnPlanetData
        , OnUnitData
        , OpinionFeeling(..)
        , OpinionIntel(..)
        , OpinionReason(..)
        , OpinionReport(..)
        , OpinionScore(..)
        , Person
        , PersonIntel(..)
        , PersonLocation(..)
        , PersonName(..)
        , PetType(..)
        , PlanetDemesneReportShort
        , RegnalNumber(..)
        , RelationLink
        , RelationType(..)
        , RelationVisibility(..)
        , Sex(..)
        , ShortTitle(..)
        , StarSystemDemesneReportShort
        , StatValue(..)
        , StatValues
        , Trait
        , TraitDescription(..)
        , TraitName(..)
        , TraitType(..)
        )
import Data.Vehicles exposing (CrewPosition(..))
import Http
import Json.Decode as Decode
    exposing
        ( andThen
        , bool
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
import Json.Encode as Encode


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


personIdEncoder : PersonId -> Encode.Value
personIdEncoder (PersonId n) =
    Encode.int n


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
        |> andMap (field "Avatar" bool)
        |> andMap (field "Name" personNameDecoder)
        |> andMap (field "ShortTitle" (maybe shortTitleDecoder))
        |> andMap (field "LongTitle" (maybe longTitleDecoder))
        |> andMap (field "Stats" (maybe statsDecoder))
        |> andMap (field "Sex" sexDecoder)
        |> andMap (field "Gender" genderDecoder)
        |> andMap (field "Age" ageDecoder)
        |> andMap (field "Relations" (list relationLinkDecoder))
        |> andMap (field "IntelTypes" (list personIntelDecoder))
        |> andMap (field "Dynasty" (maybe dynastyLinkDecoder))
        |> andMap (field "Traits" (maybe (list traitDecoder)))
        |> andMap (field "AvatarOpinion" opinionReportDecoder)
        |> andMap (field "OpinionOfAvatar" opinionReportDecoder)
        |> andMap (field "Location" personLocationDecoder)


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
        |> andMap (field "Opinion" opinionReportDecoder)


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
    oneOf
        [ when tagType (is "Stats") (succeed Stats)
        , when tagType (is "Demesne") (succeed Demesne)
        , when tagType (is "FamilyRelations") (succeed FamilyRelations)
        , when tagType (is "SecretRelations") (succeed SecretRelations)
        , when tagType (is "Traits") (succeed Traits)
        , when tagType
            (is "Opinions")
            (succeed Opinions
                |> andMap (field "contents" opinionIntelDecoder)
            )
        , when tagType (is "Location") (succeed Location)
        , when tagType (is "Activity") (succeed Activity)
        ]


tagType : Decode.Decoder String
tagType =
    field "tag" string


dynastyLinkDecoder : Decode.Decoder DynastyLink
dynastyLinkDecoder =
    succeed DynastyLink
        |> andMap (field "Id" dynastyIdDecoder)
        |> andMap (field "Name" dynastyNameDecoder)


opinionIntelDecoder : Decode.Decoder OpinionIntel
opinionIntelDecoder =
    oneOf
        [ when tagType
            (is "BaseOpinionIntel")
            (succeed BaseOpinionIntel
                |> andMap (field "contents" relationVisibilityDecoder)
            )
        , when tagType
            (is "ReasonsForOpinions")
            (succeed ReasonsForOpinions
                |> andMap (field "contents" relationVisibilityDecoder)
            )
        , when tagType
            (is "DetailedOpinions")
            (succeed DetailedOpinions
                |> andMap (field "contents" relationVisibilityDecoder)
            )
        ]


relationVisibilityDecoder : Decode.Decoder RelationVisibility
relationVisibilityDecoder =
    string |> andThen stringToRelationVisibility


stringToRelationVisibility : String -> Decode.Decoder RelationVisibility
stringToRelationVisibility s =
    case s of
        "PublicRelation" ->
            succeed PublicRelation

        "FamilyRelation" ->
            succeed FamilyRelation

        "SecretRelation" ->
            succeed SecretRelation

        _ ->
            fail "failed to decode"


traitDecoder : Decode.Decoder Trait
traitDecoder =
    succeed Trait
        |> andMap (field "Name" traitNameDecoder)
        |> andMap (field "Type" traitTypeDecoder)
        |> andMap (field "Description" traitDescriptionDecoder)
        |> andMap (field "ValidUntil" (maybe starDateDecoder))


traitNameDecoder : Decode.Decoder TraitName
traitNameDecoder =
    succeed TraitName
        |> andMap string


traitTypeDecoder : Decode.Decoder TraitType
traitTypeDecoder =
    succeed TraitType
        |> andMap string


traitDescriptionDecoder : Decode.Decoder TraitDescription
traitDescriptionDecoder =
    succeed TraitDescription
        |> andMap string


opinionFeelingDecoder : Decode.Decoder OpinionFeeling
opinionFeelingDecoder =
    string |> andThen stringToOpinionFeeling


stringToOpinionFeeling : String -> Decode.Decoder OpinionFeeling
stringToOpinionFeeling s =
    case s of
        "PositiveFeeling" ->
            succeed PositiveFeeling

        "NeutralFeeling" ->
            succeed NeutralFeeling

        "NegativeFeeling" ->
            succeed NegativeFeeling

        _ ->
            fail "failed to deserialize"


opinionReasonDecoder : Decode.Decoder OpinionReason
opinionReasonDecoder =
    succeed OpinionReason
        |> andMap string


opinionScoreDecoder : Decode.Decoder OpinionScore
opinionScoreDecoder =
    succeed OpinionScore
        |> andMap int


opinionReportDecoder : Decode.Decoder OpinionReport
opinionReportDecoder =
    oneOf
        [ when opinionReportType
            (is "BaseOpinionReport")
            (succeed BaseOpinionReport
                |> andMap (field "Feeling" opinionFeelingDecoder)
            )
        , when opinionReportType
            (is "OpinionReasonReport")
            (succeed OpinionReasonReport
                |> andMap (field "Feeling" opinionFeelingDecoder)
                |> andMap (field "Reasons" (list opinionReasonDecoder))
            )
        , when opinionReportType
            (is "DetailedOpinionReport")
            (succeed DetailedOpinionReport
                |> andMap (field "Score" opinionScoreDecoder)
                |> andMap (field "Reasons" (list opinionReasonDecoder))
            )
        ]


opinionReportType : Decode.Decoder String
opinionReportType =
    field "Tag" string


petIdDecoder : Decode.Decoder PetId
petIdDecoder =
    succeed PetId
        |> andMap int


petIdEncoder : PetId -> Encode.Value
petIdEncoder (PetId n) =
    Encode.int n


petTypeDecoder : Decode.Decoder PetType
petTypeDecoder =
    string |> andThen stringToPetType


stringToPetType : String -> Decode.Decoder PetType
stringToPetType s =
    case s of
        "Cat" ->
            succeed Cat

        "Rat" ->
            succeed Rat

        _ ->
            fail "failed to deserialize"


petTypeEncoder : PetType -> Encode.Value
petTypeEncoder t =
    case t of
        Cat ->
            Encode.string "Cat"

        Rat ->
            Encode.string "Rat"


personLocationDecoder : Decode.Decoder PersonLocation
personLocationDecoder =
    oneOf
        [ when opinionReportType
            (is "OnPlanet")
            (succeed OnPlanet
                |> andMap (field "Contents" onPlanetDecoder)
            )
        , when opinionReportType
            (is "OnUnit")
            (succeed OnUnit
                |> andMap (field "Contents" onUnitDecoder)
            )
        , when opinionReportType
            (is "UnknownLocation")
            (succeed UnknownLocation)
        ]


locationTagDecoder : Decode.Decoder String
locationTagDecoder =
    field "Tag" string


onPlanetDecoder : Decode.Decoder OnPlanetData
onPlanetDecoder =
    succeed OnPlanetData
        |> andMap (field "PersonId" personIdDecoder)
        |> andMap (field "PlanetId" planetIdDecoder)
        |> andMap (field "StarSystemId" starSystemIdDecoder)
        |> andMap (field "PlanetName" planetNameDecoder)


onUnitDecoder : Decode.Decoder OnUnitData
onUnitDecoder =
    succeed OnUnitData
        |> andMap (field "PersonId" personIdDecoder)
        |> andMap (field "UnitId" unitIdDecoder)
        |> andMap (field "CrewPosition" crewPositionDecoder)


crewPositionDecoder : Decode.Decoder CrewPosition
crewPositionDecoder =
    fail "ups, oho"
