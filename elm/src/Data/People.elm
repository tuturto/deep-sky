module Data.People exposing
    ( Age(..)
    , DemesneShortInfo(..)
    , DynastyLink
    , Gender(..)
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
    , PetType(..)
    , PlanetDemesneReportShort
    , RelationLink
    , RelationType(..)
    , RelationVisibility(..)
    , Sex(..)
    , StarSystemDemesneReportShort
    , StatValue(..)
    , StatValues
    , Trait
    , TraitDescription(..)
    , TraitName(..)
    , TraitType(..)
    , age
    , formalName
    , personIntelToString
    , petTypeToString
    , relationTypeOrdering
    , relationTypeToString
    , traitNameOrdering
    , traitOrdering
    , unAge
    , unOpinionReason
    , unOpinionScore
    , unStatValue
    , unTraitDescription
    , unTraitName
    , unTraitType
    )

import Data.Common
    exposing
        ( DemesneName
        , DynastyId
        , DynastyName
        , PersonId
        , PlanetId
        , PlanetName
        , StarDate
        , StarSystemId
        , StarSystemName
        , UnitId
        , unStarDate
        )
import Data.PersonNames exposing (LongTitle(..), PersonName(..), ShortTitle(..))
import Data.Vehicles exposing (CrewPosition(..), UnitName(..))
import Ordering exposing (Ordering)


type alias Person =
    { id : PersonId
    , avatar : Bool
    , name : PersonName
    , shortTitle : Maybe ShortTitle
    , longTitle : Maybe LongTitle
    , stats : Maybe StatValues
    , sex : Sex
    , gender : Gender
    , age : Age
    , relations : List RelationLink
    , intelTypes : List PersonIntel
    , dynasty : Maybe DynastyLink
    , traits : Maybe (List Trait)
    , avatarOpinion : OpinionReport
    , opinionOfAvatar : OpinionReport
    , location : PersonLocation
    }


type StatValue
    = StatValue Int


unStatValue : StatValue -> Int
unStatValue (StatValue n) =
    n


type alias StatValues =
    { diplomacy : StatValue
    , learning : StatValue
    , martial : StatValue
    , intrique : StatValue
    , stewardship : StatValue
    }


type Sex
    = Male
    | Female
    | Intersex


type Gender
    = Man
    | Woman
    | Agender
    | Nonbinary


type Age
    = Age Int


unAge : Age -> Int
unAge (Age n) =
    n


{-| Age (time difference between two points in time) in full star years
-}
age : StarDate -> StarDate -> Age
age old new =
    let
        diff =
            unStarDate old - unStarDate new

        fullYears =
            diff // 10
    in
    Age fullYears


{-| Short form demesne report, listing only ID and Name
-}
type DemesneShortInfo
    = PlanetDemesneShort PlanetDemesneReportShort
    | StarSystemDemesneShort StarSystemDemesneReportShort


type alias PlanetDemesneReportShort =
    { planetId : PlanetId
    , starSystemId : StarSystemId
    , name : PlanetName
    , formalName : DemesneName
    , date : StarDate
    }


type alias StarSystemDemesneReportShort =
    { starSystemId : StarSystemId
    , name : StarSystemName
    , formalName : DemesneName
    , date : StarDate
    }


formalName : DemesneShortInfo -> DemesneName
formalName info =
    case info of
        PlanetDemesneShort report ->
            report.formalName

        StarSystemDemesneShort report ->
            report.formalName


type alias RelationLink =
    { id : PersonId
    , name : PersonName
    , shortTitle : Maybe ShortTitle
    , longTitle : Maybe LongTitle
    , types : List RelationType
    , opinion : OpinionReport
    }


type RelationType
    = Parent
    | Child
    | Sibling
    | StepParent
    | StepChild
    | StepSibling
    | Spouse
    | ExSpouse
    | Lover
    | ExLover
    | Friend
    | Rival


{-| Arbitrary ordering for relation types
-}
relationTypeOrdering : Ordering RelationType
relationTypeOrdering =
    Ordering.explicit
        [ Parent
        , StepParent
        , Sibling
        , StepSibling
        , Spouse
        , ExSpouse
        , Child
        , StepChild
        , Lover
        , ExLover
        , Friend
        , Rival
        ]


relationTypeToString : RelationType -> String
relationTypeToString r =
    case r of
        Parent ->
            "Parent"

        Child ->
            "Child"

        Sibling ->
            "Sibling"

        StepParent ->
            "Stepparent"

        StepChild ->
            "Stepchild"

        StepSibling ->
            "Stepsibling"

        Spouse ->
            "Spouse"

        ExSpouse ->
            "Ex-spouse"

        Lover ->
            "Lover"

        ExLover ->
            "Ex-lover"

        Friend ->
            "Friend"

        Rival ->
            "Rival"


type PersonIntel
    = Stats
    | Demesne
    | FamilyRelations
    | SecretRelations
    | Opinions OpinionIntel
    | Traits
    | Location
    | Activity


type OpinionIntel
    = BaseOpinionIntel RelationVisibility
    | ReasonsForOpinions RelationVisibility
    | DetailedOpinions RelationVisibility


type RelationVisibility
    = SecretRelation
    | FamilyRelation
    | PublicRelation


personIntelToString : PersonIntel -> String
personIntelToString intel =
    case intel of
        Stats ->
            "Stats"

        Demesne ->
            "Demesne"

        FamilyRelations ->
            "Family relations"

        SecretRelations ->
            "Secret relations"

        Opinions _ ->
            "Opinions"

        Traits ->
            "Traits"

        Location ->
            "Location"

        Activity ->
            "Activity"


type alias DynastyLink =
    { id : DynastyId
    , name : DynastyName
    }


type alias Trait =
    { name : TraitName
    , traitType : TraitType
    , description : TraitDescription
    , validUntil : Maybe StarDate
    }


type TraitName
    = TraitName String


unTraitName : TraitName -> String
unTraitName (TraitName s) =
    s


type TraitType
    = TraitType String


unTraitType : TraitType -> String
unTraitType (TraitType t) =
    t


type TraitDescription
    = TraitDescription String


unTraitDescription : TraitDescription -> String
unTraitDescription (TraitDescription s) =
    s


traitNameOrdering : Ordering TraitName
traitNameOrdering =
    Ordering.byField unTraitName


traitOrdering : Ordering Trait
traitOrdering =
    Ordering.byFieldWith traitNameOrdering .name


type OpinionReport
    = BaseOpinionReport OpinionFeeling
    | OpinionReasonReport OpinionFeeling (List OpinionReason)
    | DetailedOpinionReport OpinionScore (List OpinionReason)


type OpinionFeeling
    = PositiveFeeling
    | NeutralFeeling
    | NegativeFeeling


type OpinionReason
    = OpinionReason String


unOpinionReason : OpinionReason -> String
unOpinionReason (OpinionReason s) =
    s


type OpinionScore
    = OpinionScore Int


unOpinionScore : OpinionScore -> Int
unOpinionScore (OpinionScore n) =
    n


type PetType
    = Cat
    | Rat


petTypeToString : PetType -> String
petTypeToString pType =
    case pType of
        Cat ->
            "cat"

        Rat ->
            "rat"


type PersonLocation
    = OnPlanet OnPlanetData
    | OnUnit OnUnitData
    | UnknownLocation


type alias OnPlanetData =
    { planetId : PlanetId
    , starSystemId : StarSystemId
    , planetName : PlanetName
    }


type alias OnUnitData =
    { unitId : UnitId
    , unitName : UnitName
    , position : Maybe CrewPosition
    }
