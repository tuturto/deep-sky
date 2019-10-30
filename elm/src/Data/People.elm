module Data.People exposing
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
    , age
    , displayName
    , formalName
    , getCognomen
    , getFamilyName
    , getFirstName
    , getRegnalNumber
    , nameWithTitle
    , personIntelToString
    , personNameOrdering
    , petTypeToString
    , relationTypeOrdering
    , relationTypeToString
    , traitNameOrdering
    , traitOrdering
    , unAge
    , unCognomen
    , unFamilyName
    , unFirstName
    , unLongTitle
    , unOpinionReason
    , unOpinionScore
    , unRegnalNumber
    , unShortTitle
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
        , findFirst
        , perhapsOrdering
        , unStarDate
        )
import Data.Vehicles exposing (CrewPosition(..))
import List exposing (repeat)
import Maybe exposing (map, withDefault)
import Ordering exposing (Ordering)
import String exposing (join)
import Tuple exposing (first, second)


type FirstName
    = FirstName String


unFirstName : FirstName -> String
unFirstName (FirstName name) =
    name


firstNameOrdering : Ordering FirstName
firstNameOrdering =
    Ordering.byField unFirstName


type FamilyName
    = FamilyName String


unFamilyName : FamilyName -> String
unFamilyName (FamilyName name) =
    name


familyNameOrdering : Ordering FamilyName
familyNameOrdering =
    Ordering.byField unFamilyName


type Cognomen
    = Cognomen String


unCognomen : Cognomen -> String
unCognomen (Cognomen name) =
    name


cognomenOrdering : Ordering Cognomen
cognomenOrdering =
    Ordering.byField unCognomen


type RegnalNumber
    = RegnalNumber Int


unRegnalNumber : RegnalNumber -> Int
unRegnalNumber (RegnalNumber n) =
    n


regnalNumberOrdering : Ordering RegnalNumber
regnalNumberOrdering =
    Ordering.byField unRegnalNumber


type PersonName
    = SimpleName FirstName (Maybe Cognomen)
    | RegularName FirstName FamilyName (Maybe Cognomen)
    | RegalName FirstName FamilyName RegnalNumber (Maybe Cognomen)


getFirstName : PersonName -> FirstName
getFirstName name =
    case name of
        SimpleName s _ ->
            s

        RegularName s _ _ ->
            s

        RegalName s _ _ _ ->
            s


getFamilyName : PersonName -> Maybe FamilyName
getFamilyName name =
    case name of
        SimpleName _ _ ->
            Nothing

        RegularName _ s _ ->
            Just s

        RegalName _ s _ _ ->
            Just s


getCognomen : PersonName -> Maybe Cognomen
getCognomen name =
    case name of
        SimpleName _ s ->
            s

        RegularName _ _ s ->
            s

        RegalName _ _ _ s ->
            s


getRegnalNumber : PersonName -> Maybe RegnalNumber
getRegnalNumber name =
    case name of
        SimpleName _ _ ->
            Nothing

        RegularName _ _ _ ->
            Nothing

        RegalName _ _ n _ ->
            Just n


{-| Ordering for Person name
-}
personNameOrdering : Ordering PersonName
personNameOrdering a b =
    case ( a, b ) of
        ( SimpleName _ _, SimpleName _ _ ) ->
            Ordering.byFieldWith firstNameOrdering getFirstName a b
                |> Ordering.ifStillTiedThen (Ordering.byFieldWith (perhapsOrdering cognomenOrdering) getCognomen a b)

        ( SimpleName _ _, RegularName _ _ _ ) ->
            GT

        ( SimpleName _ _, RegalName _ _ _ _ ) ->
            GT

        ( RegularName _ _ _, SimpleName _ _ ) ->
            LT

        ( RegularName _ _ _, RegularName _ _ _ ) ->
            Ordering.byFieldWith (perhapsOrdering familyNameOrdering) getFamilyName a b
                |> Ordering.ifStillTiedThen (Ordering.byFieldWith firstNameOrdering getFirstName a b)
                |> Ordering.ifStillTiedThen (Ordering.byFieldWith (perhapsOrdering cognomenOrdering) getCognomen a b)

        ( RegularName _ _ _, RegalName _ _ _ _ ) ->
            Ordering.byFieldWith (perhapsOrdering familyNameOrdering) getFamilyName a b
                |> Ordering.ifStillTiedThen (Ordering.byFieldWith firstNameOrdering getFirstName a b)
                |> Ordering.ifStillTiedThen (Ordering.byFieldWith (perhapsOrdering cognomenOrdering) getCognomen a b)
                |> Ordering.ifStillTiedThen GT

        ( RegalName _ _ _ _, SimpleName _ _ ) ->
            LT

        ( RegalName _ _ _ _, RegularName _ _ _ ) ->
            Ordering.byFieldWith (perhapsOrdering familyNameOrdering) getFamilyName a b
                |> Ordering.ifStillTiedThen (Ordering.byFieldWith firstNameOrdering getFirstName a b)
                |> Ordering.ifStillTiedThen (Ordering.byFieldWith (perhapsOrdering cognomenOrdering) getCognomen a b)
                |> Ordering.ifStillTiedThen LT

        ( RegalName aFirst aFamily aRegnal aCognomen, RegalName bFirst bFamily bRegnal bCognomen ) ->
            familyNameOrdering aFamily bFamily
                |> Ordering.ifStillTiedThen (firstNameOrdering aFirst bFirst)
                |> Ordering.ifStillTiedThen (regnalNumberOrdering aRegnal bRegnal)
                |> Ordering.ifStillTiedThen (perhapsOrdering cognomenOrdering aCognomen bCognomen)


{-| Name intended to be displayed
-}
displayName : PersonName -> String
displayName name =
    case name of
        SimpleName firstName cognomen ->
            case cognomen of
                Just cog ->
                    unFirstName firstName ++ " \"" ++ unCognomen cog ++ "\""

                Nothing ->
                    unFirstName firstName

        RegularName firstName familyName cognomen ->
            case cognomen of
                Just cog ->
                    unFirstName firstName ++ " \"" ++ unCognomen cog ++ "\" " ++ unFamilyName familyName

                Nothing ->
                    unFirstName firstName ++ " " ++ unFamilyName familyName

        RegalName firstName _ regnalNumber cognomen ->
            case cognomen of
                Just cog ->
                    unFirstName firstName ++ " " ++ displayRegnal regnalNumber ++ " " ++ " \"" ++ unCognomen cog ++ "\""

                Nothing ->
                    unFirstName firstName ++ " " ++ displayRegnal regnalNumber


nameWithTitle : PersonName -> Maybe ShortTitle -> String
nameWithTitle name title =
    case title of
        Nothing ->
            displayName name

        Just t ->
            unShortTitle t ++ " " ++ displayName name


{-| Representation of regnal number in roman numerals
-}
displayRegnal : RegnalNumber -> String
displayRegnal (RegnalNumber regnal) =
    findNumerals "" regnal


{-| Recursively build roman numeral using given string as starting point
-}
findNumerals : String -> Int -> String
findNumerals s n =
    if n <= 0 then
        s

    else
        let
            match =
                findFirst (\x -> n >= second x) numerals
                    |> withDefault ( "I", 1 )
        in
        findNumerals (s ++ first match) (n - second match)


{-| mapping between roman numerals and integers
-}
numerals : List ( String, Int )
numerals =
    [ ( "M", 1000 )
    , ( "CM", 900 )
    , ( "D", 500 )
    , ( "CD", 400 )
    , ( "C", 100 )
    , ( "CX", 90 )
    , ( "L", 50 )
    , ( "XL", 40 )
    , ( "X", 10 )
    , ( "IX", 9 )
    , ( "V", 5 )
    , ( "IV", 4 )
    , ( "I", 1 )
    ]


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


type ShortTitle
    = ShortTitle String


unShortTitle : ShortTitle -> String
unShortTitle (ShortTitle s) =
    s


type LongTitle
    = LongTitle String


unLongTitle : LongTitle -> String
unLongTitle (LongTitle s) =
    s


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
    { personId : PersonId
    , planetId : PlanetId
    , starSystemId : StarSystemId
    , planetName : PlanetName
    }


type alias OnUnitData =
    { personId : PersonId
    , unitId : UnitId
    , position : CrewPosition
    }
