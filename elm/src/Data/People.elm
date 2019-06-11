module Data.People exposing
    ( Age(..)
    , Cognomen(..)
    , DemesneShortInfo(..)
    , FamilyName(..)
    , FirstName(..)
    , Gender(..)
    , LongTitle(..)
    , Person
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
    , displayName
    , formalName
    , nameWithTitle
    , personNameOrdering
    , relationTypeOrdering
    , relationTypeToString
    , unAge
    , unCognomen
    , unFamilyName
    , unFirstName
    , unLongTitle
    , unRegnalNumber
    , unShortTitle
    , unStatValue
    )

import Data.Common
    exposing
        ( DemesneName
        , PersonId
        , PlanetId
        , PlanetName
        , StarDate
        , StarSystemId
        , StarSystemName
        , findFirst
        , perhapsOrdering
        )
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
                    unFirstName firstName ++ " " ++ displayRegnal regnalNumber ++ " " ++ " \"" ++ unCognomen cog

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
    , name : PersonName
    , shortTitle : Maybe ShortTitle
    , longTitle : Maybe LongTitle
    , stats : Maybe StatValues
    , sex : Sex
    , gender : Gender
    , age : Age
    , relations : List RelationLink
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
