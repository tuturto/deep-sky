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
    , Sex(..)
    , ShortTitle(..)
    , StarSystemDemesneReportShort
    , StatValue(..)
    , StatValues
    , displayName
    , formalName
    , nameWithTitle
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
        )
import List exposing (repeat)
import Maybe exposing (map, withDefault)
import String exposing (join)
import Tuple exposing (first, second)


type FirstName
    = FirstName String


unFirstName : FirstName -> String
unFirstName (FirstName name) =
    name


type FamilyName
    = FamilyName String


unFamilyName : FamilyName -> String
unFamilyName (FamilyName name) =
    name


type Cognomen
    = Cognomen String


unCognomen : Cognomen -> String
unCognomen (Cognomen name) =
    name


type RegnalNumber
    = RegnalNumber Int


unRegnalNumber : RegnalNumber -> Int
unRegnalNumber (RegnalNumber n) =
    n


type PersonName
    = SimpleName FirstName (Maybe Cognomen)
    | RegularName FirstName FamilyName (Maybe Cognomen)
    | RegalName FirstName FamilyName RegnalNumber (Maybe Cognomen)


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
