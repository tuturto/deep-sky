module Data.People exposing
    ( Cognomen(..)
    , FamilyName(..)
    , FirstName(..)
    , PersonName(..)
    , RegnalNumber(..)
    , displayName
    , unCognomen
    , unFamilyName
    , unFirstName
    , unRegnalNumber
    )

import Data.Common exposing (findFirst)
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
