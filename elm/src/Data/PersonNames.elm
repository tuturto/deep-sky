module Data.PersonNames exposing
    ( Cognomen(..)
    , FamilyName(..)
    , FirstName(..)
    , LongTitle(..)
    , PersonName(..)
    , RegnalNumber(..)
    , ShortTitle(..)
    , cognomenOrdering
    , displayName
    , displayRegnal
    , familyNameOrdering
    , findNumerals
    , firstNameOrdering
    , getCognomen
    , getFamilyName
    , getFirstName
    , getRegnalNumber
    , nameWithTitle
    , personNameOrdering
    , regnalNumberOrdering
    , unCognomen
    , unFamilyName
    , unFirstName
    , unLongTitle
    , unRegnalNumber
    , unShortTitle
    )

import Data.Common exposing (findFirst, perhapsOrdering)
import Maybe exposing (withDefault)
import Ordering exposing (Ordering)
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
