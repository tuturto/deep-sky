module Data.StarSystem exposing
    ( Gravity(..)
    , Inhabitants(..)
    , LuminosityClass(..)
    , Planet
    , PlanetPosition(..)
    , Population
    , Race(..)
    , SpectralType(..)
    , Star
    , StarSystem
    , gravityToString
    , stellarClassification
    , unInhabitants
    , unPlanetPosition
    , unRace
    )

import Data.Common
    exposing
        ( FactionId
        , Location(..)
        , PlanetId
        , StarDate(..)
        , StarId
        , StarSystemId
        )
import Maybe exposing (andThen, withDefault)


type alias StarSystem =
    { id : StarSystemId
    , name : String
    , location : Location
    , date : StarDate
    }


type SpectralType
    = O
    | B
    | A
    | F
    | G
    | K
    | M
    | L
    | T


spectralTypeToString : SpectralType -> String
spectralTypeToString spectralType =
    case spectralType of
        O ->
            "O"

        B ->
            "B"

        A ->
            "A"

        F ->
            "F"

        G ->
            "G"

        K ->
            "K"

        M ->
            "M"

        L ->
            "L"

        T ->
            "T"


type LuminosityClass
    = Iap
    | Ia
    | Iab
    | Ib
    | II
    | III
    | IV
    | V
    | VI
    | VII


luminosityClassToString : LuminosityClass -> String
luminosityClassToString luminosity =
    case luminosity of
        Iap ->
            "Iap"

        Ia ->
            "Ia"

        Iab ->
            "Iab"

        Ib ->
            "Ib"

        II ->
            "II"

        III ->
            "III"

        IV ->
            "IV"

        V ->
            "V"

        VI ->
            "VI"

        VII ->
            "VII"


type alias Star =
    { id : StarId
    , systemId : StarSystemId
    , name : String
    , spectralType : Maybe SpectralType
    , luminosityClass : Maybe LuminosityClass
    , date : StarDate
    }


stellarClassification : Star -> String
stellarClassification star =
    let
        luminosity =
            star.luminosityClass |> andThen (Just << luminosityClassToString)

        spectral =
            star.spectralType |> andThen (Just << spectralTypeToString)
    in
    withDefault "" spectral ++ withDefault "" luminosity


type alias Planet =
    { id : PlanetId
    , systemId : StarSystemId
    , name : String
    , position : Maybe PlanetPosition
    , gravity : Maybe Gravity
    , ownerId : Maybe FactionId
    , date : StarDate
    }


type PlanetPosition
    = PlanetPosition Int


unPlanetPosition : PlanetPosition -> Int
unPlanetPosition (PlanetPosition x) =
    x


type Gravity
    = Gravity Float


gravityToString : Gravity -> String
gravityToString (Gravity grav) =
    String.fromFloat grav ++ "g"


type alias Population =
    { planetId : PlanetId
    , race : Race
    , inhabitants : Inhabitants
    , date : StarDate
    }


type Race
    = Race String


type Inhabitants
    = Inhabitants Int


unRace : Race -> String
unRace (Race x) =
    x


unInhabitants : Inhabitants -> Int
unInhabitants (Inhabitants x) =
    x
