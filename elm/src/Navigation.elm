module Navigation exposing (parseLocation, routes)

{-| Navigation related functions and data types
-}

import Data.Common
    exposing
        ( PersonId(..)
        , PlanetId(..)
        , Route(..)
        , StarSystemId(..)
        , UnitId(..)
        )
import Url exposing (Url)
import Url.Parser
    exposing
        ( (</>)
        , Parser
        , custom
        , map
        , oneOf
        , parse
        , s
        , top
        )


starSystemId : Parser (StarSystemId -> a) a
starSystemId =
    custom "STARSYSTEM_ID" <|
        stringToStarSystemId


planetId : Parser (PlanetId -> a) a
planetId =
    custom "PLANET_ID" <|
        stringToPlanetId


personId : Parser (PersonId -> a) a
personId =
    custom "PERSON_ID" <|
        stringToPersonId


unitId : Parser (UnitId -> a) a
unitId =
    custom "UNIT_ID" <|
        stringToUnitId


stringToStarSystemId : String -> Maybe StarSystemId
stringToStarSystemId s =
    Maybe.map StarSystemId (String.toInt s)


stringToPlanetId : String -> Maybe PlanetId
stringToPlanetId s =
    Maybe.map PlanetId (String.toInt s)


stringToPersonId : String -> Maybe PersonId
stringToPersonId s =
    Maybe.map PersonId (String.toInt s)


stringToUnitId : String -> Maybe UnitId
stringToUnitId s =
    Maybe.map UnitId (String.toInt s)


routes : Parser (Route -> a) a
routes =
    oneOf
        [ map HomeR top
        , map ProfileR (s "profile")
        , map ResearchR (s "research")
        , map StarSystemsR (s "starsystem")
        , map StarSystemR (s "starsystem" </> starSystemId)
        , map PlanetR (s "planet" </> planetId)
        , map BasesR (s "base")
        , map FleetR (s "fleet")
        , map DesignerR (s "designer")
        , map ConstructionR (s "construction")
        , map MessagesR (s "message")
        , map PersonR (s "person" </> personId)
        , map UnitR (s "unit" </> unitId)
        , map AdminR (s "admin")
        , map AdminListPeopleR (s "admin" </> s "people")
        , map AdminPersonR (s "admin" </> s "people" </> personId)
        , map AdminNewPersonR (s "admin" </> s "addPerson")
        , map LogoutR (s "logout")
        ]


{-| Parse route from Url
In case when parsing fails, HomeR is returned
-}
parseLocation : Url -> Route
parseLocation url =
    case parse routes url of
        Just route ->
            route

        Nothing ->
            HomeR
