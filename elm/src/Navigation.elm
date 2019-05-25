module Navigation exposing (parseLocation, routes)

{-| Navigation related functions and data types
-}

import Data.Common
    exposing
        ( PersonId(..)
        , PlanetId(..)
        , Route(..)
        , StarSystemId(..)
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
        , string
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


stringToStarSystemId : String -> Maybe StarSystemId
stringToStarSystemId s =
    Maybe.map StarSystemId (String.toInt s)


stringToPlanetId : String -> Maybe PlanetId
stringToPlanetId s =
    Maybe.map PlanetId (String.toInt s)


stringToPersonId : String -> Maybe PersonId
stringToPersonId s =
    Maybe.map PersonId (String.toInt s)


routes : Parser (Route -> a) a
routes =
    oneOf
        [ map HomeR top
        , map ProfileR (s "profile")
        , map ResearchR (s "research")
        , map StarSystemsR (s "starsystem")
        , map StarSystemR (s "starsystem" </> starSystemId)
        , map PlanetR (s "starsystem" </> starSystemId </> planetId)
        , map BasesR (s "base")
        , map FleetR (s "fleet")
        , map DesignerR (s "designer")
        , map ConstructionR (s "construction")
        , map MessagesR (s "message")
        , map PersonR (s "person" </> personId)
        , map AdminR (s "admin")
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
