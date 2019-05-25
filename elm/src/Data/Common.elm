module Data.Common exposing
    ( BioResource(..)
    , BuildingId(..)
    , ChemResource(..)
    , ConstructionId(..)
    , DesignId(..)
    , ErrorMessage(..)
    , FactionId(..)
    , InfoPanelStatus(..)
    , Location(..)
    , MechResource(..)
    , MessageId(..)
    , PersonId(..)
    , PlanetId(..)
    , ResourceType(..)
    , Resources
    , Route(..)
    , ShipId(..)
    , StarDate(..)
    , StarId(..)
    , StarSystemId(..)
    , UserId(..)
    , capitalize
    , constructionIdToString
    , designIdToString
    , error
    , findFirst
    , locationToString
    , maxPage
    , messageIdToString
    , personIdToString
    , planetIdToString
    , routeToString
    , triple
    , unBio
    , unBuildingId
    , unChem
    , unConstructionId
    , unDesignId
    , unFactionId
    , unMech
    , unMessageId
    , unPersonId
    , unPlanetId
    , unShipId
    , unStarDate
    , unStarId
    , unStarSystemId
    , unUserId
    , writtenNumber
    )

import Http exposing (Error(..))


type StarDate
    = StarDate Int


unStarDate : StarDate -> Int
unStarDate (StarDate x) =
    x


type BioResource
    = BioResource Int


type MechResource
    = MechResource Int


type ChemResource
    = ChemResource Int


unBio : BioResource -> Int
unBio (BioResource x) =
    x


unMech : MechResource -> Int
unMech (MechResource x) =
    x


unChem : ChemResource -> Int
unChem (ChemResource x) =
    x


type alias Resources =
    { biological : BioResource
    , mechanical : MechResource
    , chemical : ChemResource
    }


type Location
    = Location Float Float


locationToString : Location -> String
locationToString (Location x y) =
    "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"


type Route
    = HomeR
    | ProfileR
    | StarSystemsR
    | StarSystemR StarSystemId
    | PlanetR StarSystemId PlanetId
    | BasesR
    | FleetR
    | DesignerR
    | ConstructionR
    | MessagesR
    | PersonR PersonId
    | AdminR
    | LogoutR
    | ResearchR


type StarSystemId
    = StarSystemId Int


type StarId
    = StarId Int


type PlanetId
    = PlanetId Int


planetIdToString : PlanetId -> String
planetIdToString (PlanetId x) =
    String.fromInt x


type FactionId
    = FactionId Int


type BuildingId
    = BuildingId Int


type ConstructionId
    = ConstructionId Int


unConstructionId : ConstructionId -> Int
unConstructionId (ConstructionId cId) =
    cId


constructionIdToString : ConstructionId -> String
constructionIdToString (ConstructionId cId) =
    String.fromInt cId


unStarSystemId : StarSystemId -> Int
unStarSystemId (StarSystemId sId) =
    sId


unStarId : StarId -> Int
unStarId (StarId sId) =
    sId


unPlanetId : PlanetId -> Int
unPlanetId (PlanetId pId) =
    pId


unFactionId : FactionId -> Int
unFactionId (FactionId fId) =
    fId


unBuildingId : BuildingId -> Int
unBuildingId (BuildingId bId) =
    bId


routeToString : Route -> String
routeToString route =
    case route of
        HomeR ->
            "/home"

        ProfileR ->
            "/profile"

        StarSystemR (StarSystemId sId) ->
            "/starsystem/" ++ String.fromInt sId

        PlanetR (StarSystemId sId) (PlanetId pId) ->
            "/starsystem/" ++ String.fromInt sId ++ "/" ++ String.fromInt pId

        StarSystemsR ->
            "/starsystem"

        BasesR ->
            "/base"

        FleetR ->
            "/fleet"

        DesignerR ->
            "/designer"

        ConstructionR ->
            "/construction"

        MessagesR ->
            "/message"

        PersonR pId ->
            "/person/" ++ personIdToString pId

        AdminR ->
            "/admin"

        LogoutR ->
            "/logout"

        ResearchR ->
            "/research"


type InfoPanelStatus
    = InfoPanelOpen
    | InfoPanelClosed


type UserId
    = UserId Int


unUserId : UserId -> Int
unUserId (UserId x) =
    x


type DesignId
    = DesignId Int


unDesignId : DesignId -> Int
unDesignId (DesignId x) =
    x


designIdToString : DesignId -> String
designIdToString (DesignId x) =
    String.fromInt x


type ShipId
    = ShipId Int


unShipId : ShipId -> Int
unShipId (ShipId x) =
    x


type MessageId
    = MessageId Int


unMessageId : MessageId -> Int
unMessageId (MessageId x) =
    x


messageIdToString : MessageId -> String
messageIdToString (MessageId x) =
    String.fromInt x


type PersonId
    = PersonId Int


unPersonId : PersonId -> Int
unPersonId (PersonId x) =
    x


personIdToString : PersonId -> String
personIdToString (PersonId x) =
    String.fromInt x


triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )


{-| Given a list and page size, determine number of last page
First page number is considered to be 0
-}
maxPage : Int -> List a -> Int
maxPage pageSize coll =
    let
        listLength =
            List.length coll

        pageCount =
            listLength // pageSize

        lastPageCorrection =
            if remainderBy pageSize listLength == 0 && pageCount > 0 then
                -1

            else
                0
    in
    pageCount + lastPageCorrection


type ResourceType
    = BiologicalResource
    | MechanicalResource
    | ChemicalResource


{-| First element of the list that satisfies predicate
-}
findFirst : (a -> Bool) -> List a -> Maybe a
findFirst pred l =
    List.head <| List.filter pred l


{-| Turn number into name of the number.
Only small space is currently supported
-}
writtenNumber : Int -> String
writtenNumber n =
    case n of
        0 ->
            "zero"

        1 ->
            "one"

        2 ->
            "two"

        3 ->
            "three"

        4 ->
            "four"

        5 ->
            "five"

        6 ->
            "six"

        7 ->
            "seven"

        8 ->
            "eight"

        9 ->
            "nine"

        10 ->
            "ten"

        _ ->
            "many"


{-| String with first letter turned into upper-case
-}
capitalize : String -> String
capitalize s =
    case String.uncons s of
        Nothing ->
            s

        Just ( x, xs ) ->
            String.cons (Char.toUpper x) xs


{-| Turn given error with descriptive text into error message
-}
error : Http.Error -> String -> ErrorMessage
error err msg =
    ErrorMessage (msg ++ " - " ++ errorToString err)


type ErrorMessage
    = ErrorMessage String


{-| Turn Http error message into user readable string
-}
errorToString : Error -> String
errorToString err =
    case err of
        BadUrl url ->
            "Bad URL: " ++ url

        Timeout ->
            "timeout"

        NetworkError ->
            "network error"

        BadStatus _ ->
            "bad status"

        BadPayload msg _ ->
            "Bad payload: " ++ msg
