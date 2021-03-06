module Data.Common exposing
    ( BioResource(..)
    , BuildingId(..)
    , ChemResource(..)
    , ConstructionId(..)
    , DemesneName(..)
    , DesignId(..)
    , DynastyId(..)
    , DynastyName(..)
    , ErrorMessage(..)
    , FactionId(..)
    , InfoPanelStatus(..)
    , Location(..)
    , MechResource(..)
    , MessageId(..)
    , PagedResult
    , PersonId(..)
    , PetId(..)
    , PlanetId(..)
    , PlanetName(..)
    , ResourceType(..)
    , Resources
    , Route(..)
    , ShipId(..)
    , StarDate(..)
    , StarId(..)
    , StarName(..)
    , StarSystemId(..)
    , StarSystemName(..)
    , UnitId(..)
    , UserId(..)
    , apMaybe
    , capitalize
    , constructionIdToString
    , designIdToString
    , error
    , findFirst
    , joinMaybe
    , listOrdering
    , locationToString
    , maxPage
    , messageIdToString
    , nextStarDate
    , perhapsOrdering
    , personIdToString
    , planetIdToString
    , routeToString
    , starSystemIdToString
    , triple
    , unBio
    , unBuildingId
    , unChem
    , unConstructionId
    , unDemesneName
    , unDesignId
    , unDynastyId
    , unDynastyName
    , unFactionId
    , unMech
    , unMessageId
    , unPersonId
    , unPetId
    , unPlanetId
    , unPlanetName
    , unShipId
    , unStarDate
    , unStarId
    , unStarName
    , unStarSystemId
    , unStarSystemName
    , unUnitId
    , unUserId
    , unitIdToString
    , writtenNumber
    )

import Http exposing (Error(..))
import Ordering exposing (Ordering)
import Url.Builder exposing (absolute)


type StarDate
    = StarDate Int


unStarDate : StarDate -> Int
unStarDate (StarDate x) =
    x


nextStarDate : StarDate -> StarDate
nextStarDate (StarDate x) =
    StarDate (x + 1)


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
    | PlanetR PlanetId
    | BasesR
    | FleetR
    | DesignerR
    | ConstructionR
    | MessagesR
    | PersonR PersonId
    | UnitR UnitId
    | AdminR
    | AdminListPeopleR
    | AdminPersonR PersonId
    | AdminNewPersonR
    | LogoutR
    | ResearchR


type StarSystemId
    = StarSystemId Int


starSystemIdToString : StarSystemId -> String
starSystemIdToString (StarSystemId x) =
    String.fromInt x


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
            absolute [ "home" ] []

        ProfileR ->
            absolute [ "profile" ] []

        StarSystemR (StarSystemId sId) ->
            absolute [ "starsystem", String.fromInt sId ] []

        PlanetR (PlanetId pId) ->
            absolute [ "planet", String.fromInt pId ] []

        StarSystemsR ->
            absolute [ "starsystem" ] []

        BasesR ->
            absolute [ "base" ] []

        FleetR ->
            absolute [ "fleet" ] []

        DesignerR ->
            absolute [ "designer" ] []

        ConstructionR ->
            absolute [ "construction" ] []

        MessagesR ->
            absolute [ "message" ] []

        PersonR pId ->
            absolute [ "person", personIdToString pId ] []

        UnitR uId ->
            absolute [ "unit", unitIdToString uId ] []

        AdminR ->
            absolute [ "admin" ] []

        AdminListPeopleR ->
            absolute [ "admin", "people" ] []

        AdminPersonR pId ->
            absolute [ "admin", "people", personIdToString pId ] []

        AdminNewPersonR ->
            absolute [ "admin", "addPerson" ] []

        LogoutR ->
            absolute [ "logout" ] []

        ResearchR ->
            absolute [ "research" ] []


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


joinMaybe : Maybe (Maybe a) -> Maybe a
joinMaybe m =
    case m of
        Nothing ->
            Nothing

        Just x ->
            x


type PlanetName
    = PlanetName String


unPlanetName : PlanetName -> String
unPlanetName (PlanetName s) =
    s


type StarSystemName
    = StarSystemName String


unStarSystemName : StarSystemName -> String
unStarSystemName (StarSystemName s) =
    s


type StarName
    = StarName String


unStarName : StarName -> String
unStarName (StarName s) =
    s


type DemesneName
    = DemesneName String


unDemesneName : DemesneName -> String
unDemesneName (DemesneName s) =
    s


{-| Ordering of two lists of orderable items
-}
listOrdering : Ordering a -> List a -> List a -> Order
listOrdering ord a b =
    let
        aOrd =
            List.sortWith ord a

        bOrd =
            List.sortWith ord b
    in
    List.map2 ord aOrd bOrd
        |> List.foldl
            (\acc new ->
                case acc of
                    EQ ->
                        new

                    curr ->
                        curr
            )
            EQ
        |> Ordering.ifStillTiedThen (compare (List.length aOrd) (List.length bOrd))


{-| Ordering of two orderable Maybes
-}
perhapsOrdering : Ordering a -> Ordering (Maybe a)
perhapsOrdering ord a b =
    case a of
        Just ja ->
            case b of
                Nothing ->
                    LT

                Just jb ->
                    ord ja jb

        Nothing ->
            case b of
                Nothing ->
                    EQ

                Just _ ->
                    GT


type DynastyId
    = DynastyId Int


unDynastyId : DynastyId -> Int
unDynastyId (DynastyId n) =
    n


type DynastyName
    = DynastyName String


unDynastyName : DynastyName -> String
unDynastyName (DynastyName s) =
    s


type PetId
    = PetId Int


unPetId : PetId -> Int
unPetId (PetId n) =
    n


type UnitId
    = UnitId Int


unUnitId : UnitId -> Int
unUnitId (UnitId n) =
    n


unitIdToString : UnitId -> String
unitIdToString (UnitId n) =
    String.fromInt n


{-| Results of paged query
-}
type alias PagedResult a =
    { skip : Int
    , take : Int
    , page : Int
    , results : List a
    }


{-| Apply Maybe a to Maybe (a -> b), producing Maybe b
-}
apMaybe : Maybe a -> Maybe (a -> b) -> Maybe b
apMaybe a f =
    case ( a, f ) of
        ( Just value, Just func ) ->
            Just (func value)

        _ ->
            Nothing
