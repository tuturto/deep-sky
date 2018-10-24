module Data.Common exposing
    ( BioResource(..)
    , BuildingId(..)
    , ChemResource(..)
    , ConstructionId(..)
    , FactionId(..)
    , InfoPanelStatus(..)
    , Location(..)
    , MechResource(..)
    , PlanetId(..)
    , Resources
    , Route(..)
    , StarDate(..)
    , StarId(..)
    , StarSystemId(..)
    , locationToString
    , planetIdToString
    , routeToString
    , unBio
    , unBuildingId
    , unChem
    , unConstructionId
    , unFactionId
    , unMech
    , unPlanetId
    , unStarId
    , unStarSystemId
    )


type StarDate
    = StarDate Int


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

        AdminR ->
            "/admin"

        LogoutR ->
            "/logout"

        ResearchR ->
            "/research"


type InfoPanelStatus
    = InfoPanelOpen
    | InfoPanelClosed
