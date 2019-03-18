module Data.Construction exposing
    ( Building
    , BuildingConstructionData
    , BuildingDamage(..)
    , BuildingInfo
    , BuildingLevel(..)
    , BuildingType(..)
    , Construction(..)
    , ConstructionIndex(..)
    , ShipConstructionData
    , buildingDamageToString
    , buildingTypeToString
    , constructionIndex
    , constructionIndexSorter
    , constructionName
    , constructionPlanet
    , constructionWorkLeft
    , unBuildingDamage
    , unBuildingLevel
    , unConstructionIndex
    )

import Data.Common
    exposing
        ( BioResource(..)
        , BuildingId(..)
        , ChemResource(..)
        , ConstructionId(..)
        , MechResource(..)
        , PlanetId(..)
        , Resources
        , StarDate
        )


type alias Building =
    { id : BuildingId
    , planetId : PlanetId
    , buildingType : BuildingType
    , level : BuildingLevel
    , damage : BuildingDamage
    , date : StarDate
    }


type BuildingType
    = SensorStation
    | ResearchComplex
    | Farm
    | ParticleAccelerator
    | NeutronDetector
    | BlackMatterScanner
    | GravityWaveSensor


buildingTypeToString : BuildingType -> String
buildingTypeToString bType =
    case bType of
        SensorStation ->
            "Sensor station"

        ResearchComplex ->
            "Research complex"

        Farm ->
            "Farm"

        ParticleAccelerator ->
            "Particle accelerator"

        NeutronDetector ->
            "Neutron detector"

        BlackMatterScanner ->
            "Black matter scanner"

        GravityWaveSensor ->
            "Gravity wave sensor"


type BuildingLevel
    = BuildingLevel Int


type BuildingDamage
    = BuildingDamage Float


buildingDamageToString : BuildingDamage -> String
buildingDamageToString (BuildingDamage damage) =
    (String.fromFloat <| damage * 100) ++ "%"


unBuildingLevel : BuildingLevel -> Int
unBuildingLevel (BuildingLevel x) =
    x


unBuildingDamage : BuildingDamage -> Float
unBuildingDamage (BuildingDamage x) =
    x


type Construction
    = BuildingConstruction BuildingConstructionData
    | ShipConstruction ShipConstructionData


type ConstructionIndex
    = ConstructionIndex Int


unConstructionIndex : ConstructionIndex -> Int
unConstructionIndex (ConstructionIndex x) =
    x


type alias BuildingConstructionData =
    { id : ConstructionId
    , name : String
    , index : ConstructionIndex
    , level : BuildingLevel
    , buildingType : BuildingType
    , planet : PlanetId
    , workLeft : Resources
    }


type alias ShipConstructionData =
    { id : ConstructionId
    , name : String
    , index : ConstructionIndex
    }


constructionPlanet : Construction -> Maybe PlanetId
constructionPlanet construction =
    case construction of
        BuildingConstruction building ->
            Just building.planet

        ShipConstruction ship ->
            Nothing


constructionIndex : Construction -> ConstructionIndex
constructionIndex c =
    case c of
        BuildingConstruction x ->
            x.index

        ShipConstruction x ->
            x.index


constructionName : Construction -> String
constructionName c =
    case c of
        BuildingConstruction x ->
            x.name

        ShipConstruction x ->
            x.name


constructionWorkLeft : Construction -> Resources
constructionWorkLeft c =
    case c of
        BuildingConstruction x ->
            x.workLeft

        ShipConstruction x ->
            Resources (BioResource 0) (MechResource 0) (ChemResource 0)


constructionIndexSorter : Construction -> Construction -> Order
constructionIndexSorter a b =
    let
        index_a =
            constructionIndex a

        index_b =
            constructionIndex b
    in
    compare (unConstructionIndex index_a) (unConstructionIndex index_b)


type alias BuildingInfo =
    { infoType : BuildingType
    , name : String
    , level : BuildingLevel
    , cost : Resources
    , description : String
    }
