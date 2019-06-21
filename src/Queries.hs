{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Queries
    ( ShipLandingStatus(..), PersonRelationData(..), PersonDataLink(..)
    , planetPopulationReports, shipsAtPlanet, planetConstructionQueue
    , kragiiTargetPlanets, farmingChangeTargetPlanets, factionBuildings
    , chassisList, planetReports, starSystemReports, personAndDynasty
    , personRelations, personDataLinkPersonL, personDataLinkRelationL
    , personDataLinkTargetIntelligenceL, personDataLinkOriginatorIntelligenceL
    )
    where

import Import
import qualified Prelude as P
import Control.Lens ( Lens', lens )
import qualified Database.Esqueleto as E
import Data.List ( nub )
import Common ( safeHead )
import CustomTypes ( BuildingType(..), PlanetaryStatus(..) )


-- | Load population reports of a planet and respective races
planetPopulationReports :: (MonadIO m, BackendCompatible SqlBackend backend,
                                PersistQueryRead backend, PersistUniqueRead backend) =>
                               Key Planet -> Key Faction -> ReaderT backend m [(Entity PlanetPopulationReport, Maybe (Entity Race))]
planetPopulationReports pId fId =
    E.select $
        E.from $ \(popReport `E.LeftOuterJoin` pRace) -> do
            E.on (popReport E.^. PlanetPopulationReportRaceId E.==. pRace E.?. RaceId )
            E.where_ (popReport E.^. PlanetPopulationReportPlanetId E.==. E.val pId
                      E.&&. popReport E.^. PlanetPopulationReportFactionId E.==. E.val fId)
            E.orderBy [ E.asc ( popReport E.^. PlanetPopulationReportPlanetId)
                      , E.asc ( popReport E.^. PlanetPopulationReportRaceId)
                      , E.asc ( popReport E.^. PlanetPopulationReportDate)
                      ]
            return (popReport, pRace)


-- | Load ships that are on or around a given planet and their faction info
shipsAtPlanet :: (MonadIO m, BackendCompatible SqlBackend backend,
                    PersistQueryRead backend, PersistUniqueRead backend) =>
                   Key Planet -> ShipLandingStatus -> ReaderT backend m [(Entity Ship, Entity Faction)]
shipsAtPlanet pId landingStatus = do
    let landed = case landingStatus of
                    ShipOnPlanet -> True
                    ShipInOrbit -> False
    E.select $
        E.from $ \(ship `E.InnerJoin` faction) -> do
                E.on (ship E.^. ShipOwnerId E.==. faction E.^. FactionId)
                E.where_ (ship E.^. ShipPlanetId E.==. E.val (Just pId)
                          E.&&. ship E.^. ShipLanded E.==. E.val landed)
                return (ship, faction)

data ShipLandingStatus = ShipOnPlanet | ShipInOrbit


-- | Load planet with construction queue
planetConstructionQueue :: (MonadIO m, BackendCompatible SqlBackend backend,
                            PersistQueryRead backend, PersistUniqueRead backend) =>
                           Key Planet -> ReaderT backend m (Maybe (Entity Planet), [Entity BuildingConstruction])
planetConstructionQueue pId = do
    res <- E.select $
            E.from $ \(planet `E.LeftOuterJoin` bConstruction) -> do
                E.on (bConstruction E.?. BuildingConstructionPlanetId E.==. E.just (planet E.^. PlanetId))
                E.where_ (planet E.^. PlanetId E.==. E.val pId)
                return (planet, bConstruction)
    let planet = fst <$> safeHead res
    let constructions = mapMaybe snd res
    return (planet, constructions)


-- | Load planets that are kragii attack candidates
kragiiTargetPlanets :: (MonadIO m, BackendCompatible SqlBackend backend
                           , PersistQueryRead backend, PersistUniqueRead backend) =>
                           Int -> Int -> Key Faction -> ReaderT backend m [Entity Planet]
kragiiTargetPlanets pop farms fId = do
    planets <- E.select $
        E.from $ \(planet `E.LeftOuterJoin` population `E.LeftOuterJoin` building `E.LeftOuterJoin` status) -> do
            E.on (status E.?. PlanetStatusPlanetId E.==. E.just (planet E.^. PlanetId)
                  E.&&. status E.?. PlanetStatusStatus E.==. E.val (Just KragiiAttack))
            E.on (building E.?. BuildingPlanetId E.==. E.just (planet E.^. PlanetId))
            E.on (population E.?. PlanetPopulationPlanetId E.==. E.just (planet E.^. PlanetId))
            E.where_ (planet E.^. PlanetOwnerId E.==. E.val (Just fId)
                      E.&&. building E.?. BuildingType E.==. E.val (Just Farm)
                      E.&&. E.isNothing (status E.?. PlanetStatusStatus))
            E.orderBy [ E.asc (planet E.^. PlanetId) ]
            return (planet, population, building)
    let grouped = groupBy (\(a, _, _) (b, _, _) -> entityKey a == entityKey b) planets
    let counted = catMaybes $ fmap farmAndPopCount grouped
    let filtered = filter (\(_, p, f) ->
                                p >= pop
                                || f >= farms) counted
    let mapped = fmap (\(ent, _, _) -> ent) filtered
    return mapped


-- | Load planets that are farming production change candidates
-- these planets have population of at least 1 and 1 or more farms
farmingChangeTargetPlanets :: (MonadIO m, BackendCompatible SqlBackend backend
    , PersistQueryRead backend, PersistUniqueRead backend) =>
    Key Faction -> ReaderT backend m [Entity Planet]
farmingChangeTargetPlanets fId = do
    planets <- E.select $
        E.from $ \(planet `E.LeftOuterJoin` population `E.LeftOuterJoin` building `E.LeftOuterJoin` status) -> do
            E.on (status E.?. PlanetStatusPlanetId E.==. E.just (planet E.^. PlanetId)
                  E.&&. status E.?. PlanetStatusStatus `E.in_` E.valList [ Just GoodHarvest
                                                                         , Just PoorHarvest
                                                                         ])
            E.on (building E.?. BuildingPlanetId E.==. E.just (planet E.^. PlanetId))
            E.on (population E.?. PlanetPopulationPlanetId E.==. E.just (planet E.^. PlanetId))
            E.where_ (planet E.^. PlanetOwnerId E.==. E.val (Just fId)
                      E.&&. building E.?. BuildingType E.==. E.val (Just Farm)
                      E.&&. E.isNothing (status E.?. PlanetStatusStatus))
            E.orderBy [ E.asc (planet E.^. PlanetId) ]
            return (planet, population, building)
    let grouped = groupBy (\(a, _, _) (b, _, _) -> entityKey a == entityKey b) planets
    let counted = catMaybes $ fmap farmAndPopCount grouped
    let filtered = filter (\(_, p, f) ->
                                p >= 1
                                && f >= 1) counted
    let mapped = fmap (\(ent, _, _) -> ent) filtered
    return mapped


-- | Obtain planet's total population and total amount of farms
-- All tuples are considered to be for same planet
farmAndPopCount :: [(Entity Planet, Maybe (Entity PlanetPopulation), Maybe (Entity Building))] -> Maybe (Entity Planet, Int, Int)
farmAndPopCount [] = Nothing
farmAndPopCount xs =
    let
        populationCount = (sum . fmap (planetPopulationPopulation . entityVal) . nub . catMaybes . fmap (\(_, x, _) -> x)) xs
        farmCount = (length . nub . catMaybes . fmap (\(_, _, x) -> x)) xs
        (planet, _, _) = P.head xs
    in
        Just (planet, populationCount, farmCount)


-- | Load given faction's planets and buildings on them
factionBuildings :: (MonadIO m,
    BackendCompatible SqlBackend backend, PersistQueryRead backend,
    PersistUniqueRead backend) =>
    Key Faction -> ReaderT backend m [(Entity Planet, [Entity Building])]
factionBuildings fId = do
    planetBuildings <- E.select $
        E.from $ \(planet `E.LeftOuterJoin` building) -> do
            E.on (building E.?. BuildingPlanetId E.==. E.just (planet E.^. PlanetId))
            E.where_ (planet E.^. PlanetOwnerId E.==. E.val (Just fId))
            E.orderBy [ E.asc (planet E.^. PlanetId)]
            return (planet, building)
    let grouped = groupBy (\a b -> (entityKey $ fst a) == (entityKey $ fst b)) planetBuildings
    return $ mapMaybe groupUnderParent grouped


-- | Load chassis and their required components for a given faction, taking
-- faction's current completed research into account
chassisList :: (MonadIO m, BackendCompatible SqlBackend backend,
    PersistQueryRead backend, PersistUniqueRead backend) =>
    (Key Faction) -> ReaderT backend m [(Entity Chassis, [Entity RequiredComponent])]
chassisList fId = do
    chassisRequirements <- E.select $
        E.from $ \(chassis `E.LeftOuterJoin` requirement) -> do
            E.on (requirement E.?. RequiredComponentChassisId E.==. E.just (chassis E.^. ChassisId))
            E.where_ (chassis E.^. ChassisTechnology `E.in_`
                        (E.subList_select $
                            E.from $ \tech -> do
                                E.where_ (tech E.^. CompletedResearchFactionId E.==. (E.val fId))
                                return $ E.just $ tech E.^. CompletedResearchType)
                      E.||. E.isNothing (chassis E.^. ChassisTechnology))
            E.orderBy [ E.asc ( chassis E.^. ChassisId) ]
            return (chassis, requirement)
    let grouped = groupBy (\a b -> (entityKey $ fst a) == (entityKey $ fst b)) chassisRequirements
    return $ mapMaybe groupUnderParent grouped


-- | Given a list of tuples, where first element is always the same and is
-- considered as a parent for second element in tuple, group children
-- under single parent
groupUnderParent :: [(a, Maybe b)] -> Maybe (a, [b])
groupUnderParent xs =
    (,) <$> parent <*> Just childs
    where
        parent = fst <$> (safeHead xs)
        childs = mapMaybe snd xs


-- | Load reports of given planet and join people for ruler information
planetReports :: (MonadIO m, BackendCompatible SqlBackend backend,
    PersistQueryRead backend, PersistUniqueRead backend) =>
    Key Faction -> Key Planet
    -> ReaderT backend m [(PlanetReport, Maybe (Person))]
planetReports fId planetId = do
    pairs <- E.select $
        E.from $ \(planet `E.LeftOuterJoin` person) -> do
            E.on (person E.?. PersonId E.==. ( planet E.^. PlanetReportRulerId))
            E.where_ (planet E.^. PlanetReportFactionId E.==. (E.val fId)
                      E.&&. planet E.^. PlanetReportPlanetId E.==. (E.val planetId))
            E.orderBy [ E.asc (planet E.^. PlanetReportPlanetId)
                      , E.desc (planet E.^. PlanetReportDate)]
            return (planet, person)
    let res = fmap (\(x, y) -> (entityVal x, entityVal <$> y)) pairs
    return res


-- | Load reports of given star system and join people for ruler information
starSystemReports :: (MonadIO m,
    BackendCompatible SqlBackend backend, PersistQueryRead backend,
    PersistUniqueRead backend) =>
    Key Faction -> Key StarSystem
   -> ReaderT backend m [(StarSystemReport, Maybe Person)]
starSystemReports fId sId = do
    pairs <- E.select $
        E.from $ \(system `E.LeftOuterJoin` person) -> do
            E.on (person E.?. PersonId E.==. ( system E.^. StarSystemReportRulerId))
            E.where_ (system E.^. StarSystemReportFactionId E.==. (E.val fId)
                      E.&&. system E.^. StarSystemReportStarSystemId E.==. (E.val sId))
            E.orderBy [ E.asc (system E.^. StarSystemReportStarSystemId)
                      , E.desc (system E.^. StarSystemReportDate)]
            return (system, person)
    let res = fmap (\(x, y) -> (entityVal x, entityVal <$> y)) pairs
    return res


personAndDynasty :: (MonadIO m, BackendCompatible SqlBackend backend,
    PersistQueryRead backend, PersistUniqueRead backend) =>
    Key Person -> ReaderT backend m (Maybe (Entity Person, Maybe (Entity Dynasty)))
personAndDynasty pId = do
    pairs <- E.select $
        E.from $ \(person `E.LeftOuterJoin` dynasty) -> do
            E.on (person E.^. PersonDynastyId E.==. ( dynasty E.?. DynastyId))
            E.where_ (person E.^. PersonId E.==. (E.val pId))
            return (person, dynasty)
    return $ safeHead pairs


-- | Load person relation information
-- returned data is structured as
-- target of query as Entity Person
-- intelligence targeting target as Entity HumanIntelligence
-- relation targeting the target as Entity Relation
-- originator of relation as Entity Person
-- intelligence targeting originator as Entity HumanIntelligence
personRelations :: (MonadIO m, BackendCompatible SqlBackend backend,
    PersistQueryRead backend, PersistUniqueRead backend) =>
    Key Person -> Key Person -> ReaderT backend m (Maybe PersonRelationData)
personRelations pId ownerId = do
    info <- E.select $
        E.from $ \(person1
                    `E.LeftOuterJoin` relation
                    `E.LeftOuterJoin` person2
                    `E.LeftOuterJoin` intel1
                    `E.LeftOuterJoin` intel2) -> do
            E.on (person2 E.?. PersonId E.==. ( intel2 E.?. HumanIntelligencePersonId))
            E.on (E.just (person1 E.^. PersonId) E.==. ( intel1 E.?. HumanIntelligencePersonId))
            E.on (person2 E.?. PersonId E.==. ( relation E.?. RelationOriginatorId))
            E.on (E.just (person1 E.^. PersonId) E.==. ( relation E.?. RelationTargetId))
            E.where_ (person1 E.^. PersonId E.==. (E.val pId)
                      E.&&. (intel1 E.?. HumanIntelligenceOwnerId E.==. (E.just (E.val ownerId))
                             E.||. intel2 E.?. HumanIntelligenceOwnerId E.==. (E.just (E.val ownerId))))
            return (person1, intel1, relation, person2, intel2)
    return $ mapPersonInfo info


data PersonRelationData = PersonRelationData
    { personRelationDataPerson :: !(Entity Person)
    , personRelationDataLinks :: ![PersonDataLink]
    } deriving (Show, Read, Eq)


data PersonDataLink = PersonDataLink
    { personDataLinkTargetIntelligence :: !(Maybe HumanIntelligence)
    , personDataLinkRelation :: !Relation
    , personDataLinkPerson :: !(Entity Person)
    , personDataLinkOriginatorIntelligence :: !(Maybe HumanIntelligence)
    } deriving (Show, Read, Eq)


-- | post process data fetched by personRelations query
mapPersonInfo :: [(Entity Person, Maybe (Entity HumanIntelligence)
                 ,Maybe (Entity Relation), Maybe (Entity Person), Maybe (Entity HumanIntelligence))]
                 -> Maybe PersonRelationData
mapPersonInfo [] =
    Nothing

mapPersonInfo info =
    Just $ PersonRelationData { personRelationDataPerson = (target . P.head) info
                              , personRelationDataLinks = mapMaybe linker info
                              }
    where
        target = \(x, _, _, _, _) -> x


-- | build person data link from data fetched by personRelations query
linker :: (Entity Person, Maybe (Entity HumanIntelligence),Maybe (Entity Relation)
          , Maybe (Entity Person), Maybe (Entity HumanIntelligence)) -> Maybe PersonDataLink
linker (_, Nothing, _, _, Nothing) =
    -- if there's no intel at all, there's nothing we can report
    Nothing

linker (_, _, Nothing, _, _) =
    -- if relation isn't known, there's nothing we can report
    Nothing

linker (_, _, _, Nothing, _) =
    -- if related person isn't known, there's nothing we can report
    Nothing

linker (_, meTargetIntel, Just (eRelation), Just (eOriginator), meOriginatorIntel) =
    Just $ PersonDataLink
        { personDataLinkTargetIntelligence = entityVal <$> meTargetIntel
        , personDataLinkRelation = entityVal eRelation
        , personDataLinkPerson = eOriginator
        , personDataLinkOriginatorIntelligence = entityVal <$> meOriginatorIntel
        }


personDataLinkOriginatorIntelligenceL :: Lens' PersonDataLink (Maybe HumanIntelligence)
personDataLinkOriginatorIntelligenceL = lens personDataLinkOriginatorIntelligence
                                             (\r v -> r { personDataLinkOriginatorIntelligence = v})


personDataLinkTargetIntelligenceL :: Lens' PersonDataLink (Maybe HumanIntelligence)
personDataLinkTargetIntelligenceL = lens personDataLinkTargetIntelligence
                                         (\r v -> r { personDataLinkTargetIntelligence = v})


personDataLinkRelationL :: Lens' PersonDataLink Relation
personDataLinkRelationL = lens personDataLinkRelation
                               (\r v -> r { personDataLinkRelation = v})


personDataLinkPersonL :: Lens' PersonDataLink (Entity Person)
personDataLinkPersonL = lens personDataLinkPerson
                             (\r v -> r { personDataLinkPerson = v})
