{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.People
    where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()
import QC.Generators.Common ( perhaps, ArbStarDate(..) )
import QC.Generators.Database

import TestImport
import Common ( safeHead )
import People.Data ( PersonName(..), FirstName(..), FamilyName(..)
                   , Cognomen(..), RegnalNumber(..), PersonIntel(..)
                   , RelationType(..), RelationVisibility(..)
                   , StatScore(..), Diplomacy(..), Martial(..)
                   , Stewardship(..), Intrique(..), Learning(..), Gender(..)
                   , Sex(..)
                   )
import Queries ( PersonDataLink(..) )


anyPersonName :: Gen PersonName
anyPersonName =
    oneof [ anySimpleName
          , anyRegularName
          , anyRegalName
          ]


-- | Generator for simple names consisting of first name and maybe cognomen
anySimpleName :: Gen PersonName
anySimpleName = do
    firstName <- anyFirstName
    cognomen <- perhaps anyCognomen
    return $ SimpleName firstName cognomen


-- | Generator for regular names consisting of first and family names and maybe a cognomen
anyRegularName :: Gen PersonName
anyRegularName = do
    firstName <- anyFirstName
    familyName <- anyFamilyName
    cognomen <- perhaps anyCognomen
    return $ RegularName firstName familyName cognomen


-- | Generator for regal names consisting of first and family names, regnal number and maybe a cognomen
anyRegalName :: Gen PersonName
anyRegalName = do
    firstName <- anyFirstName
    familyName <- anyFamilyName
    regnalNumber <- anyRegnalNumber
    cognomen <- perhaps anyCognomen
    return $ RegalName firstName familyName regnalNumber cognomen


anyFirstName :: Gen FirstName
anyFirstName = do
    s <- arbitrary
    return $ FirstName s


anyFamilyName :: Gen FamilyName
anyFamilyName = do
    s <- arbitrary
    return $ FamilyName s


anyCognomen :: Gen Cognomen
anyCognomen = do
    s <- arbitrary
    return $ Cognomen s


anyRegnalNumber :: Gen RegnalNumber
anyRegnalNumber = do
    n <- arbitrary `suchThat` \x -> x > 0
    return $ RegnalNumber n


anyPersonIntel :: Gen PersonIntel
anyPersonIntel = elements [minBound..]


intelWithoutFamilyOrSecretMatters :: Gen PersonIntel
intelWithoutFamilyOrSecretMatters = do
    let relevant = filter (\x -> x /= FamilyRelations && x /= SecretRelations) [minBound..]
    elements relevant


intelWithoutSecretMatters :: Gen PersonIntel
intelWithoutSecretMatters = do
    let relevant = filter (\x -> x /= SecretRelations) [minBound..]
    elements relevant


intelWithFamilyOrSecretMatters :: Gen PersonIntel
intelWithFamilyOrSecretMatters = do
    elements [ FamilyRelations
             , SecretRelations
             ]


intelWithFamilyMatters :: Gen PersonIntel
intelWithFamilyMatters = do
    elements [ FamilyRelations]


intelWithSecretMatters :: Gen PersonIntel
intelWithSecretMatters = do
    elements [ SecretRelations ]


anyRelationType :: Gen RelationType
anyRelationType = elements [minBound..]


anyRelationVisibility :: Gen RelationVisibility
anyRelationVisibility = elements [minBound..]


-- | Arbitrary relation with given visibility
relationWithVisibility :: RelationVisibility -> Gen Relation
relationWithVisibility visibility = do
    originator <- randomPersonKey
    target <- randomPersonKey `suchThat` \x -> x /= originator
    aType <- anyRelationType
    return $ Relation
                { relationOriginatorId = originator
                , relationTargetId = target
                , relationType = aType
                , relationVisibility = visibility
                }


anyPersonDataLink :: RelationVisibility -> Gen PersonIntel -> Gen PersonIntel -> Gen PersonDataLink
anyPersonDataLink linkVisibility targetIntel originatorIntel = do
    targetPerson <- anyPersonEntity []
    ownerId <- randomPersonKey `suchThat` (\x -> x /= entityKey targetPerson)
    originatorPersonKey <- randomPersonKey `suchThat` (\x -> x /= (entityKey targetPerson)
                                                             && x /= ownerId)
    tIntel <- anyHumanIntelligence ownerId (entityKey targetPerson) targetIntel
    oIntel <- anyHumanIntelligence ownerId originatorPersonKey originatorIntel
    relation <- anyRelation (entityKey targetPerson) originatorPersonKey linkVisibility
    return $ PersonDataLink
        { personDataLinkTargetIntelligence = tIntel
        , personDataLinkRelation = relation
        , personDataLinkPerson = targetPerson
        , personDataLinkOriginatorIntelligence = oIntel
        }


anyHumanIntelligence :: Key Person -> Key Person -> Gen PersonIntel -> Gen HumanIntelligence
anyHumanIntelligence ownerId targetId levels = do
    aLevel <- levels
    return $ HumanIntelligence
                { humanIntelligencePersonId = targetId
                , humanIntelligenceOwnerId = ownerId
                , humanIntelligenceLevel = aLevel
                }


anyRelation :: Key Person -> Key Person -> RelationVisibility -> Gen Relation
anyRelation targetId originatorId visibility = do
    aType <- anyRelationType
    return $ Relation
                { relationOriginatorId = originatorId
                , relationTargetId = targetId
                , relationType = aType
                , relationVisibility = visibility
                }


anySex :: Gen Sex
anySex =
    elements [minBound..]


anyGender :: Gen Gender
anyGender =
    elements [minBound..]


anyDiplomacyScore :: Gen (StatScore Diplomacy)
anyDiplomacyScore = do
    n <- arbitrary `suchThat` \x -> x > 0
    return $ StatScore n


anyMartialScore :: Gen (StatScore Martial)
anyMartialScore = do
    n <- arbitrary `suchThat` \x -> x > 0
    return $ StatScore n


anyStewardship :: Gen (StatScore Stewardship)
anyStewardship = do
    n <- arbitrary `suchThat` \x -> x > 0
    return $ StatScore n


anyLearning :: Gen (StatScore Learning)
anyLearning = do
    n <- arbitrary `suchThat` \x -> x > 0
    return $ StatScore n


anyIntrique :: Gen (StatScore Intrique)
anyIntrique = do
    n <- arbitrary `suchThat` \x -> x > 0
    return $ StatScore n


anyPersonEntity  :: [PersonParameter] -> Gen (Entity Person)
anyPersonEntity params = do
    personId <- case getPersonId params of
                    Nothing ->
                        randomPersonKey
                    Just pId ->
                        return pId
    name <- anyPersonName
    sex <- anySex
    gender <- anyGender
    aStarDate <- arbitrary
    diplomacy <- anyDiplomacyScore
    martial <- anyMartialScore
    stewardship <- anyStewardship
    intrique <- anyIntrique
    learning <- anyLearning
    factionId <- perhaps randomFactionKey -- todo implement
    let planetTitle = Nothing -- todo implement
    let starSystemTitle = Nothing -- todo implement
    let dynastyId = Nothing -- todo implement
    return $
        Entity personId $ Person
                { personName = name
                , personSex = sex
                , personGender = gender
                , personDateOfBirth = unArbStarDate aStarDate
                , personDiplomacy = diplomacy
                , personMartial = martial
                , personStewardship = stewardship
                , personIntrique = intrique
                , personLearning = learning
                , personFactionId = factionId
                , personPlanetTitle = planetTitle
                , personStarSystemTitle = starSystemTitle
                , personDynastyId = dynastyId
                }


data PersonParameter =
    PlanetaryPrimaryTitle (Key Planet)
    | RandomPlanetaryPrimaryTitle
    | StarSystemPrimaryTitle (Key StarSystem)
    | RandomStarSystemPrimaryTitle
    | WithPersonId (Key Person)
    | WithFactionId (Key Faction)
    | RandomFactionId
    | WithDynastyId (Key Dynasty)
    | RandomDynastyId
    deriving (Show, Read, Eq)


getPersonId :: [PersonParameter] -> Maybe (Key Person)
getPersonId params =
    safeHead $ mapMaybe (\x -> case x of
                            WithPersonId n ->
                                Just n
                            _ ->
                                Nothing)
                        params
