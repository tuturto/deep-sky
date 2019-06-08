{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.People
    where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()
import QC.Generators.Common ( perhaps )
import QC.Generators.Database

import TestImport
import People.Data ( PersonName(..), FirstName(..), FamilyName(..)
                   , Cognomen(..), RegnalNumber(..), PersonIntel(..)
                   , RelationType(..), RelationVisibility(..)
                   )


newtype ArbPersonName = ArbPersonName { unArbPersonName :: PersonName }
    deriving (Show, Read, Eq)


newtype ArbFirstName = ArbFirstName { unArbFirstName :: FirstName }
    deriving (Show, Read, Eq)


newtype ArbFamilyName = ArbFamilyName { unArbFamilyName :: FamilyName }
    deriving (Show, Read, Eq)


newtype ArbCognomen = ArbCognomen { unArbCognomen :: Cognomen }
    deriving (Show, Read, Eq)


newtype ArbRegnalNumber = ArbRegnalNumber { unArbRegnalNumber :: RegnalNumber }
    deriving (Show, Read, Eq)


instance Arbitrary ArbPersonName where
    arbitrary = oneof [ simpleName
                      , regularName
                      , regalName
                      ]


-- | Generator for simple names consisting of first name and maybe cognomen
simpleName :: Gen ArbPersonName
simpleName = do
    aFirstName <- arbitrary
    aCognomen <- perhaps arbitrary
    let aName = SimpleName (unArbFirstName aFirstName) (unArbCognomen <$> aCognomen)
    return $ ArbPersonName aName


-- | Generator for regular names consisting of first and family names and maybe a cognomen
regularName :: Gen ArbPersonName
regularName = do
    aFirstName <- arbitrary
    aFamilyName <- arbitrary
    aCognomen <- perhaps arbitrary
    let aName = RegularName (unArbFirstName aFirstName)
                            (unArbFamilyName aFamilyName)
                            (unArbCognomen <$> aCognomen)
    return $ ArbPersonName aName


-- | Generator for regal names consisting of first and family names, regnal number and maybe a cognomen
regalName :: Gen ArbPersonName
regalName = do
    aFirstName <- arbitrary
    aFamilyName <- arbitrary
    aRegnalNumber <- arbitrary
    aCognomen <- perhaps arbitrary
    let aName = RegalName (unArbFirstName aFirstName)
                          (unArbFamilyName aFamilyName)
                          (unArbRegnalNumber aRegnalNumber)
                          (unArbCognomen <$> aCognomen)
    return $ ArbPersonName aName


instance Arbitrary ArbFirstName where
    arbitrary = do
        s <- arbitrary
        return $ ArbFirstName $ FirstName s


instance Arbitrary ArbFamilyName where
    arbitrary = do
        s <- arbitrary
        return $ ArbFamilyName $ FamilyName s


instance Arbitrary ArbCognomen where
    arbitrary = do
        s <- arbitrary
        return $ ArbCognomen $ Cognomen s


instance Arbitrary ArbRegnalNumber where
    arbitrary = do
        n <- arbitrary `suchThat` \x -> x > 0
        return $ ArbRegnalNumber $ RegnalNumber n


publicRelation :: Gen ([PersonIntel], Relation)
publicRelation = do
    intel <- listOf anyPersonIntel
    aRelation <- relationWithVisibility PublicRelation
    return (intel, aRelation)


familyRelationsWithoutFamilyOrSecretIntel :: Gen ([PersonIntel], Relation)
familyRelationsWithoutFamilyOrSecretIntel = do
    intel <- listOf intelWithoutFamilyOrSecretMatters
    aRelation <- relationWithVisibility FamilyRelation
    return (intel, aRelation)


familyRelationsWithFamilyOrSecretIntel :: Gen ([PersonIntel], Relation)
familyRelationsWithFamilyOrSecretIntel = do
    intel <- listOf anyPersonIntel `suchThat` familyOrSecretIntelIncluded
    aRelation <- relationWithVisibility FamilyRelation
    return (intel, aRelation)


secretRelationsWithoutSecretIntel :: Gen ([PersonIntel], Relation)
secretRelationsWithoutSecretIntel = do
    intel <- listOf intelWithoutSecretMatters
    aRelation <- relationWithVisibility SecretRelation
    return (intel, aRelation)


secretRelationsWithSecretIntel :: Gen ([PersonIntel], Relation)
secretRelationsWithSecretIntel = do
    intel <- listOf anyPersonIntel `suchThat` \x -> SecretRelations `elem` x
    aRelation <- relationWithVisibility SecretRelation
    return (intel, aRelation)


familyOrSecretIntelIncluded :: [PersonIntel] -> Bool
familyOrSecretIntelIncluded intel =
    FamilyRelations `elem` intel
    || SecretRelations `elem` intel


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