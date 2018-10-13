{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.Construction where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()
import QC.Generators.Database ( randomPlanetKey )

import CustomTypes ( RawResources(..), RawResource(..), ResourceCost, ConstructionSpeed, BuildingType(..) )
import Buildings ( building, BuildingInfo(..), BLevel(..) )
import Model

rawResources :: Gen (RawResources t)
rawResources = do
    aBio <- arbitrary `suchThat` \x -> x >= 0
    aMech <- arbitrary `suchThat` \x -> x >= 0
    aChem <- arbitrary `suchThat` \x -> x >= 0
    return $ RawResources (RawResource aBio) (RawResource aMech) (RawResource aChem)

instance Arbitrary BuildingType where
    arbitrary = oneof [ return SensorStation
                      , return ResearchComplex
                      , return Farm
                      , return ParticleAccelerator
                      , return NeutronDetector
                      , return BlackMatterScanner
                      , return GravityWaveSensor ]

unstartedConstruction :: Gen BuildingConstruction
unstartedConstruction = do
    aPlanetId <- randomPlanetKey
    aBuildingType <- arbitrary
    return $ BuildingConstruction aPlanetId 0 0 0 0 aBuildingType 1

randomConstruction :: Gen BuildingConstruction
randomConstruction = do
    aPlanetId <- randomPlanetKey
    aBuildingType <- arbitrary
    let modelBuilding = building aBuildingType $ BLevel 1
    bioProgress <- arbitrary `suchThat` \x -> x >= 0 && x <= (unRawResource . ccdBiologicalCost . buildingInfoCost) modelBuilding
    mechProgress <- arbitrary `suchThat` \x -> x >= 0 && x <= (unRawResource . ccdMechanicalCost . buildingInfoCost) modelBuilding
    chemProgress <- arbitrary `suchThat` \x -> x >= 0 && x <= (unRawResource . ccdChemicalCost . buildingInfoCost) modelBuilding
    return $ BuildingConstruction aPlanetId 0 bioProgress mechProgress chemProgress aBuildingType 1

unstartedConstructionsWithSomeSpeed :: Gen (RawResources ConstructionSpeed, BuildingConstruction, RawResources ResourceCost)
unstartedConstructionsWithSomeSpeed = do
    aConstructionSpeed <- rawResources
    aBuildingConstruction <- unstartedConstruction
    aTotalCost <- rawResources
    return (aConstructionSpeed, aBuildingConstruction, aTotalCost)

unfinishedConstructionsWithSomeSpeed :: Gen (RawResources ConstructionSpeed, BuildingConstruction, RawResources ResourceCost)
unfinishedConstructionsWithSomeSpeed = do
    aConstructionSpeed <- rawResources
    aBuildingConstruction <- randomConstruction
    aTotalCost <- rawResources
    return (aConstructionSpeed, aBuildingConstruction, aTotalCost)
