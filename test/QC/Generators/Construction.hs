{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.Construction where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()
import QC.Generators.Database ( randomPlanetKey )

import CustomTypes ( BuildingType(..) )
import Resources ( RawResources(..), RawResource(..), ResourceCost, ConstructionSpeed
                 , ResourcesAvailable )
import Buildings ( building, BuildingInfo(..), BLevel(..) )
import Model

instance Arbitrary (RawResource t) where
    arbitrary = do
        aValue <- arbitrary `suchThat` \x -> x >= 0
        return $ RawResource aValue

instance Arbitrary (RawResources t) where
    arbitrary = do
        aBio <- arbitrary
        aMech <- arbitrary
        aChem <- arbitrary
        return $ RawResources { ccdMechanicalCost = aMech
                              , ccdBiologicalCost = aBio
                              , ccdChemicalCost = aChem
                              }

rawResourcesThatAre :: (Int -> Int -> Bool) -> RawResources a -> Gen (RawResources b)
rawResourcesThatAre check base = do
    aBio <- arbitrary `suchThat` \x -> x `check` (unRawResource . ccdBiologicalCost) base
    aMech <- arbitrary `suchThat` \x -> x `check` (unRawResource . ccdMechanicalCost) base
    aChem <- arbitrary `suchThat` \x -> x `check` (unRawResource . ccdChemicalCost) base
    return $ RawResources { ccdMechanicalCost = RawResource aMech
                          , ccdBiologicalCost = RawResource aBio
                          , ccdChemicalCost = RawResource aChem
                          }
                          
instance Arbitrary BuildingType where
    arbitrary = oneof [ return SensorStation
                      , return ResearchComplex
                      , return Farm
                      , return ParticleAccelerator
                      , return NeutronDetector
                      , return BlackMatterScanner
                      , return GravityWaveSensor
                      ]

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
    aConstructionSpeed <- arbitrary
    aBuildingConstruction <- unstartedConstruction
    aTotalCost <- arbitrary
    return (aConstructionSpeed, aBuildingConstruction, aTotalCost)

unfinishedConstructionsWithSomeSpeed :: Gen (RawResources ConstructionSpeed, BuildingConstruction, RawResources ResourceCost)
unfinishedConstructionsWithSomeSpeed = do
    aConstructionSpeed <- arbitrary
    aBuildingConstruction <- randomConstruction
    aTotalCost <- arbitrary
    return (aConstructionSpeed, aBuildingConstruction, aTotalCost)

resourceCostAndEnoughAvailableResources :: Gen (RawResources ResourceCost, RawResources ResourcesAvailable)
resourceCostAndEnoughAvailableResources = do
    aCost <- arbitrary
    aAvailable <- rawResourcesThatAre (>) aCost
    return (aCost, aAvailable)

resourceCostAndLimitedResources :: Gen (RawResources ResourceCost, RawResources ResourcesAvailable)
resourceCostAndLimitedResources = do
    aCost <- arbitrary
    aAvailable <- rawResourcesThatAre (<) aCost
    return (aCost, aAvailable)
