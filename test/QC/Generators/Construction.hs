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


newtype ArbRawResource t =
    ArbRawResource { unArbRawResource :: RawResource t }


instance Arbitrary (ArbRawResource t) where
    arbitrary = do
        aValue <- arbitrary `suchThat` \x -> x >= 0
        return $ ArbRawResource $ RawResource aValue


newtype ArbRawResources t =
    ArbRawResources { unArbRawResources :: RawResources t }


instance Arbitrary (ArbRawResources t) where
    arbitrary = do
        aBio <- arbitrary
        aMech <- arbitrary
        aChem <- arbitrary
        return $ ArbRawResources $ RawResources { ccdMechanicalCost = unArbRawResource aMech
                                                , ccdBiologicalCost = unArbRawResource aBio
                                                , ccdChemicalCost = unArbRawResource aChem
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


newtype ArbBuildingType =
    ArbBuildingType { unArbBuildingType :: BuildingType }


instance Arbitrary ArbBuildingType where
    arbitrary = oneof [ return $ ArbBuildingType SensorStation
                      , return $ ArbBuildingType ResearchComplex
                      , return $ ArbBuildingType Farm
                      , return $ ArbBuildingType ParticleAccelerator
                      , return $ ArbBuildingType NeutronDetector
                      , return $ ArbBuildingType BlackMatterScanner
                      , return $ ArbBuildingType GravityWaveSensor
                      ]


unstartedConstruction :: Gen BuildingConstruction
unstartedConstruction = do
    aPlanetId <- randomPlanetKey
    arbBuildingType <- arbitrary
    let aBuildingType = unArbBuildingType arbBuildingType
    return $ BuildingConstruction aPlanetId 0 0 0 0 aBuildingType 1


randomConstruction :: Gen BuildingConstruction
randomConstruction = do
    aPlanetId <- randomPlanetKey
    arbBuildingType <- arbitrary
    let aBuildingType = unArbBuildingType arbBuildingType
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
    return (unArbRawResources aConstructionSpeed, aBuildingConstruction, unArbRawResources aTotalCost)


unfinishedConstructionsWithSomeSpeed :: Gen (RawResources ConstructionSpeed, BuildingConstruction, RawResources ResourceCost)
unfinishedConstructionsWithSomeSpeed = do
    arbConstructionSpeed <- arbitrary
    let aConstructionSpeed = unArbRawResources arbConstructionSpeed
    aBuildingConstruction <- randomConstruction
    arbTotalCost <- arbitrary
    let aTotalCost = unArbRawResources arbTotalCost
    return (aConstructionSpeed, aBuildingConstruction, aTotalCost)


resourceCostAndEnoughAvailableResources :: Gen (RawResources ResourceCost, RawResources ResourcesAvailable)
resourceCostAndEnoughAvailableResources = do
    arbCost <- arbitrary
    let aCost = unArbRawResources arbCost
    aAvailable <- rawResourcesThatAre (>) aCost
    return (aCost, aAvailable)


resourceCostAndLimitedResources :: Gen (RawResources ResourceCost, RawResources ResourcesAvailable)
resourceCostAndLimitedResources = do
    arbCost <- arbitrary
    let aCost = unArbRawResources arbCost
    aAvailable <- rawResourcesThatAre (<) aCost
    return (aCost, aAvailable)
