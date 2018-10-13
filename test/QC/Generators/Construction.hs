{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.Construction where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()
import QC.Generators.Database ( randomPlanetKey )

import CustomTypes ( RawResources(..), RawResource(..), ResourceCost, ConstructionSpeed, BuildingType(..) )
import Model

-- | Limit construction speed to amount that there's work left to do
--    speedLimitedByWorkLeft :: RawResources ConstructionSpeed -> BuildingConstruction -> RawResources ResourceCost -> RawResources ConstructionSpeed
--    speedLimitedByWorkLeft cSpeed bConst cost =

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

unstartedConstructionsWithSomeSpeed :: Gen (RawResources ConstructionSpeed, BuildingConstruction, RawResources ResourceCost)
unstartedConstructionsWithSomeSpeed = do
    aConstructionSpeed <- rawResources
    aBuildingConstruction <- unstartedConstruction
    aTotalCost <- rawResources
    return (aConstructionSpeed, aBuildingConstruction, aTotalCost)
