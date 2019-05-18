{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.Planets where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()

import Database.Persist.Sql
import Model
import Report
import CustomTypes
import QC.Generators.Common
import QC.Generators.Database


singlePlanet :: Gen Planet
singlePlanet = do
    aName <- arbitrary
    aPosition <- arbitrary `suchThat` \x -> x > 0
    aStarSystemId <- randomStarSystemKey
    aOwnerId <- perhaps randomFactionKey
    aGravity <- arbitrary `suchThat` \x -> x > 0
    return $ Planet aName aPosition aStarSystemId aOwnerId aGravity

singlePlanetEntity :: Gen (Entity Planet)
singlePlanetEntity = do
    aPlanet <- singlePlanet
    aId <- randomPlanetKey
    return $ Entity aId aPlanet

singlePlanetReport :: Gen CollatedPlanetReport
singlePlanetReport = do
    aPlanetId <- randomPlanetKey
    aOwnerId <- perhaps randomFactionKey
    aStarSystemId <- randomStarSystemKey
    aName <- perhaps arbitrary
    aPosition <- perhaps $ arbitrary `suchThat` \x -> x > 0
    aGravity <- perhaps $ arbitrary `suchThat` \x -> x > 0
    aDate <- arbitrary `suchThat` \x -> x > 0
    return $ CollatedPlanetReport aPlanetId aStarSystemId aOwnerId aName aPosition
                                  aGravity (unArbStarDate aDate)

allPlanets :: Gen [Planet]
allPlanets = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k singlePlanet

planetEntities :: Gen [(Entity Planet)]
planetEntities = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k singlePlanetEntity

planetReports :: Gen [CollatedPlanetReport]
planetReports = do
    k <- arbitrary `suchThat` \x -> x >= 0
    vectorOf k singlePlanetReport

planetsAndReports :: Gen ([Entity Planet], [CollatedPlanetReport])
planetsAndReports = do
    aPlanets <- planetEntities
    aReports <- planetReports
    return $ (aPlanets, aReports)

singlePopulation :: Gen PlanetPopulation
singlePopulation = do
    let aPlanetId = toSqlKey 0
    let aRaceId = toSqlKey 0
    aPopulation <- arbitrary `suchThat` \x -> x > 0
    return $ PlanetPopulation aPlanetId aRaceId aPopulation

positivePopulation :: Gen [PlanetPopulation]
positivePopulation = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k singlePopulation

allPopulations :: Gen [PlanetPopulation]
allPopulations = do
    k <- arbitrary `suchThat` \x -> x >= 0
    vectorOf k singlePopulation

singleBuildingOf :: BuildingType -> Gen Building
singleBuildingOf bType = do
    let aPlanetId = toSqlKey 0
    aLevel <- choose (1, 5)
    let aDamage = 0.0
    return $ Building aPlanetId bType aLevel aDamage

someFarms :: Gen [Building]
someFarms = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k $ singleBuildingOf Farm
