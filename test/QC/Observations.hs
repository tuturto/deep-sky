{-# LANGUAGE TemplateHaskell            #-}

module QC.Observations where

import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck
import Test.QuickCheck.Instances

import Database.Persist.Sql
import Model
import CustomTypes
import Simulation.Observations (groupPlanetReports)
import Report
import Data.List (find)

randomStarSystemKey :: Gen (Key StarSystem)
randomStarSystemKey = do
    aId <- arbitrary
    return $ toSqlKey aId

randomFactionKey :: Gen (Key Faction)
randomFactionKey = do
    aId <- arbitrary
    return $ toSqlKey aId

randomPlanetKey :: Gen (Key Planet)
randomPlanetKey = do
    aId <- arbitrary
    return $ toSqlKey aId

randomUserKey :: Gen (Key User)
randomUserKey = do
    aId <- arbitrary
    return $ toSqlKey aId

perhaps :: Gen a -> Gen (Maybe a)
perhaps a = do
    res <- a
    oneof [return Nothing, return $ Just res]

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
    aOwnerId <- perhaps randomUserKey
    aStarSystemId <- randomStarSystemKey
    aName <- perhaps arbitrary
    aPosition <- perhaps $ arbitrary `suchThat` \x -> x > 0
    aGravity <- perhaps $ arbitrary `suchThat` \x -> x > 0
    aDate <- arbitrary `suchThat` \x -> x > 0
    return $ CollatedPlanetReport aPlanetId aStarSystemId aOwnerId aName aPosition
                                  aGravity aDate

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

planetIsInGroupedReport :: [(Entity Planet, Maybe CollatedPlanetReport)] -> Entity Planet -> Bool
planetIsInGroupedReport report planet = 
    case wasFound of
        (Just _) -> True
        Nothing  -> False
    where
        wasFound = find (\p -> (entityKey planet) == (entityKey $ fst p)) report


prop_planets_and_their_reports_are_grouped_by_ids :: Property
prop_planets_and_their_reports_are_grouped_by_ids = 
    forAll planetsAndReports $ \(planets, reports) 
        -> all fn (groupPlanetReports planets reports)
            where fn (planet, (Just report)) = (entityKey planet) == (cprPlanetId report)
                  fn _ = True

prop_grouped_planet_report_list_is_as_long_as_planets_list :: Property
prop_grouped_planet_report_list_is_as_long_as_planets_list = 
    forAll planetsAndReports $ \(planets, reports) 
        -> length planets == (length $ groupPlanetReports planets reports)

prop_every_planet_is_present_in_grouped_planet_report_list :: Property
prop_every_planet_is_present_in_grouped_planet_report_list =
    forAll planetsAndReports $ 
        \(planets, reports) -> all (planetIsInGroupedReport $ groupPlanetReports planets reports) planets

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll