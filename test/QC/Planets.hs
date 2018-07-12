{-# LANGUAGE TemplateHaskell #-}
module QC.Planets where

import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Gen
import Test.QuickCheck
import Test.QuickCheck.Instances.Text

import Database.Persist.Sql (toSqlKey)
import Model
import Simulation.Main (pFoodRequirement)

instance Arbitrary Planet where
    arbitrary = do
        aName <- arbitrary
        aPosition <- arbitrary
        let aStarSystemId = toSqlKey 0
        let aOwnerId = Nothing
        aGravity <- arbitrary
        return $ Planet aName (getPositive aPosition) aStarSystemId aOwnerId (getPositive aGravity)

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

prop_foodRequirement_for_positive_amount_of_population_is_more_than_zero :: Property
prop_foodRequirement_for_positive_amount_of_population_is_more_than_zero  
    = forAll positivePopulation $ \res -> pFoodRequirement res  > 0

prop_foodRequirement_for_all_populations_is_non_negative :: Property
prop_foodRequirement_for_all_populations_is_non_negative  
    = forAll allPopulations $ \res -> pFoodRequirement res  >= 0

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll