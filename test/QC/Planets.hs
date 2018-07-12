{-# LANGUAGE TemplateHaskell #-}
module QC.Planets where

import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck

import Database.Persist.Sql (toSqlKey)
import Model
import CustomTypes
import Simulation.Food (foodRequirement, foodProduction)

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
    let aConstruction = 1.0
    let aDamage = 0.0
    return $ Building aPlanetId bType aLevel aConstruction aDamage

someFarms :: Gen [Building]
someFarms = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k $ singleBuildingOf Farm

prop_foodRequirement_for_positive_amount_of_population_is_more_than_zero :: Property
prop_foodRequirement_for_positive_amount_of_population_is_more_than_zero  
    = forAll positivePopulation $ \x -> foodRequirement x  > 0

prop_foodRequirement_for_all_populations_is_non_negative :: Property
prop_foodRequirement_for_all_populations_is_non_negative  
    = forAll allPopulations $ \x -> foodRequirement x  >= 0

prop_foodProduction_for_farms_is_non_negative :: Property
prop_foodProduction_for_farms_is_non_negative
    = forAll someFarms $ \x -> foodProduction x > 0

prop_food_production_for_farms_is_equal_or_greater_than_their_amount :: Property
prop_food_production_for_farms_is_equal_or_greater_than_their_amount
    = forAll someFarms $ \x -> foodProduction x > length x

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll