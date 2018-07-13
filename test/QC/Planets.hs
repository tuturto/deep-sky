{-# LANGUAGE TemplateHaskell            #-}
module QC.Planets where

import Test.QuickCheck.All
import Test.QuickCheck

import Simulation.Food (foodRequirement, foodProduction)
import QC.Generators.Import

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