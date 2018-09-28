{-# LANGUAGE TemplateHaskell            #-}
module QC.PlanetsSpec (spec) 
    where

import Test.QuickCheck
import Test.Hspec

import Simulation.Food (foodRequirement, foodProduction)
import QC.Generators.Import

spec :: Spec
spec = do
    describe "planets" $ do
        describe "food" $ do
            it "food requirement for positive amount of population is more than zero" $ do
                forAll positivePopulation $ \x -> foodRequirement x  > 0

            it "food requirement for all populations is non negative" $ do
                forAll allPopulations $ \x -> foodRequirement x  >= 0

            it "food production for farms is non negative" $ do
                forAll someFarms $ \x -> foodProduction x > 0
        
            it "food production for farms is equal or greater than their amount" $ do
                forAll someFarms $ \x -> foodProduction x > length x
