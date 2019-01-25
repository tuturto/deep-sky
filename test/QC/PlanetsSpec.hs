{-# LANGUAGE TemplateHaskell            #-}
module QC.PlanetsSpec (spec)
    where

import Test.QuickCheck
import Test.Hspec

import Resources ( RawResource(..) )
import Simulation.Food  (foodRequirement, foodBaseProduction )

import QC.Generators.Import

spec :: Spec
spec = do
    describe "planets" $ do
        describe "food" $ do
            it "food requirement for positive amount of population is more than zero" $ do
                forAll positivePopulation $ \x -> foodRequirement x  > RawResource 0

            it "food requirement for all populations is non negative" $ do
                forAll allPopulations $ \x -> foodRequirement x  >= RawResource 0

            it "food base production for farms is non negative" $ do
                forAll someFarms $ \x -> (sum (fmap foodBaseProduction x)) > RawResource 0

            it "food base production for farms is equal or greater than their amount" $ do
                forAll someFarms $ \x -> (sum (fmap foodBaseProduction x)) > (RawResource $ length x)
