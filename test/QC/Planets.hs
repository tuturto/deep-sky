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
import Simulation.Main (foodRequirement)

instance Arbitrary Planet where
    arbitrary = do
        aName <- arbitrary
        aPosition <- arbitrary
        let aStarSystemId = toSqlKey 0
        let aOwnerId = Nothing
        aGravity <- arbitrary
        return $ Planet aName (getPositive aPosition) aStarSystemId aOwnerId (getPositive aGravity)

prop_foodRequirementZeroOrPositive :: [Planet] -> Bool
prop_foodRequirementZeroOrPositive planets = foodRequirement planets >= 0

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll