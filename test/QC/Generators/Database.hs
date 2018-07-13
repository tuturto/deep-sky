{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.Database where

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