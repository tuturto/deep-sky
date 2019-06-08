{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.Database where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Database.Persist.Sql
import Model

randomStarKey :: Gen (Key Star)
randomStarKey = do
    aId <- arbitrary
    return $ toSqlKey aId


randomStarSystemKey :: Gen (Key StarSystem)
randomStarSystemKey = do
    aId <- arbitrary
    return $ toSqlKey aId


randomStarLaneKey :: Gen (Key StarLane)
randomStarLaneKey = do
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


randomDesignKey :: Gen (Key Design)
randomDesignKey = do
    aId <- arbitrary
    return $ toSqlKey aId


randomPersonKey :: Gen (Key Person)
randomPersonKey = do
    aId <- arbitrary
    return $ toSqlKey aId
