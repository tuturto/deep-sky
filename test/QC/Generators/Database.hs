{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.Database where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Database.Persist.Sql
import Model

randomStarKey :: Gen StarId
randomStarKey = do
    aId <- arbitrary
    return $ toSqlKey aId


randomStarSystemKey :: Gen StarSystemId
randomStarSystemKey = do
    aId <- arbitrary
    return $ toSqlKey aId


randomStarLaneKey :: Gen StarLaneId
randomStarLaneKey = do
    aId <- arbitrary
    return $ toSqlKey aId


randomFactionKey :: Gen FactionId
randomFactionKey = do
    aId <- arbitrary
    return $ toSqlKey aId


randomPlanetKey :: Gen PlanetId
randomPlanetKey = do
    aId <- arbitrary
    return $ toSqlKey aId

randomUserKey :: Gen UserId
randomUserKey = do
    aId <- arbitrary
    return $ toSqlKey aId


randomDesignKey :: Gen DesignId
randomDesignKey = do
    aId <- arbitrary
    return $ toSqlKey aId


randomPersonKey :: Gen PersonId
randomPersonKey = do
    aId <- arbitrary
    return $ toSqlKey aId
