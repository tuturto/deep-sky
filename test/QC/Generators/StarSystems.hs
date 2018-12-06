{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.StarSystems where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()

import Database.Persist.Sql
import Model
import Report
import CustomTypes
import QC.Generators.Common
import QC.Generators.Database


newtype ArbLuminosityClass = ArbLuminosityClass
    { unArbLuminosityClass :: LuminosityClass }


instance Arbitrary ArbLuminosityClass where
    arbitrary = oneof [ return $ ArbLuminosityClass Iap
                      , return $ ArbLuminosityClass Ia
                      , return $ ArbLuminosityClass Iab
                      , return $ ArbLuminosityClass Ib
                      , return $ ArbLuminosityClass II
                      , return $ ArbLuminosityClass III
                      , return $ ArbLuminosityClass IV
                      , return $ ArbLuminosityClass V
                      , return $ ArbLuminosityClass VI
                      , return $ ArbLuminosityClass VII ]


newtype ArbSpectralType = ArbSpectralType
    { unArbSpectralType :: SpectralType }


instance Arbitrary ArbSpectralType where
    arbitrary = oneof [ return $ ArbSpectralType O
                      , return $ ArbSpectralType B
                      , return $ ArbSpectralType A
                      , return $ ArbSpectralType F
                      , return $ ArbSpectralType G
                      , return $ ArbSpectralType K
                      , return $ ArbSpectralType M
                      , return $ ArbSpectralType L
                      , return $ ArbSpectralType T ]


singleStar :: Gen Star
singleStar = do
    aName <- arbitrary
    aStarSystemId <- randomStarSystemKey
    aSpectralType <- arbitrary
    aLuminosityClass <- arbitrary
    return $ Star aName aStarSystemId (unArbSpectralType aSpectralType) (unArbLuminosityClass aLuminosityClass)


singleStarEntity :: Gen (Entity Star)
singleStarEntity = do
    aStar <- singleStar
    aId <- randomStarKey
    return $ Entity aId aStar


singleStarReport :: Gen CollatedStarReport
singleStarReport = do
    aStarId <- randomStarKey
    aStarSystemId <- randomStarSystemKey
    aName <- perhaps arbitrary
    aSpectralType <- perhaps arbitrary
    aLuminosityClass <- perhaps arbitrary
    aDate <- arbitrary `suchThat` \x -> x > 0
    return $ CollatedStarReport aStarId aStarSystemId aName (fmap unArbSpectralType aSpectralType)
                                (fmap unArbLuminosityClass aLuminosityClass) aDate


allStars :: Gen [Star]
allStars = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k singleStar


starEntities :: Gen [(Entity Star)]
starEntities = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k singleStarEntity


starReports :: Gen [CollatedStarReport]
starReports = do
    k <- arbitrary `suchThat` \x -> x >= 0
    vectorOf k singleStarReport


starsAndReports :: Gen ([Entity Star], [CollatedStarReport])
starsAndReports = do
    aStars <- starEntities
    aReports <- starReports
    return $ (aStars, aReports)


singleStarLane :: Gen StarLane
singleStarLane = do
    aSystem1 <- randomStarSystemKey
    aSystem2 <- randomStarSystemKey
    return $ StarLane aSystem1 aSystem2


singleStarLaneEntity :: Gen (Entity StarLane)
singleStarLaneEntity = do
    aStarLane <- singleStarLane
    aId <- randomStarLaneKey
    return $ Entity aId aStarLane


singleStarLaneReport :: Gen CollatedStarLaneReport
singleStarLaneReport = do
    aLaneId <- randomStarLaneKey
    aSystem1 <- randomStarSystemKey
    aSystem2 <- randomStarSystemKey
    aSystemName1 <- perhaps arbitrary
    aSystemName2 <- perhaps arbitrary
    aDate <- arbitrary `suchThat` \x -> x > 0
    return $ CollatedStarLaneReport aLaneId aSystem1 aSystem2 aSystemName1 aSystemName2 aDate


starLaneEntities :: Gen [(Entity StarLane)]
starLaneEntities = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k singleStarLaneEntity


starLaneReports :: Gen [CollatedStarLaneReport]
starLaneReports = do
    k <- arbitrary `suchThat` \x -> x >= 0
    vectorOf k singleStarLaneReport


starLanesAndReports :: Gen ([Entity StarLane], [CollatedStarLaneReport])
starLanesAndReports = do
    aStarLanes <- starLaneEntities
    aReports <- starLaneReports
    return $ (aStarLanes, aReports)
