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
import Space.Data ( StarName(..), StarSystemName(..) )


anyLuminosityClass :: Gen LuminosityClass
anyLuminosityClass = elements [minBound..]


anySpectralType :: Gen SpectralType
anySpectralType = elements [minBound..]


singleStar :: Gen Star
singleStar = do
    aName <- arbitrary
    aStarSystemId <- randomStarSystemKey
    aSpectralType <- anySpectralType
    aLuminosityClass <- anyLuminosityClass
    return $ Star (MkStarName aName) aStarSystemId aSpectralType aLuminosityClass


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
    aSpectralType <- perhaps anySpectralType
    aLuminosityClass <- perhaps anyLuminosityClass
    aDate <- arbitrary `suchThat` \x -> x > 0
    return $ CollatedStarReport aStarId aStarSystemId (MkStarName <$> aName)
                                aSpectralType aLuminosityClass
                                (unArbStarDate aDate)


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
    return $ CollatedStarLaneReport aLaneId aSystem1 aSystem2
                                    (MkStarSystemName <$> aSystemName1)
                                    (MkStarSystemName <$> aSystemName2)
                                    (unArbStarDate aDate)


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
