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

instance Arbitrary LuminosityClass where
    arbitrary = oneof [ return Iap
                      , return Ia 
                      , return Iab 
                      , return Ib 
                      , return II 
                      , return III 
                      , return IV 
                      , return V
                      , return VI
                      , return VII ]

instance Arbitrary SpectralType where
    arbitrary = oneof [ return O
                      , return B 
                      , return A 
                      , return F 
                      , return G
                      , return K
                      , return M
                      , return L
                      , return T ]

singleStar :: Gen Star
singleStar = do
    aName <- arbitrary
    aStarSystemId <- randomStarSystemKey
    aSpectralType <- arbitrary
    aLuminosityClass <- arbitrary
    return $ Star aName aStarSystemId aSpectralType aLuminosityClass
    
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
    return $ CollatedStarReport aStarId aStarSystemId aName aSpectralType
                                aLuminosityClass aDate

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
