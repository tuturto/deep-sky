{-# LANGUAGE TemplateHaskell            #-}

module QC.Generators.Reports where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()

import Data.Maybe (isJust, isNothing)

import Database.Persist.Sql
import Model
import Report
import QC.Generators.Common ( ArbStarDate(..) )
import QC.Generators.StarSystems
import QC.Generators.Planets
import QC.Generators.Database


-- | generator for list of unobserved star reports
-- all these stars either have their report missing or only partially filled in
unobservedStarList :: Gen [(Entity Star, Maybe CollatedStarReport)]
unobservedStarList = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k unobservedStar


-- | generator for pair of star and respective report
-- report is either missing completely or partially filled in
unobservedStar :: Gen (Entity Star, Maybe CollatedStarReport)
unobservedStar = do
    star <- singleStarEntity
    report <- oneof [ missingStarReport
                    , (starReport star) `suchThat` starReportIsPartiallyFilled ]
    return (star, report)


-- | generator for creating potentially partially filled star reports for given star
-- Generator will always return Just CollatedStarReport
starReport :: Entity Star -> Gen (Maybe CollatedStarReport)
starReport entity = do
    let star = entityVal entity
    let aStarId = entityKey entity
    let aStarSystemId = starStarSystemId star
    aName <- oneof [ return Nothing
                   , return $ Just $ starName star ]
    aSpectralType <- oneof [ return Nothing
                           , return $ Just $ starSpectralType star ]
    aLuminosityClass <- oneof [ return Nothing
                              , return $ Just $ starLuminosityClass star ]
    aDate <- arbitrary `suchThat` \x -> x > 18000
    return $ Just $ CollatedStarReport aStarId aStarSystemId aName aSpectralType aLuminosityClass (unArbStarDate aDate)


-- | generator to simply return Nothing
missingStarReport :: Gen (Maybe CollatedStarReport)
missingStarReport = do
    return Nothing


-- | check if given collated star report is partially filled
-- reports that have one of their Maybe fields entered as Nothing are considered partial
starReportIsPartiallyFilled :: (Maybe CollatedStarReport) -> Bool
starReportIsPartiallyFilled (Just report) =
    isNothing (csrName report) ||
        isNothing (csrSpectralType report) ||
        isNothing (csrLuminosityClass report)
starReportIsPartiallyFilled Nothing =
    False


-- | generator for list of unobserved planet reports
-- all these stars either have their report missing or only partially filled in
unobservedPlanetList :: Gen [(Entity Planet, Maybe CollatedPlanetReport)]
unobservedPlanetList = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k unobservedPlanet


-- | generator for list of unobserved planet reports
-- all these stars either have their report partially filled in
partiallyObservedPlanetList :: Gen [(Entity Planet, Maybe CollatedPlanetReport)]
partiallyObservedPlanetList = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k partiallyObservedPlanet


-- | generator for list of fully observed planet reports
-- all these stars either have their reports fully filled in
observedPlanetList :: Gen [(Entity Planet, Maybe CollatedPlanetReport)]
observedPlanetList = do
    k <- arbitrary `suchThat` \x -> x > 0
    vectorOf k observedPlanet


-- | generator for pair of planet and respective report
-- report is either missing completely or partially filled in
unobservedPlanet :: Gen (Entity Planet, Maybe CollatedPlanetReport)
unobservedPlanet = do
    planet <- singlePlanetEntity
    report <- oneof [ missingPlanetReport
                    , (planetReport planet) `suchThat` (planetReportIsPartiallyFilled planet) ]
    return (planet, report)


-- | generator for pair of planet and respective report
-- report is partially filled in
partiallyObservedPlanet :: Gen (Entity Planet, Maybe CollatedPlanetReport)
partiallyObservedPlanet = do
    planet <- singlePlanetEntity
    report <- (planetReport planet) `suchThat` (planetReportIsPartiallyFilled planet)
    return (planet, report)


-- | generator for pair of planet and respective report
-- report is fully filled in
observedPlanet :: Gen (Entity Planet, Maybe CollatedPlanetReport)
observedPlanet = do
    planet <- singlePlanetEntity
    report <- (fullPlanetReport planet)
    return (planet, report)


-- | generator for creating potentially partially filled planet reports for given planet
-- Generator will always return Just CollatedPlanetReport
planetReport :: Entity Planet -> Gen (Maybe CollatedPlanetReport)
planetReport entity = do
    let planet = entityVal entity
    let aPlanetId = entityKey entity
    let aStarSystemId = planetStarSystemId planet
    ownerId <- randomFactionKey
    aOwnerId <- oneof [ return Nothing
                      , return $ Just ownerId ]
    aName <- oneof [ return Nothing
                   , return $ Just $ planetName planet ]
    aPosition <- oneof [ return Nothing
                       , return $ Just $ planetPosition planet ]
    aGravity <- oneof [ return Nothing
                      , return $ Just $ planetGravity planet ]
    aDate <- arbitrary `suchThat` \x -> x > 18000
    return $ Just $ CollatedPlanetReport aPlanetId aStarSystemId aOwnerId aName aPosition aGravity (unArbStarDate aDate) Nothing Nothing


-- | generator that creates fully filled in planet report from a planet
fullPlanetReport :: Entity Planet -> Gen (Maybe CollatedPlanetReport)
fullPlanetReport entity = do
    let planet = entityVal entity
    let aPlanetId = entityKey entity
    let aStarSystemId = planetStarSystemId planet
    let aOwnerId = planetOwnerId planet
    let aName = Just $ planetName planet
    let aPosition = Just $ planetPosition planet
    let aGravity = Just $ planetGravity planet
    aDate <- arbitrary `suchThat` \x -> x > 18000
    return $ Just $ CollatedPlanetReport aPlanetId aStarSystemId aOwnerId aName aPosition aGravity (unArbStarDate aDate) Nothing Nothing


-- | generator to simply return Nothing
missingPlanetReport :: Gen (Maybe CollatedPlanetReport)
missingPlanetReport = do
    return Nothing


-- | check if given collated planet report is partially filled
-- reports that have one of their Maybe fields entered as Nothing are considered partial
-- special case of not being partially filled for planets is when nobody owns it and report
-- states that correctly
planetReportIsPartiallyFilled :: (Entity Planet) -> (Maybe CollatedPlanetReport) -> Bool
planetReportIsPartiallyFilled (Entity _ planet) (Just report) =
    (isNothing (cprOwnerId report) && isJust (planetOwnerId planet))
    || (isJust (cprOwnerId report) && isNothing (planetOwnerId planet))
    || isNothing (cprName report)
    || isNothing (cprPosition report)
    || isNothing (cprGravity report)

planetReportIsPartiallyFilled _ Nothing =
    False
