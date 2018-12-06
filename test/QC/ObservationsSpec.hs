{-# LANGUAGE TemplateHaskell            #-}

module QC.ObservationsSpec (spec)
    where

import Prelude
import Data.List (find)
import Data.Maybe (isJust)

import Test.QuickCheck
import Test.Hspec

import Database.Persist.Sql
import Model
import Simulation.Observations ( groupPlanetReports, groupStarReports, groupStarLaneReports
                               , buildOCStarList, buildOCPlanetList, ObservationCandidate(..)
                               , ObservationType(..) )
import Report

import QC.Generators.Import

planetIsInGroupedReport :: [(Entity Planet, Maybe CollatedPlanetReport)] -> Entity Planet -> Bool
planetIsInGroupedReport report planet =
    isJust $ find compareIds report
    where
        compareIds p = (entityKey planet) == (entityKey $ fst p)

starIsInGroupedReport :: [(Entity Star, Maybe CollatedStarReport)] -> Entity Star -> Bool
starIsInGroupedReport report star =
    isJust $ find compareIds report
    where
        compareIds p = (entityKey star) == (entityKey $ fst p)

starLaneIsInGroupedReport :: [(Entity StarLane, Maybe CollatedStarLaneReport)] -> Entity StarLane -> Bool
starLaneIsInGroupedReport report starlane =
    isJust $ find compareIds report
    where
        compareIds p = (entityKey starlane) == (entityKey $ fst p)

starIsInCandidateList :: [ObservationCandidate] -> (Entity Star, Maybe CollatedStarReport) -> Bool
starIsInCandidateList candidates (Entity starId _, _) =
    isJust $ find compareIds candidates
    where
        compareIds (OCStar ocStar _ _) = (entityKey ocStar) == starId
        compareIds _ = False


planetIsInCandidateList :: [ObservationCandidate] -> (Entity Planet, Maybe CollatedPlanetReport) -> Bool
planetIsInCandidateList candidates (Entity planetId _, _) =
    isJust $ find compareIds candidates
    where
        compareIds (OCPlanet ocPlanet _ _) = (entityKey ocPlanet) == planetId
        compareIds _ = False


candidateIsKnown :: ObservationCandidate -> Bool
candidateIsKnown (OCStar _ _ UpdatedObservation) = True
candidateIsKnown (OCPlanet _ _ UpdatedObservation) = True
candidateIsKnown (OCStar _ _ NewObservation) = False
candidateIsKnown (OCPlanet _ _ NewObservation) = False


starAndReportMatch :: (Entity Star, Maybe CollatedStarReport) -> Bool
starAndReportMatch (star, (Just report)) = (entityKey star) == (csrStarId report)
starAndReportMatch _ = True


planetAndReportMatch :: (Entity Planet, Maybe CollatedPlanetReport) -> Bool
planetAndReportMatch (planet, (Just report)) = (entityKey planet) == (cprPlanetId report)
planetAndReportMatch _ = True


spec :: Spec
spec = do
    describe "observations" $ do
        describe "starlanes" $ do
            it "grouped starlane report list is as long as starlanes list" $ do
                forAll starLanesAndReports $ \(lanes, reports)
                    -> length lanes == (length $ groupStarLaneReports lanes reports)

            it "every starlane is present in grouped starlanes report list" $ do
                forAll starLanesAndReports $ \(lanes, reports)
                    -> all (starLaneIsInGroupedReport $ groupStarLaneReports lanes reports) lanes

        describe "stars" $ do
            it "stars and their reports are grouped by ids" $ do
                forAll starsAndReports $ \(stars, reports)
                    -> all starAndReportMatch (groupStarReports stars reports)

            it "grouped star report list is as long as stars list" $ do
                forAll starsAndReports $ \(stars, reports)
                    -> length stars == (length $ groupStarReports stars reports)

            it "every star is present in grouped star report list" $ do
                forAll starsAndReports $ \(stars, reports)
                    -> all (starIsInGroupedReport $ groupStarReports stars reports) stars

            it "star list is as long as needs observation list" $ do
                forAll unobservedStarList $ \entities
                    -> length entities == (length $ buildOCStarList entities)

            it "star list contains items needing observation" $ do
                forAll unobservedStarList $ \entities
                    -> all (starIsInCandidateList $ buildOCStarList entities) entities

        describe "planets" $ do
            it "planets and their reports are grouped by ids" $ do
                forAll planetsAndReports $ \(planets, reports)
                    -> all planetAndReportMatch (groupPlanetReports planets reports)

            it "grouped planet report list is as long as planets list" $ do
                forAll planetsAndReports $ \(planets, reports)
                    -> length planets == (length $ groupPlanetReports planets reports)

            it "every planet is present in grouped planet report list" $ do
                forAll planetsAndReports $ \(planets, reports)
                    -> all (planetIsInGroupedReport $ groupPlanetReports planets reports) planets

            it "planet list is as long as needs observation list" $ do
                forAll unobservedPlanetList $ \entities
                    -> length entities == (length $ buildOCPlanetList entities)

            it "planet list contains items needing observation" $ do
                forAll unobservedPlanetList $ \entities
                    -> all (planetIsInCandidateList $ buildOCPlanetList entities) entities

            it "fully observed planet is not considered a candidate" $ do
                forAll observedPlanetList $ \entities
                    -> not $ any (planetIsInCandidateList $ buildOCPlanetList entities) entities

            it "planet already observed is not considered a new planet" $ do
                forAll partiallyObservedPlanetList $ \entities
                    -> all ((==) True) $ map candidateIsKnown $ buildOCPlanetList entities


-- needsObservation
--  fully observed don't need observation
--  not observed needs observation

--- needsObservation :: ObservationCandidate -> Bool

--- data ObservationCandidate = OCStar (Entity Star) (Maybe CollatedStarReport)
--- | OCPlanet (Entity Planet) (Maybe CollatedPlanetReport)
--- | OCStarLane (Entity StarLane) (Maybe CollatedStarLaneReport)
