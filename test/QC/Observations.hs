{-# LANGUAGE TemplateHaskell            #-}

module QC.Observations where

import Prelude
import Data.List (find)
import Data.Maybe (isJust)

import Test.QuickCheck.All
import Test.QuickCheck

import Database.Persist.Sql
import Model
import Simulation.Observations (groupPlanetReports, groupStarReports, groupStarLaneReports,
                                buildOCStarList, buildOCPlanetList, ObservationCandidate(..))
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

prop_grouped_starlane_report_list_is_as_long_as_starlanes_list :: Property
prop_grouped_starlane_report_list_is_as_long_as_starlanes_list = 
    forAll starLanesAndReports $ \(lanes, reports) 
        -> length lanes == (length $ groupStarLaneReports lanes reports)

prop_every_starlane_is_present_in_grouped_starlanes_report_list :: Property
prop_every_starlane_is_present_in_grouped_starlanes_report_list =
    forAll starLanesAndReports $ \(lanes, reports) 
        -> all (starLaneIsInGroupedReport $ groupStarLaneReports lanes reports) lanes

prop_stars_and_their_reports_are_grouped_by_ids :: Property
prop_stars_and_their_reports_are_grouped_by_ids = 
    forAll starsAndReports $ \(stars, reports) 
        -> all fn (groupStarReports stars reports)
            where fn (star, (Just report)) = (entityKey star) == (csrStarId report)
                  fn _ = True
        
prop_grouped_star_report_list_is_as_long_as_stars_list :: Property
prop_grouped_star_report_list_is_as_long_as_stars_list = 
    forAll starsAndReports $ \(stars, reports) 
        -> length stars == (length $ groupStarReports stars reports)
        
prop_every_star_is_present_in_grouped_star_report_list :: Property
prop_every_star_is_present_in_grouped_star_report_list =
    forAll starsAndReports $ \(stars, reports) 
        -> all (starIsInGroupedReport $ groupStarReports stars reports) stars

prop_planets_and_their_reports_are_grouped_by_ids :: Property
prop_planets_and_their_reports_are_grouped_by_ids = 
    forAll planetsAndReports $ \(planets, reports) 
        -> all fn (groupPlanetReports planets reports)
            where fn (planet, (Just report)) = (entityKey planet) == (cprPlanetId report)
                  fn _ = True
        
prop_grouped_planet_report_list_is_as_long_as_planets_list :: Property
prop_grouped_planet_report_list_is_as_long_as_planets_list = 
    forAll planetsAndReports $ \(planets, reports) 
        -> length planets == (length $ groupPlanetReports planets reports)
        
prop_every_planet_is_present_in_grouped_planet_report_list :: Property
prop_every_planet_is_present_in_grouped_planet_report_list =
    forAll planetsAndReports $ \(planets, reports) 
        -> all (planetIsInGroupedReport $ groupPlanetReports planets reports) planets

prop_ocStarList_is_as_long_as_needs_observation_list :: Property
prop_ocStarList_is_as_long_as_needs_observation_list =
    forAll unobservedStarList $ \entities
        -> length entities == (length $ buildOCStarList entities)

prop_ocStarList_contains_items_needing_observation :: Property
prop_ocStarList_contains_items_needing_observation =
    forAll unobservedStarList $ \entities
        -> all (starIsInCandidateList $ buildOCStarList entities) entities

prop_ocPlanetList_is_as_long_as_needs_observation_list :: Property
prop_ocPlanetList_is_as_long_as_needs_observation_list =
    forAll unobservedPlanetList $ \entities
        -> length entities == (length $ buildOCPlanetList entities)

prop_ocPlanetList_contains_items_needing_observation :: Property
prop_ocPlanetList_contains_items_needing_observation =
    forAll unobservedPlanetList $ \entities
        -> all (planetIsInCandidateList $ buildOCPlanetList entities) entities

-- needsObservation
--  fully observed don't need observation
--  not observed needs observation

--- needsObservation :: ObservationCandidate -> Bool

--- data ObservationCandidate = OCStar (Entity Star) (Maybe CollatedStarReport)
--- | OCPlanet (Entity Planet) (Maybe CollatedPlanetReport)
--- | OCStarLane (Entity StarLane) (Maybe CollatedStarLaneReport)



--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll