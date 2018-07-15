{-# LANGUAGE TemplateHaskell            #-}

module QC.Observations where

import Test.QuickCheck.All
import Test.QuickCheck

import Database.Persist.Sql
import Model
import Simulation.Observations (groupPlanetReports, groupStarReports, groupStarLaneReports)
import Report
import Data.List (find)
import QC.Generators.Import

planetIsInGroupedReport :: [(Entity Planet, Maybe CollatedPlanetReport)] -> Entity Planet -> Bool
planetIsInGroupedReport report planet = 
    case wasFound of
        (Just _) -> True
        Nothing  -> False
    where
        wasFound = find (\p -> (entityKey planet) == (entityKey $ fst p)) report

starIsInGroupedReport :: [(Entity Star, Maybe CollatedStarReport)] -> Entity Star -> Bool
starIsInGroupedReport report star = 
    case wasFound of
        (Just _) -> True
        Nothing  -> False
    where
        wasFound = find (\p -> (entityKey star) == (entityKey $ fst p)) report

starLaneIsInGroupedReport :: [(Entity StarLane, Maybe CollatedStarLaneReport)] -> Entity StarLane -> Bool
starLaneIsInGroupedReport report starlane = 
    case wasFound of
        (Just _) -> True
        Nothing  -> False
    where
        wasFound = find (\p -> (entityKey starlane) == (entityKey $ fst p)) report

prop_starlanes_and_their_reports_are_grouped_by_ids :: Property
prop_starlanes_and_their_reports_are_grouped_by_ids = 
    forAll starLanesAndReports $ \(lanes, reports) 
        -> all fn (groupStarLaneReports lanes reports)
            where fn (lane, (Just report)) = (entityKey lane) == (cslStarLaneId report)
                  fn _ = True

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

-- buildOCStarList
-- buildOCPlanetList
-- buildOCStarLaneList
-- needsObservation

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll