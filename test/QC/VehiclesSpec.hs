{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}

module QC.VehiclesSpec (spec)
    where

import qualified Prelude as P
import Test.Hspec
import Test.QuickCheck
import TestImport

import Database.Persist.Sql (toSqlKey)

import QC.Generators.Vehicles ( positiveCrewRequirements )

import Control.Lens ( (^.), (^..), traverse )
import Research.Data ( Technology(..) )
import Vehicles.Components ( ComponentId(..), ChassisType(..), components )
import Vehicles.Data ( CrewPosition(..), CrewRank(..), CrewAmount(..)
                     , CrewSpaceReq(..), unCrewAmountL )
import Vehicles.Stats ( CrewRequirement(..), seniorityRanks
                      , crewRequirementAmountL, quarterCrew, compsToCrew
                      , crewRequirementPositionL, designNominalCrew
                      , plannedToCompPair, unCrewSpaceL, designCrewSpace
                      , totalCrewSpaceSteerageL )

spec :: Spec
spec = do
    describe "Vehicle stats" $ do
        describe "Crew requirements" $ do
            describe "Seniority ranks" $ do
                it "Commander will not have any seniority ranks" $ do
                    let res = seniorityRanks $ CrewRequirement Commander Chief 1
                    length res `shouldBe` 0

                it "4 second class ranked crew will not have seniority ranks" $ do
                    let res = seniorityRanks $ CrewRequirement Artificer SecondClass 4
                    length res `shouldBe` 0

                it "5 second class ranked crew will have one chief ranked senior" $ do
                    let res = seniorityRanks $ CrewRequirement Artificer SecondClass 5
                    matchingCrew res Artificer Chief `shouldBe` 1

                it "5 second class ranked crew will not have 1st class ranked seniors" $ do
                    let res = seniorityRanks $ CrewRequirement Artificer SecondClass 5
                    matchingCrew res Artificer FirstClass `shouldBe` 0

                it "10 second class ranked crew will have one chief ranked senior" $ do
                    let res = seniorityRanks $ CrewRequirement Artificer SecondClass 10
                    matchingCrew res Artificer Chief `shouldBe` 1

                it "10 second class ranked crew will have two 1st class ranked senior" $ do
                    let res = seniorityRanks $ CrewRequirement Artificer SecondClass 10
                    matchingCrew res Artificer FirstClass `shouldBe` 2

                it "25 second class ranked crew will have five 1st class ranked senior" $ do
                    let res = seniorityRanks $ CrewRequirement Artificer SecondClass 25
                    matchingCrew res Artificer FirstClass `shouldBe` 5

                it "25 second class ranked crew will have one senior class ranked senior" $ do
                    let res = seniorityRanks $ CrewRequirement Artificer SecondClass 25
                    matchingCrew res Artificer Senior `shouldBe` 1

            describe "minimum crew" $ do
                it "for all positive nominal crew, quarter crew is also positive" $ do
                    forAll positiveCrewRequirements $
                        \reqs -> all (\x -> x ^. (crewRequirementAmountL . unCrewAmountL) > 0) (quarterCrew reqs)

                it "every position in nominal crew is also present in quarter crew" $ do
                    forAll positiveCrewRequirements $
                        (\reqs -> let
                                    minPos = quarterCrew reqs ^.. traverse . crewRequirementPositionL
                                    nomPos = reqs ^.. traverse . crewRequirementPositionL
                                  in
                                    all (\x -> x `elem` minPos) nomPos)

            describe "component crew requirements" $ do
                it "Single ship long range sensor requires artificer, sensor operator and crew member" $ do
                    let sensors = [ components 1 ShipLongRangeSensors ]
                    let reqs = compsToCrew sensors
                    matchingCrew reqs Artificer SecondClass `shouldBe` 1
                    matchingCrew reqs SensorOperator SecondClass `shouldBe` 1
                    matchingCrew reqs Crew SecondClass `shouldBe` 1

                it "Two ship long ranger sensors requires artificer, two sensor operators and two crew members" $ do
                    let sensors = [ components 1 ShipLongRangeSensors
                                  , components 1 ShipLongRangeSensors
                                  ]
                    let reqs = compsToCrew sensors
                    matchingCrew reqs Artificer SecondClass `shouldBe` 1
                    matchingCrew reqs SensorOperator SecondClass `shouldBe` 2
                    matchingCrew reqs Crew SecondClass `shouldBe` 2

                it "Ship luxury quarters and ship infantry bay requires three crew members and one artificer" $ do
                    let sensors = [ components 1 ShipLuxuryQuarters
                                  , components 1 ShipInfantryBay
                                  ]
                    let reqs = compsToCrew sensors
                    matchingCrew reqs Artificer SecondClass `shouldBe` 1
                    matchingCrew reqs Crew SecondClass `shouldBe` 3

            describe "Design crew requirements" $ do
                it "Two star sails require more crew than one" $ do

                    let comp0 = [PlannedComponent
                                    { plannedComponentDesignId = toSqlKey 0
                                    , plannedComponentComponentId = ShipStarSail
                                    , plannedComponentLevel = 1
                                    , plannedComponentAmount = 1
                                    }
                                ]
                    let res0 = designNominalCrew (plannedToCompPair <$> comp0)

                    let comp1 = [PlannedComponent
                                    { plannedComponentDesignId = toSqlKey 0
                                    , plannedComponentComponentId = ShipStarSail
                                    , plannedComponentLevel = 1
                                    , plannedComponentAmount = 2
                                    }
                                ]
                    let res1 = designNominalCrew (plannedToCompPair <$> comp1)

                    matchingCrew res1 Crew SecondClass > matchingCrew res0 Crew SecondClass `shouldBe` True

            describe "Crew space" $ do
                it "Two planned steerage quarters provide more space than one" $ do
                    let res_0 = designCrewSpace [PlannedComponent
                                                    { plannedComponentDesignId = toSqlKey 0
                                                    , plannedComponentComponentId = ShipSteerageQuarters
                                                    , plannedComponentLevel = 1
                                                    , plannedComponentAmount = 1
                                                    }
                                                ]

                    let res_1 = designCrewSpace [PlannedComponent
                                                    { plannedComponentDesignId = toSqlKey 0
                                                    , plannedComponentComponentId = ShipSteerageQuarters
                                                    , plannedComponentLevel = 1
                                                    , plannedComponentAmount = 2
                                                    }
                                                ]

                    (res_0 ^. totalCrewSpaceSteerageL . unCrewSpaceL) < (res_1 ^. totalCrewSpaceSteerageL . unCrewSpaceL)
                        `shouldBe` True


-- | Amount of crew matching to position and rank
matchingCrew :: [CrewRequirement] -> CrewPosition -> CrewRank -> CrewAmount
matchingCrew crew p r =
    sum $ fmap (\(CrewRequirement _ _ ca) -> ca) matching
    where
        matching = P.filter (\(CrewRequirement cp cr _) -> cp == p && cr == r) crew


-- TODO: move somewhere general
bilanderChassis :: Chassis
bilanderChassis =
    Chassis
        { chassisName = "Bilander"
        , chassisTonnage = 400
        , chassisType = SpaceShip
        , chassisTechnology = Just BilanderHulls
        , chassisArmourSlots = 3
        , chassisInnerSlots = 8
        , chassisOuterSlots = 6
        , chassisSensorSlots = 2
        , chassisWeaponSlots = 4
        , chassisEngineSlots = 0
        , chassisMotiveSlots = 0
        , chassisSailSlots = 2
        , chassisCrewSpaceRequired = CrewSpaceRequired
        }
