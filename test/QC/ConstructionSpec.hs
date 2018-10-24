{-# LANGUAGE TemplateHaskell            #-}
module QC.ConstructionSpec (spec) 
    where

import Test.QuickCheck
import Test.Hspec

import QC.Generators.Import
import Model
import Resources ( RawResources(..), RawResource(..) )
import Simulation.Construction ( overallConstructionSpeed )
import Construction ( ConstructionSpeedCoeff(..), OverallConstructionSpeed(..), speedLimitedByWorkLeft )

spec :: Spec
spec = do
    describe "construction" $ do
        describe "speed" $ do
            it "construction speed is never greater than total cost in unstarted construction" $ do
                forAll unstartedConstructionsWithSomeSpeed $ \(cSpeed, bConst, cTotal) -> 
                    let
                        speed = speedLimitedByWorkLeft cSpeed bConst cTotal
                    in
                        ccdMechanicalCost speed <= ccdMechanicalCost cTotal
                        && ccdBiologicalCost speed <= ccdBiologicalCost cTotal
                        && ccdChemicalCost speed <= ccdChemicalCost cTotal

            it "construction speed + construction done is never greater than total construction" $ do
                forAll unfinishedConstructionsWithSomeSpeed $ \(cSpeed, bConst, cTotal) ->
                    let
                        speed = speedLimitedByWorkLeft cSpeed bConst cTotal
                    in
                        ccdMechanicalCost speed + (RawResource . buildingConstructionProgressMechanicals) bConst <= ccdMechanicalCost cTotal
                        && ccdBiologicalCost speed + (RawResource . buildingConstructionProgressBiologicals) bConst <= ccdBiologicalCost cTotal
                        && ccdChemicalCost speed + (RawResource . buildingConstructionProgressChemicals) bConst <= ccdChemicalCost cTotal

            it "overall speed is full when there is enough resources" $ do
                forAll resourceCostAndEnoughAvailableResources $ \(cost, available) ->
                    let
                        speed = overallConstructionSpeed cost available
                    in
                        overallSpeedBiological speed == NormalConstructionSpeed
                        && overallSpeedMechanical speed == NormalConstructionSpeed
                        && overallSpeedChemical speed == NormalConstructionSpeed

            it "overall speed is less than full when there is not enough resources" $ do
                forAll resourceCostAndLimitedResources $ \(cost, available) ->
                    let
                        speed = overallConstructionSpeed cost available
                    in
                        overallSpeedBiological speed < NormalConstructionSpeed
                        && overallSpeedMechanical speed < NormalConstructionSpeed
                        && overallSpeedChemical speed < NormalConstructionSpeed
