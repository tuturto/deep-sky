{-# LANGUAGE TemplateHaskell            #-}
module QC.ConstructionSpec (spec) 
    where

import Test.QuickCheck
import Test.Hspec

import QC.Generators.Import
import CustomTypes ( RawResources(..) )
import Simulation.Construction ( speedLimitedByWorkLeft )

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
