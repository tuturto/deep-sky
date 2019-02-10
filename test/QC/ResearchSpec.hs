{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module QC.ResearchSpec ( spec )
    where

import Test.QuickCheck

import TestImport

import QC.Generators.Research ( researchWithoutAntecedents, researchWithAntecedents )
import Research.Data ( Research(..) )
import Research.Import ( availableForResearch, isAvailable)
import Research.Tree ( techTree )


spec :: Spec
spec = do
    describe "research" $ do
        describe "tech tree" $ do
            it "unfinished research without antecedents is available for research" $ do
                forAll researchWithoutAntecedents $ \x -> isAvailable [] x `shouldBe` True

            it "unfinished research with antecedents and empty research done isn't available for research" $ do
                forAll researchWithAntecedents $ \x -> isAvailable [] x `shouldBe` False

            it "all research available when no research has been done has no antecedents" $ do
                let available = availableForResearch techTree []
                all (null . researchAntecedents) available `shouldBe` True
