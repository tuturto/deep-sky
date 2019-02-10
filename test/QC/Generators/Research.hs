{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module QC.Generators.Research
    where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()

import TestImport
import qualified Prelude as P

import Research.Data ( Research(..), TechTree(..) )
import Research.Tree ( techTree )


researchWithoutAntecedents :: Gen Research
researchWithoutAntecedents = do
    let targets = filter (null . researchAntecedents) $ unTechTree techTree
    n <- arbitrary `suchThat` (\x -> x >= 0 && x < length targets)
    return $ targets P.!! n


researchWithAntecedents :: Gen Research
researchWithAntecedents = do
    let targets = filter (not . null . researchAntecedents) $ unTechTree techTree
    n <- arbitrary `suchThat` (\x -> x >= 0 && x < length targets)
    return $ targets P.!! n
