{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module QC.Generators.Common ( ArbStarDate(..), perhaps, anyRandomGen )
    where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import System.Random ( StdGen, mkStdGen )

import CustomTypes ( StarDate(..) )

perhaps :: Gen a -> Gen (Maybe a)
perhaps a = do
    res <- a
    oneof [return Nothing, return $ Just res]


newtype ArbStarDate =
    ArbStarDate { unArbStarDate :: StarDate }
    deriving (Show, Read, Eq, Num, Ord)


instance Arbitrary ArbStarDate where
    arbitrary = do
        date <- arbitrary `suchThat` \x -> x > 0
        return $ ArbStarDate $ StarDate date


anyRandomGen :: Gen StdGen
anyRandomGen = do
    seed <- arbitrary
    return $ mkStdGen seed
