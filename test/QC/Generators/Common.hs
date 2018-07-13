module QC.Generators.Common where

import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck
import Test.QuickCheck.Instances

perhaps :: Gen a -> Gen (Maybe a)
perhaps a = do
    res <- a
    oneof [return Nothing, return $ Just res]