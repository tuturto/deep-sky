module QC.Generators.Common where

import Test.QuickCheck.Gen

perhaps :: Gen a -> Gen (Maybe a)
perhaps a = do
    res <- a
    oneof [return Nothing, return $ Just res]