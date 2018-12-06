{-# LANGUAGE TemplateHaskell            #-}

module QC.NewsSpec (spec)
    where

import Prelude
import Test.QuickCheck
import Test.Hspec

import Data.Aeson ( decode, encode, FromJSON, ToJSON )

import QC.Generators.Import


-- | Helper to check if Maybe Bool is True
isMaybeTrue :: Maybe Bool -> Bool
isMaybeTrue (Just x) = x == True
isMaybeTrue Nothing = False


-- | Check if deserializing a serialized object will yield original data
deserializationMirrosSerialization :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
deserializationMirrosSerialization dto =
    isMaybeTrue $ fmap (== dto) $ (decode . encode) dto


spec :: Spec
spec = do
    describe "News and messages" $ do
        describe "JSON handling" $ do
            it "deserializing serialized design created news dto yields original data" $ do
                forAll singleDesignCreatedNewsDto deserializationMirrosSerialization

            it "deserializing serialized user written news dto yields original data" $ do
                forAll singleUserWrittenNewsDto deserializationMirrosSerialization
