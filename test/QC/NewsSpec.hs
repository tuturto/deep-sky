{-# LANGUAGE TemplateHaskell            #-}

module QC.NewsSpec (spec)
    where

import Prelude
import Test.QuickCheck
import Test.Hspec

import QC.Helpers (deserializationMirrosSerialization)

import QC.Generators.Import


spec :: Spec
spec = do
    describe "News and messages" $ do
        describe "JSON handling" $ do
            it "deserializing serialized design created news dto yields original data" $ do
                forAll singleDesignCreatedNewsDto deserializationMirrosSerialization

            it "deserializing serialized user written news dto yields original data" $ do
                forAll singleUserWrittenNewsDto deserializationMirrosSerialization
