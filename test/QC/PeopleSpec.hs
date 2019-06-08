{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
module QC.PeopleSpec (spec)
    where

import Test.QuickCheck
import Test.Hspec

import QC.Generators.Import

import TestImport
import People.Import ( knownRelation )

-- knownRelation :: [PersonIntel] -> Relation -> Bool

spec :: Spec
spec = do
    describe "people" $ do
        describe "relations" $ do
            it "public relations are always known" $ do
                forAll publicRelation $
                    \(intel, relation) ->
                        knownRelation intel relation

            it "family relations are not known if intel doesn't include family or secret relations" $ do
                forAll familyRelationsWithoutFamilyOrSecretIntel $
                    \(intel, relation) ->
                        not $ knownRelation intel relation

            it "family relations are known if intel includes family or secret relations" $ do
                forAll familyRelationsWithFamilyOrSecretIntel $
                    \(intel, relation) ->
                        knownRelation intel relation

            it "secret relations are not known if intel doesn't include secret relations" $ do
                forAll secretRelationsWithoutSecretIntel $
                    \(intel, relation) ->
                        not $ knownRelation intel relation

            it "secret relations are known if intel includes secret relations" $ do
                forAll secretRelationsWithSecretIntel $
                    \(intel, relation) ->
                        knownRelation intel relation
