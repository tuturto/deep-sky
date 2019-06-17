{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
module QC.PeopleSpec (spec)
    where

import Test.QuickCheck
import Test.Hspec

import QC.Generators.Import

import TestImport
import People.Data ( RelationVisibility(..) )
import People.Import ( knownLink )


spec :: Spec
spec = do
    describe "people" $ do
        describe "relations" $ do
            it "public relations are always known" $ do
                forAll (anyPersonDataLink PublicRelation
                                          anyPersonIntel
                                          anyPersonIntel) $
                    \item ->
                        knownLink item

            it "family relations are not known if intel doesn't include family or secret relations" $ do
                forAll (anyPersonDataLink FamilyRelation
                                          intelWithoutFamilyOrSecretMatters
                                          intelWithoutFamilyOrSecretMatters) $
                    \item ->
                        not $ knownLink item

            it "family relations are known if intel of target person includes family or secret relations" $ do
                forAll (anyPersonDataLink FamilyRelation
                                          intelWithFamilyOrSecretMatters
                                          intelWithoutFamilyOrSecretMatters) $
                    \item ->
                        knownLink item

            it "family relations are known if intel of originator person includes family or secret relations" $ do
                forAll (anyPersonDataLink FamilyRelation
                                          intelWithoutFamilyOrSecretMatters
                                          intelWithFamilyOrSecretMatters) $
                    \item ->
                        knownLink item


            it "secret relations are not known if intel doesn't include secret relations" $ do
                forAll (anyPersonDataLink SecretRelation
                                          intelWithoutSecretMatters
                                          intelWithoutSecretMatters) $
                    \item ->
                        not $ knownLink item

            it "secret relations are known if intel of target person includes secret relations" $ do
                forAll (anyPersonDataLink SecretRelation
                                          intelWithSecretMatters
                                          intelWithoutSecretMatters) $
                    \item ->
                        knownLink item

            it "secret relations are known if intel of originator person includes secret relations" $ do
                forAll (anyPersonDataLink SecretRelation
                                          intelWithoutSecretMatters
                                          intelWithSecretMatters) $
                    \item ->
                        knownLink item
