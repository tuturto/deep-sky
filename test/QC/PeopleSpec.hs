{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
module QC.PeopleSpec (spec)
    where

import Test.QuickCheck
import Test.Hspec

import QC.Generators.Import

import TestImport
import People.Import ( knownLink, flipRelation )
import People.Opinion ( OpinionReport(..), reportResultToOpinionResult )

spec :: Spec
spec = do
    describe "people" $ do
        describe "relations" $ do
            it "relation flipped twice is the original relation" $ do
                forAll anyRelationVisibility $ \visibility ->
                    forAll (relationWithVisibility visibility) $ \relation ->
                        relation == (flipRelation . flipRelation) relation

            it "public relations are always known" $ do
                forAll (anyPersonDataLink publicRelation
                                          anyPersonIntel
                                          anyPersonIntel) $
                    \item ->
                        knownLink item

            it "family relations are not known if intel doesn't include family or secret relations" $ do
                forAll (anyPersonDataLink familyRelation
                                          intelWithoutFamilyOrSecretMatters
                                          intelWithoutFamilyOrSecretMatters) $
                    \item ->
                        not $ knownLink item

            it "family relations are known if intel of target person includes family or secret relations" $ do
                forAll (anyPersonDataLink familyRelation
                                          intelWithFamilyOrSecretMatters
                                          intelWithoutFamilyOrSecretMatters) $
                    \item ->
                        knownLink item

            it "family relations are known if intel of originator person includes family or secret relations" $ do
                forAll (anyPersonDataLink familyRelation
                                          intelWithoutFamilyOrSecretMatters
                                          intelWithFamilyOrSecretMatters) $
                    \item ->
                        knownLink item


            it "secret relations are not known if intel doesn't include secret relations" $ do
                forAll (anyPersonDataLink secretRelation
                                          intelWithoutSecretMatters
                                          intelWithoutSecretMatters) $
                    \item ->
                        not $ knownLink item

            it "secret relations are known if intel of target person includes secret relations" $ do
                forAll (anyPersonDataLink secretRelation
                                          intelWithSecretMatters
                                          intelWithoutSecretMatters) $
                    \item ->
                        knownLink item

            it "secret relations are known if intel of originator person includes secret relations" $ do
                forAll (anyPersonDataLink secretRelation
                                          intelWithoutSecretMatters
                                          intelWithSecretMatters) $
                    \item ->
                        knownLink item

        describe "opinions" $ do
            it "reported opinion score is always within -100 and 100 " $ do
                forAll anyReportResult $
                    \report ->
                        reportedScoreWithinRange $ reportResultToOpinionResult report


-- | Check that reported score in opinion report is within range of -100 and 100
reportedScoreWithinRange :: OpinionReport -> Bool
reportedScoreWithinRange (BaseOpinionReport _) =
    True

reportedScoreWithinRange (OpinionReasonReport _ _) =
    True

reportedScoreWithinRange (DetailedOpinionReport score _) =
    score >= (-100) && score <= 100
