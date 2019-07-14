{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}

module QC.MarkovSpec (spec)
    where

import Prelude
import Test.Hspec
import Test.QuickCheck

import Control.Lens ( (^?), ix, _Just, _head )
import Control.Monad.Random ( evalRand )
import Data.Char ( isLower, isUpper )
import qualified Data.Text as DT

import QC.Generators.Common ( anyRandomGen )

import Markov ( addStart, addLink, itemFreqL, itemItemL, configStartsL
              , configContinuationsL, emptyConfig )
import Names ( greekNameM )
import People.Data ( FirstName(..) )


spec :: Spec
spec = do
    describe "Markov chain configuration" $ do
        it "Adding new starting element to empty configuration creates item with frequency of 1" $ do
            let config = addStart ("AA" :: DT.Text) emptyConfig
            config ^? (configStartsL . _head . itemFreqL) `shouldBe` Just 1
            config ^? (configStartsL . _head . itemItemL . _Just) `shouldBe` Just "AA"

        it "Adding same element twice to empty configuration creates item with frequency of 2" $ do
            let config = addStart "AA" $
                         addStart ("AA" :: DT.Text) emptyConfig
            config ^? (configStartsL . _head . itemFreqL) `shouldBe` Just 2
            config ^? (configStartsL . _head . itemItemL . _Just) `shouldBe` Just "AA"

        it "Adding new continuation creates item with frequency of 1" $ do
            let config = addLink "AA" "BB" $
                         addStart ("AA" :: DT.Text) emptyConfig
            config ^? (configContinuationsL . ix "AA" . _head . itemFreqL) `shouldBe` Just 1
            config ^? (configContinuationsL . ix "AA" . _head . itemItemL . _Just) `shouldBe` Just "BB"

        it "Adding twice to same element creates item with frequency of 2" $ do
            let config = addLink "AA" "BB" $
                         addLink "AA" "BB" $
                         addStart ("AA" :: DT.Text) emptyConfig
            config ^? (configContinuationsL . ix "AA" . _head . itemFreqL) `shouldBe` Just 2
            config ^? (configContinuationsL . ix "AA" . _head . itemItemL . _Just) `shouldBe` Just "BB"

    describe "Markov chain generation" $ do
        it "Generated names are longer than zero" $ do
            forAll anyRandomGen
                   (\g -> DT.length (unFirstName $ evalRand greekNameM g) > 0)

        it "Generated names start with upper case, followed by lower case" $ do
            forAll anyRandomGen
                   (\g ->
                        let
                            name = unFirstName $ evalRand greekNameM g
                        in
                            isUpper (DT.head name) && DT.all isLower (DT.tail name))
