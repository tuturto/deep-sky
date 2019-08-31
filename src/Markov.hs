{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Markov
    ( Config(..), Item(..), chain, chainM, addStart, addLink, addEnd, itemFreqL
    , itemItemL, configStartsL, configContinuationsL, emptyConfig, chains )
    where

import Import
import Control.Lens ( Lens', lens, (.~), (^.), (?~), (%~), at, (+~) )
import Control.Lens.TH
import Control.Monad.Random ( Rand, runRand )
import qualified Data.Map.Strict as M
import System.Random

import Common ( Frequency(..), chooseM, safeHead, safeTail )


-- | Configuration for markov chains
data Config a = Config
    { configStarts :: ![Item a]
    , configContinuations :: !(M.Map a [Item a])
    } deriving (Show, Read, Eq)


-- | Completely empty config
emptyConfig :: Config a
emptyConfig =
    Config
    { configStarts = []
    , configContinuations = M.empty
    }


-- | Single element that might appear in chain
-- Nothing denotes end of chain
data Item a =
    Item (Frequency (Maybe a))
    deriving (Show, Read, Eq)


makeLensesFor [ ("configStarts", "configStartsL")
              , ("configContinuations", "configContinuationsL")] ''Config


-- | Add starting element
addStart :: Ord a => a -> Config a -> Config a
addStart nxt config =
    addElement Nothing (Just nxt) config


-- | Add element in middle of chain
addLink :: Ord a => a -> a -> Config a -> Config a
addLink prev nxt config =
    addElement (Just prev) (Just nxt) config


-- | Add ending element
addEnd :: Ord a => a -> Config a -> Config a
addEnd prev config =
    addElement (Just prev) Nothing config


-- | Add new element into markov chain config
addElement :: forall a. Ord a => Maybe a -> Maybe a -> Config a -> Config a
addElement prev nxt config =
    case prev of
        Nothing ->
            let
                (hPieces, tPieces) = break (\x -> x ^. itemItemL == nxt) (config ^. configStartsL)
            in
                case safeHead tPieces of
                    Nothing ->
                        -- new start is added
                        configStartsL %~ ((Item $ Frequency 1 nxt) :) $ config

                    Just found ->
                        -- existing start's frequency is updated
                        configStartsL .~ starts' $ config
                        where
                            starts' = hPieces ++ [ (itemFreqL +~ 1 $ found) ] ++ safeTail tPieces

        Just key ->
            case (config ^. configContinuationsL . at key) of
                Nothing ->
                    -- new continuation is added if previous item wasn't already present
                    (configContinuationsL . at key) ?~ [ Item $ Frequency 1 nxt ] $ config

                Just conts ->
                    -- if previous item has already been added, we have to either create a new
                    -- continuation for it or update frequency of the old one
                    let
                        (hPieces, tPieces) = break (\x -> x ^. itemItemL == nxt) conts
                    in
                        case safeHead tPieces of
                            Nothing ->
                                (configContinuationsL . at key) ?~ ( (Item $ Frequency 1 nxt) : conts) $ config

                            Just found ->
                                (configContinuationsL . at key) ?~ conts' $ config
                                where
                                    conts' = hPieces ++ [ (itemFreqL +~ 1 $ found) ] ++ safeTail tPieces


-- | Markov chain derived from given configuration
chainM :: (Ord a, RandomGen g) => Config a -> Rand g [a]
chainM config = do
    starter <- chooseM (itemToFreq <$> configStarts config)
    case join starter of
        Nothing ->
            return []

        Just h -> do
            t <- tailOfChain config h
            return $ h : t


-- | Tail of the chain, starting from given element
tailOfChain :: (Ord a, RandomGen g) => Config a -> a -> Rand g [a]
tailOfChain config c = do
    item <- chooseM (candidates config c)
    case join item of
        Nothing ->
            return []

        Just x -> do
             xs <- tailOfChain config x
             return $ x : xs


-- | Markov chain derived from given config and random generator
chain :: (Ord a, RandomGen g) => Config a -> g -> ([a], g)
chain config g =
    runRand (chainM config) g


-- | Infinite amount of markov chains derived from given config
chains :: (Ord a, RandomGen g) => Config a -> g -> [[a]]
chains config g =
        c : chains config g'
    where
        (c, g') = chain config g


-- | All possible continuations of given chain
-- Empty list denotes end of chain
candidates :: (Ord a) => Config a -> a -> [Frequency (Maybe a)]
candidates config x =
    concat $ (fmap . fmap) itemToFreq items
    where
        items = M.lookup x (configContinuations config)


-- | Helper function to map item of chain into frequency in weighted list
itemToFreq :: Item a -> Frequency (Maybe a)
itemToFreq (Item x) =
    x


itemFreqL :: Lens' (Item a) Int
itemFreqL = lens (\(Item (Frequency n _)) -> n)
                 (\(Item (Frequency _ a)) n -> (Item (Frequency n a)))


itemItemL :: Lens' (Item a) (Maybe a)
itemItemL = lens (\(Item (Frequency _ a)) -> a)
                 (\(Item (Frequency n _)) a -> (Item (Frequency n a)))
