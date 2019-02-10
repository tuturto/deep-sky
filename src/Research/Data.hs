{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Research.Data ( ResearchCategory(..), EngineeringSubField(..), NaturalScienceSubField(..)
                     , SocialScienceSubField(..), ResearchScore(..), ResearchTier(..)
                     , Technology(..), Research(..), TechTree(..), EngineeringCost(..)
                     , SocialScienceCost(..), NaturalScienceCost(..), TotalResearchScore(..)
                     , ResearchProduction(..), ResearchCost(..), ResearchLimit(..)
                     , TopResearchCategory(..), ResearchLeft(..), ResearchProgress(..)
                     , isEngineering, isSocialScience, isNaturalScience
                     , sameTopCategory, topCategory )
    where

import Data.Aeson ( withScientific )
import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import Data.Scientific ( toBoundedInteger )
import Database.Persist.TH
import ClassyPrelude.Yesod   as Import


data ResearchCategory =
    Engineering EngineeringSubField
    | NaturalScience NaturalScienceSubField
    | SocialScience SocialScienceSubField
    deriving (Show, Read, Eq)


data TopResearchCategory =
    Eng
    | NatSci
    | SocSci
    deriving (Show, Read, Eq, Ord)


topCategory :: ResearchCategory -> TopResearchCategory
topCategory cat =
    case cat of
        (Engineering _) ->
            Eng

        (NaturalScience _) ->
            NatSci

        (SocialScience _) ->
            SocSci


isEngineering :: ResearchCategory -> Bool
isEngineering (Engineering _) = True
isEngineering _ = False


isNaturalScience :: ResearchCategory -> Bool
isNaturalScience (NaturalScience _) = True
isNaturalScience _ = False


isSocialScience :: ResearchCategory -> Bool
isSocialScience (SocialScience _) = True
isSocialScience _ = False


sameTopCategory :: ResearchCategory -> ResearchCategory -> Bool
sameTopCategory a b =
    topCategory a == topCategory b


data EngineeringSubField =
    Industry
    | Materials
    | Propulsion
    | FieldManipulation
    deriving (Show, Read, Eq)


data NaturalScienceSubField =
    Computing
    | EnergyManipulation
    | Particles
    | Biology
    deriving (Show, Read, Eq)


data SocialScienceSubField =
    MilitaryTheory
    | Statecraft
    | ShadowOps
    deriving (Show, Read, Eq)


newtype ResearchScore a = ResearchScore { unResearchScore :: Int }
    deriving (Show, Read, Eq, Ord, Num)


instance Semigroup (ResearchScore a) where
    (<>) (ResearchScore b) (ResearchScore c) = ResearchScore (b + c)


instance Monoid (ResearchScore a) where
    mempty = ResearchScore 0


data EngineeringCost = EngineeringCost
    deriving (Show, Read, Eq)


data NaturalScienceCost = NaturalScienceCost
    deriving (Show, Read, Eq)


data SocialScienceCost = SocialScienceCost
    deriving (Show, Read, Eq)


data ResearchCost = ResearchCost
    deriving (Show, Read, Eq)


data ResearchProduction = ResearchProduction
    deriving (Show, Read, Eq)


data ResearchLeft = ResearchLeft
    deriving (Show, Read, Eq)


data TotalResearchScore a = TotalResearchScore
    { totalResearchScoreEngineering :: ResearchScore EngineeringCost
    , totalResearchScoreNatural :: ResearchScore NaturalScienceCost
    , totalResearchScoreSocial :: ResearchScore SocialScienceCost
    }
    deriving (Show, Read, Eq)


instance Semigroup (TotalResearchScore a) where
    (<>) (TotalResearchScore a0 b0 c0) (TotalResearchScore a1 b1 c1) =
        TotalResearchScore (a0 <> a1) (b0 <> b1) (c0 <> c1)


instance Monoid (TotalResearchScore a) where
    mempty = TotalResearchScore mempty mempty mempty


newtype ResearchTier = ResearchTier { unResearchTier :: Int }
    deriving (Show, Read, Eq, Ord, Num)


data Technology =
    HighSensitivitySensors -- various sensors
    | SideChannelSensors
    | HighTensileMaterials -- armour
    | SatelliteTechnology
    -- hulls
    | DestroyerHulls
    | FrigateHulls
    | CruiserHulls
    | BattleShipHulls
    | MobileBaseHulls
    | StationHulls
    -- general military theory
    | CarrierOps -- Fighter bay and related
    | ExplorerCorps
    deriving (Show, Read, Eq, Enum, Bounded, Ord)


data Research = Research
    { researchName :: Text
    , researchType :: Technology
    , researchCategory :: ResearchCategory
    , researchAntecedents :: [Technology]
    , researchCost :: TotalResearchScore ResearchCost
    , researchTier :: ResearchTier
    }
    deriving (Show, Read, Eq)


data ResearchProgress = ResearchProgress
    { researchProgressResearch :: Research
    , researchProgressProgress :: ResearchScore ResearchLeft
    }
    deriving (Show, Read, Eq)


newtype TechTree = TechTree { unTechTree :: [Research] }
    deriving (Show, Read, Eq)


newtype ResearchLimit = ResearchLimit { unResearchLimit :: Int }
    deriving (Show, Read, Eq, Ord, Num)


instance ToJSON ResearchTier where
  toJSON = toJSON . unResearchTier


instance FromJSON ResearchTier where
    parseJSON =
        withScientific "research tier"
                       (\x -> case toBoundedInteger x of
                            Nothing ->
                                return $ ResearchTier 1

                            Just n ->
                                return $ ResearchTier n)


instance ToJSON (ResearchScore a) where
    toJSON = toJSON . unResearchScore


instance FromJSON (ResearchScore a) where
    parseJSON =
        withScientific "research score"
                       (\x -> case toBoundedInteger x of
                            Nothing ->
                                return $ ResearchScore 1

                            Just n ->
                                return $ ResearchScore n)


derivePersistField "ResearchCategory"
derivePersistField "Technology"
derivePersistField "EngineeringSubField"
derivePersistField "NaturalScienceSubField"
derivePersistField "SocialScienceSubField"
derivePersistField "TopResearchCategory"


$(deriveJSON defaultOptions ''ResearchCategory)
$(deriveJSON defaultOptions ''Technology)
$(deriveJSON defaultOptions ''EngineeringSubField)
$(deriveJSON defaultOptions ''NaturalScienceSubField)
$(deriveJSON defaultOptions ''SocialScienceSubField)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 18 } ''TotalResearchScore)
$(deriveJSON defaultOptions ''ResearchCost)
$(deriveJSON defaultOptions ''ResearchProduction)
$(deriveJSON defaultOptions ''EngineeringCost)
$(deriveJSON defaultOptions ''NaturalScienceCost)
$(deriveJSON defaultOptions ''SocialScienceCost)
$(deriveJSON defaultOptions ''ResearchLeft)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 8 } ''Research)
$(deriveJSON defaultOptions ''TopResearchCategory)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 16 } ''ResearchProgress)
