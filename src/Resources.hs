{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Resources ( RawResources(..), ResourceCost(..), ResourceProduction(..), ConstructionSpeed(..)
                 , ConstructionLeft(..), ConstructionDone(..), ResourcesAvailable(..)
                 , RawResource(..), Biological(..), Mechanical(..), Chemical(..), ResourceType(..)
                 , subTotalCost )
    where

import Data.Aeson (object, withObject, (.=), ToJSON(..), FromJSON(..))
import Data.Aeson.TH
import ClassyPrelude.Yesod   as Import
import Data.Monoid ()
import CustomTypes ( Bonus(..), Boostable(..) )


data RawResources a = RawResources
    { ccdMechanicalCost :: RawResource Mechanical
    , ccdBiologicalCost :: RawResource Biological
    , ccdChemicalCost :: RawResource Chemical
    } deriving (Show, Read, Eq)


data ResourceCost = ResourceCost
data ResourceProduction = ResourceProduction
data ConstructionSpeed = ConstructionSpeed
data ConstructionLeft = ConstructionLeft
data ConstructionDone = ConstructionDone
data ResourcesAvailable = ResourcesAvailable


-- | Subtract one totalcost from another
subTotalCost :: RawResources t -> RawResources t -> RawResources t
subTotalCost a b =
    RawResources { ccdMechanicalCost = ccdMechanicalCost a - ccdMechanicalCost b
                 , ccdBiologicalCost = ccdBiologicalCost a - ccdBiologicalCost b
                 , ccdChemicalCost = ccdChemicalCost a - ccdChemicalCost b
                 }


instance Semigroup (RawResources t) where
    (<>) a b = RawResources
        { ccdMechanicalCost = ccdMechanicalCost a <> ccdMechanicalCost b
        , ccdBiologicalCost = ccdBiologicalCost a <> ccdBiologicalCost b
        , ccdChemicalCost = ccdChemicalCost a <> ccdChemicalCost b
        }


instance Monoid (RawResources t) where
    mempty = RawResources
        { ccdMechanicalCost = RawResource 0
        , ccdBiologicalCost = RawResource 0
        , ccdChemicalCost = RawResource 0
        }


newtype RawResource a = RawResource { unRawResource :: Int }
    deriving (Show, Read, Eq)


data ResourceType =
    BiologicalResource
    | MechanicalResource
    | ChemicalResource
    deriving (Show, Read, Eq)


data Biological = Biological
data Mechanical = Mechanical
data Chemical = Chemical


instance Semigroup (RawResource t) where
    (<>) a b = a + b


instance Monoid (RawResource t) where
    mempty = RawResource 0


instance Ord (RawResource t) where
    (<=) (RawResource a) (RawResource b) = a <= b


instance Num (RawResource t) where
    (+) (RawResource a) (RawResource b) = RawResource $ a + b
    (-) (RawResource a) (RawResource b) = RawResource $ a - b
    (*) (RawResource a) (RawResource b) = RawResource $ a * b
    abs (RawResource a) = RawResource $ abs a
    signum (RawResource a) = RawResource $ signum a
    fromInteger a = RawResource $ fromInteger a


instance Boostable (RawResource t) where
    applyBonus (Bonus a p) (RawResource r) =
        RawResource $ a + floor (fromIntegral r * p)


instance ToJSON (RawResources t) where
    toJSON (RawResources mech bio chem) =
        object [ "mechanical" .= unRawResource mech
               , "biological" .= unRawResource bio
               , "chemical" .= unRawResource chem
               ]


instance FromJSON (RawResources t) where
    parseJSON = withObject "resource" $ \o -> do
        mechanical <- o .: "mechanical"
        biological <- o .: "biological"
        chemical <- o .: "chemical"
        return $ RawResources (RawResource mechanical) (RawResource biological) (RawResource chemical)


$(deriveJSON defaultOptions ''ResourceType)