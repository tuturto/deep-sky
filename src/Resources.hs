{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Resources ( RawResources(..), ResourceCost(..), ResourceProduction(..), ConstructionSpeed(..)
                 , ConstructionLeft(..), ConstructionDone(..), ResourcesAvailable(..)
                 , RawResource(..), Biological(..), Mechanical(..), Chemical(..), ResourceType(..)
                 , subTotalCost )
    where

import Data.Aeson ( ToJSON(..), FromJSON(..), object, withObject
                  , withScientific, (.=) )
import Data.Aeson.TH
import Data.Scientific ( toBoundedInteger )
import Database.Persist.Sql
import ClassyPrelude.Yesod   as Import
import Data.Monoid ()
import CustomTypes ( Bonus(..), Boostable(..) )


data RawResources a = RawResources
    { ccdMechanicalCost :: RawResource Mechanical
    , ccdBiologicalCost :: RawResource Biological
    , ccdChemicalCost :: RawResource Chemical
    } deriving (Show, Read, Eq, Ord)


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
    deriving (Show, Read, Eq, Ord, Num)


instance ToJSON (RawResource a) where
    toJSON = toJSON . unRawResource


instance FromJSON (RawResource a) where
    parseJSON =
        withScientific "raw resource"
            (\x -> case toBoundedInteger x of
                Nothing ->
                    mempty

                Just n ->
                    return $ RawResource n)


instance PersistField (RawResource a) where
    toPersistValue (RawResource n) =
        PersistInt64 $ fromIntegral n

    fromPersistValue (PersistInt64 n) =
        Right $ RawResource $ fromIntegral n

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql (RawResource a) where
    sqlType _ = SqlInt64


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


instance Boostable (RawResource t) where
    applyBonus (Bonus a p) (RawResource r) =
        RawResource $ a + floor (fromIntegral r * p)


instance ToJSON (RawResources t) where
    toJSON (RawResources mech bio chem) =
        object [ "mechanical" .= toJSON mech
               , "biological" .= toJSON bio
               , "chemical" .= toJSON chem
               ]


instance FromJSON (RawResources t) where
    parseJSON = withObject "resource" $ \o -> do
        mechanical <- o .: "mechanical"
        biological <- o .: "biological"
        chemical <- o .: "chemical"
        return $ RawResources mechanical biological chemical


$(deriveJSON defaultOptions ''ResourceType)
$(deriveJSON defaultOptions ''Biological)
$(deriveJSON defaultOptions ''Mechanical)
$(deriveJSON defaultOptions ''Chemical)
