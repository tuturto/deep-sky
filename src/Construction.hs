{-# LANGUAGE NoImplicitPrelude          #-}

module Construction (ConstructionSpeed(..), Constructable(..))
    where

import Import
import CustomTypes (TotalCost(..), RawResource(..), Mechanical(..), Biological(..), Chemical(..))

class Constructable a where
    cIndex :: a -> Int
    cProgress :: a -> TotalCost

data ConstructionSpeed = ConstructionSpeed
    { constructionSpeedMechanicalCost :: RawResource Mechanical
    , constructionSpeedBiologicalCost :: RawResource Biological
    , constructionSpeedChemicalCost :: RawResource Chemical
    }
    deriving (Show, Read, Eq)

instance Constructable BuildingConstruction where
    cIndex = buildingConstructionIndex
    cProgress a =
        TotalCost (RawResource $ buildingConstructionProgressMechanicals a)
                  (RawResource $ buildingConstructionProgressBiologicals a)
                  (RawResource $ buildingConstructionProgressChemicals a)
