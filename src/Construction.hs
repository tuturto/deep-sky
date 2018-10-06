{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Construction (ConstructionSpeed(..), Constructable(..))
    where

import Import
import CustomTypes (TotalCost(..), Cost(..))

class Constructable a where
    cIndex :: a -> Int
    cCost :: a -> TotalCost

data ConstructionSpeed = ConstructionSpeed
    { constructionSpeedMechanicalCost :: Cost
    , constructionSpeedBiologicalCost :: Cost
    , constructionSpeedChemicalCost :: Cost }
    deriving (Show, Read, Eq)

instance Constructable BuildingConstruction where
    cIndex = buildingConstructionIndex
    cCost a =
        TotalCost (Cost $ buildingConstructionProgressMechanicals a)
                  (Cost $ buildingConstructionProgressBiologicals a)
                  (Cost $ buildingConstructionProgressChemicals a)
