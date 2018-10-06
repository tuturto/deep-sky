{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Construction
    where

import Import
import CustomTypes (TotalCost(..), Cost(..))

class Constructable a where
    cIndex :: a -> Int
    cCost :: a -> TotalCost

instance Constructable BuildingConstruction where
    cIndex = buildingConstructionIndex
    cCost a =
        TotalCost (Cost $ buildingConstructionProgressMechanicals a)
                  (Cost $ buildingConstructionProgressBiologicals a)
                  (Cost $ buildingConstructionProgressChemicals a)
