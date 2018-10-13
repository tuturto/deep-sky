{-# LANGUAGE NoImplicitPrelude          #-}

module Construction ( Constructable(..), constructionLeft
                    , ConstructionSpeedCoeff(..), OverallConstructionSpeed(..) )
    where

import Import
import CustomTypes ( RawResources(..), RawResource(..), ConstructionDone, ResourceCost, ConstructionLeft
                   , Biological, Mechanical, Chemical )

class Constructable a where
    cIndex :: a -> Int
    cProgress :: a -> RawResources ConstructionDone

instance Constructable BuildingConstruction where
    cIndex = buildingConstructionIndex
    cProgress a =
        RawResources (RawResource $ buildingConstructionProgressMechanicals a)
                     (RawResource $ buildingConstructionProgressBiologicals a)
                     (RawResource $ buildingConstructionProgressChemicals a)

constructionLeft :: RawResources ResourceCost -> RawResources ConstructionDone -> RawResources ConstructionLeft
constructionLeft (RawResources mechCost bioCost chemCost) (RawResources mechDone bioDone chemDone) =
    RawResources mechLeft bioLeft chemLeft
    where
        mechLeft = RawResource $ unRawResource mechCost - unRawResource mechDone
        bioLeft = RawResource $ unRawResource bioCost - unRawResource bioDone
        chemLeft = RawResource $ unRawResource chemCost - unRawResource chemDone

data ConstructionSpeedCoeff t = NormalConstructionSpeed
    | LimitedConstructionSpeed Double
    deriving (Show, Read, Eq)

instance Ord (ConstructionSpeedCoeff t) where
    (<=) (LimitedConstructionSpeed a) NormalConstructionSpeed =
        a <= 1.0
    (<=) (LimitedConstructionSpeed a) (LimitedConstructionSpeed b) =
        a <= b
    (<=) NormalConstructionSpeed NormalConstructionSpeed = True
    (<=) NormalConstructionSpeed (LimitedConstructionSpeed a) =
        a >= 1.0

data OverallConstructionSpeed = OverallConstructionSpeed
    { overallSpeedBiological :: ConstructionSpeedCoeff Biological
    , overallSpeedMechanical :: ConstructionSpeedCoeff Mechanical
    , overallSpeedChemical :: ConstructionSpeedCoeff Chemical
    }
    deriving (Show, Read, Eq)