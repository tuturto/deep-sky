{-# LANGUAGE NoImplicitPrelude          #-}

module Construction (Constructable(..), constructionLeft)
    where

import Import
import CustomTypes (RawResources(..), RawResource(..), ConstructionDone, ResourceCost, ConstructionLeft)

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
        mechLeft = RawResource $ (unRawResource mechCost) - (unRawResource mechDone)
        bioLeft = RawResource $ (unRawResource bioCost) - (unRawResource bioDone)
        chemLeft = RawResource $ (unRawResource chemCost) - (unRawResource chemDone)