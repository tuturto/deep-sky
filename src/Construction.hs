{-# LANGUAGE NoImplicitPrelude          #-}

module Construction ( Constructable(..), constructionLeft
                    , ConstructionSpeedCoeff(..), OverallConstructionSpeed(..)
                    , speedLimitedByOverallSpeed, resourceScaledBySpeed, constructionWillFinish 
                    , speedLimitedByWorkLeft )
    where

import Import
import CustomTypes ( RawResources(..), RawResource(..), ConstructionDone, ResourceCost, ConstructionLeft
                   , Biological, Mechanical, Chemical, ConstructionSpeed )

-- | Object that can be placed in construction queue
class Constructable a where
    cIndex :: a -> Int
    cProgress :: a -> RawResources ConstructionDone

instance Constructable BuildingConstruction where
    cIndex = buildingConstructionIndex
    cProgress a =
        RawResources (RawResource $ buildingConstructionProgressMechanicals a)
                     (RawResource $ buildingConstructionProgressBiologicals a)
                     (RawResource $ buildingConstructionProgressChemicals a)

-- | How much construction is there left based on total cost and construction done
constructionLeft :: RawResources ResourceCost -> RawResources ConstructionDone -> RawResources ConstructionLeft
constructionLeft (RawResources mechCost bioCost chemCost) (RawResources mechDone bioDone chemDone) =
    RawResources mechLeft bioLeft chemLeft
    where
        mechLeft = RawResource $ unRawResource mechCost - unRawResource mechDone
        bioLeft = RawResource $ unRawResource bioCost - unRawResource bioDone
        chemLeft = RawResource $ unRawResource chemCost - unRawResource chemDone

-- | Speed coefficient used to scale construction speed
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

-- | Speed coefficients for all major resources
data OverallConstructionSpeed = OverallConstructionSpeed
    { overallSpeedBiological :: ConstructionSpeedCoeff Biological
    , overallSpeedMechanical :: ConstructionSpeedCoeff Mechanical
    , overallSpeedChemical :: ConstructionSpeedCoeff Chemical
    }
    deriving (Show, Read, Eq)

-- | Construction speed based on overall construction speed and maximum construction speed
speedLimitedByOverallSpeed :: OverallConstructionSpeed -> RawResources ConstructionSpeed -> RawResources ConstructionSpeed
speedLimitedByOverallSpeed coeffSpeed speed =
    RawResources { ccdBiologicalCost = resourceScaledBySpeed (ccdBiologicalCost speed) (overallSpeedBiological coeffSpeed)
                 , ccdMechanicalCost = resourceScaledBySpeed (ccdMechanicalCost speed) (overallSpeedMechanical coeffSpeed)
                 , ccdChemicalCost = resourceScaledBySpeed (ccdChemicalCost speed) (overallSpeedChemical coeffSpeed)
                 }

-- | Raw resource scaled by construction speed coefficient
resourceScaledBySpeed :: RawResource t -> ConstructionSpeedCoeff t -> RawResource t
resourceScaledBySpeed res NormalConstructionSpeed =
    res
resourceScaledBySpeed res (LimitedConstructionSpeed speed) =
    RawResource $ floor $ fromIntegral (unRawResource res) * speed


-- | Limit construction speed to amount that there's work left to do
speedLimitedByWorkLeft :: RawResources ConstructionSpeed -> BuildingConstruction -> RawResources ResourceCost -> RawResources ConstructionSpeed
speedLimitedByWorkLeft cSpeed bConst cost =
    let
        limitPerResource progress speed total =
            if RawResource progress + speed > total
                then total - RawResource progress
                else speed
        bioSpeed = limitPerResource (buildingConstructionProgressBiologicals bConst)
                                    (ccdBiologicalCost cSpeed)
                                    (ccdBiologicalCost cost)
        mechSpeed = limitPerResource (buildingConstructionProgressMechanicals bConst)
                                     (ccdMechanicalCost cSpeed)
                                     (ccdMechanicalCost cost)
        chemSpeed = limitPerResource (buildingConstructionProgressChemicals bConst)
                                     (ccdChemicalCost cSpeed)
                                     (ccdChemicalCost cost)
    in
        RawResources { 
              ccdMechanicalCost = mechSpeed
            , ccdBiologicalCost = bioSpeed
            , ccdChemicalCost = chemSpeed
            }

-- | Will construction finish based on speed, progress so far and required construction
constructionWillFinish :: RawResources ConstructionSpeed -> RawResources ConstructionDone -> RawResources ResourceCost -> Bool
constructionWillFinish speed progress total = 
    ccdMechanicalCost speed >= ccdMechanicalCost total - ccdMechanicalCost progress 
    && ccdBiologicalCost speed >= ccdBiologicalCost total - ccdBiologicalCost progress
    && ccdChemicalCost speed >= ccdChemicalCost total - ccdChemicalCost progress