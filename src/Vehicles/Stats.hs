{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Vehicles.Stats
    ( CrewRequirement(..), CrewSpace(..), UnitStats(..), QuartersQuality(..)
    , TotalCrewSpace(..), SteerageQuarters(..), StandardQuarters(..)
    , LuxuryQuarters(..)
    , unitMinCrew, unitNominalCrew, unitCrewSpace, estimateDesign
    , designMinCrew, designNominalCrew, designCrewSpace
    , seniorityRanks, crewRequirementAmountL, crewRequirementRankL
    , crewRequirementPositionL, quarterCrew, compsToCrew, plannedToCompPair
    , unCrewSpaceL, totalCrewSpaceSteerageL, totalCrewSpaceStandardL
    , totalCrewSpaceLuxuryL, filterZeroCrews
    )
    where

import Import
import Control.Lens ( Lens', lens, (%~), (&), (+~), (^.), traverse )
import Control.Lens.TH
import Control.Monad.Random ( Rand )
import Data.Aeson ( ToJSON(..), withObject )
import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import qualified Prelude as P
import System.Random

import Vehicles.Components ( Component(..), ComponentId(..)
                           , ComponentAmount(..), components )
import Vehicles.Data ( CrewPosition(..), CrewSpaceReq(..), CrewRank(..)
                     , CrewAmount(..), unCrewAmountL )
import Vehicles.Queries ( Unit'(..) )


data CrewRequirement =
    CrewRequirement CrewPosition CrewRank CrewAmount
    deriving (Show, Read, Eq)


crewRequirementPositionL :: Lens' CrewRequirement CrewPosition
crewRequirementPositionL = lens (\(CrewRequirement p _ _) -> p)
                                (\(CrewRequirement _ r a) p -> CrewRequirement p r a)

crewRequirementAmountL :: Lens' CrewRequirement CrewAmount
crewRequirementAmountL = lens (\(CrewRequirement _ _ a) -> a)
                              (\(CrewRequirement p r _) a -> CrewRequirement p r a)

crewRequirementRankL :: Lens' CrewRequirement CrewRank
crewRequirementRankL = lens (\(CrewRequirement _ r _) -> r)
                            (\(CrewRequirement p _ a) r -> CrewRequirement p r a)

-- | Space of specific quality for crew
data CrewSpace a =
    CrewSpace { unCrewSpace :: CrewAmount }
    deriving (Show, Read, Eq)


instance Num (CrewSpace a) where
    (CrewSpace a) + (CrewSpace b) =
        CrewSpace (a + b)

    (CrewSpace a) * (CrewSpace b) =
        CrewSpace (a * b)

    abs (CrewSpace a) =
        CrewSpace (abs a)

    signum (CrewSpace n)
        | n < 0 = -1
        | n == 0 = 0
        | otherwise = 1

    fromInteger n =
        CrewSpace $ CrewAmount $ fromIntegral n

    (CrewSpace a) - (CrewSpace b) =
        CrewSpace (a - b)


instance Semigroup (CrewSpace a) where
    (<>) = (+)


instance Monoid (CrewSpace a) where
    mempty = 0


data SteerageQuarters = SteerageQuarters
    deriving (Show, Read, Eq)

data StandardQuarters = StandardQuarters
    deriving (Show, Read, Eq)

data LuxuryQuarters = LuxuryQuarters
    deriving (Show, Read, Eq)


-- | Total space for crew, categorized by quality of quarters
data TotalCrewSpace = TotalCrewSpace
    { totalCrewSpaceSteerage :: !(CrewSpace SteerageQuarters)
    , totalCrewSpaceStandard :: !(CrewSpace StandardQuarters)
    , totalCrewSpaceLuxury :: !(CrewSpace LuxuryQuarters)
    } deriving (Show, Read, Eq)


instance Semigroup TotalCrewSpace where
    a <> b =
        TotalCrewSpace
        { totalCrewSpaceSteerage = totalCrewSpaceSteerage a <> totalCrewSpaceSteerage b
        , totalCrewSpaceStandard = totalCrewSpaceStandard a <> totalCrewSpaceStandard b
        , totalCrewSpaceLuxury = totalCrewSpaceLuxury a <> totalCrewSpaceLuxury b
        }


instance Monoid TotalCrewSpace where
    mempty =
        TotalCrewSpace
        { totalCrewSpaceSteerage = 0
        , totalCrewSpaceStandard = 0
        , totalCrewSpaceLuxury = 0
        }


instance ToJSON TotalCrewSpace where
    toJSON tSpace =
        object [ "Steerage" .= (unCrewSpace . totalCrewSpaceSteerage) tSpace
               , "Standard" .= (unCrewSpace . totalCrewSpaceStandard) tSpace
               , "Luxury" .= (unCrewSpace . totalCrewSpaceLuxury) tSpace
               ]


instance FromJSON TotalCrewSpace where
    parseJSON = withObject "total crew space" $ \o -> do
        steerage <- o .: "Steerage"
        standard <- o .: "Standard"
        luxury <- o .: "Luxury"
        return $ TotalCrewSpace
            { totalCrewSpaceSteerage = CrewSpace steerage
            , totalCrewSpaceStandard = CrewSpace standard
            , totalCrewSpaceLuxury = CrewSpace luxury
            }


-- | Quality of quarters
data QuartersQuality =
    LowQuality
    | MediumQuality
    | HighQuality
    deriving (Show, Read, Eq)


-- | Performance statistics of a unit or design
data UnitStats = UnitStats
    { unitStatsMinimumCrew :: ![CrewRequirement]
    , unitStatsNominalCrew :: ![CrewRequirement]
    , unitStatsCrewSpace :: !TotalCrewSpace
    , unitStatsCrewSpaceRequired :: !CrewSpaceReq
    } deriving (Show, Read, Eq)


-- | Estimate performance of design
estimateDesign :: RandomGen g => Chassis -> [PlannedComponent] -> Rand g UnitStats
estimateDesign chassis comps = do
    let cPairs = plannedToCompPair <$> comps
    let mCrewSpace = designMinCrew cPairs
    let nCrewSpace = designNominalCrew cPairs
    let tCrewSpace = designCrewSpace comps

    return UnitStats { unitStatsMinimumCrew = mCrewSpace
                     , unitStatsNominalCrew = nCrewSpace
                     , unitStatsCrewSpace = tCrewSpace
                     , unitStatsCrewSpaceRequired = chassisCrewSpaceRequired chassis
                     }


-- | Crew required for nominal performance
designNominalCrew :: [(PlannedComponent, Component)] -> [CrewRequirement]
designNominalCrew compPairs =
    CrewRequirement Commander Chief 1 : (filterZeroCrews $ join $ addSeniorityRanks <$> crew)
    where
        crew = compsToCrew $ join (mkTotal <$> compPairs)
        mkTotal = \(plan, comp) ->
                    P.replicate (unComponentAmount $ plannedComponentAmount plan) comp


-- | Crew required for minimum performance
designMinCrew :: [(PlannedComponent, Component)] -> [CrewRequirement]
designMinCrew compPairs =
    filterZeroCrews $ quarterCrew $ designNominalCrew compPairs


-- | Quarter of given crew, rounded up
quarterCrew :: [CrewRequirement] -> [CrewRequirement]
quarterCrew reqs =
    reqs & traverse . crewRequirementAmountL . unCrewAmountL %~ (\a -> ceiling $ (fromIntegral a :: Double) / 4)


-- | Filter out requirements with zero crew
filterZeroCrews :: [CrewRequirement] -> [CrewRequirement]
filterZeroCrews =
    filter (\x -> x ^. crewRequirementAmountL > 0)


-- | Maximum amount of space available for crew
designCrewSpace :: [PlannedComponent] -> TotalCrewSpace
designCrewSpace comps =
    mconcat $ providedCrewSpace <$> allIds
    where
        allIds = join $ mkTotal <$> comps
        mkTotal = \plan ->
            P.replicate (unComponentAmount $ plannedComponentAmount plan) (plannedComponentComponentId plan)


-- | What kind of crew a unit needs to operate
unitMinCrew :: [Component] -> [CrewRequirement]
unitMinCrew comps =
    quarterCrew $ unitNominalCrew comps


-- | Crew required for optimal performance
unitNominalCrew :: [Component] -> [CrewRequirement]
unitNominalCrew comps =
    join $ addSeniorityRanks <$> crew
    where
        crew = compsToCrew comps


-- | Maximum amount of crew unit can have
unitCrewSpace :: Unit' -> Chassis -> [InstalledComponent] -> TotalCrewSpace
unitCrewSpace _ _ comps =
    mconcat $ (providedCrewSpace . installedComponentComponentId) <$> comps


-- | Amount of crew space given component provides
providedCrewSpace :: ComponentId -> TotalCrewSpace
providedCrewSpace cId =
    case cId of
        ShipLongRangeSensors ->
            TotalCrewSpace 0 0 0

        ShipShortRangeSensors ->
            TotalCrewSpace 0 0 0

        ShipArmour ->
            TotalCrewSpace 0 0 0

        ShipHeavyArmour ->
            TotalCrewSpace 0 0 0

        ShipBridge ->
            TotalCrewSpace 0 0 0

        ShipSupplyPod ->
            TotalCrewSpace 0 0 0

        ShipStarSail ->
            TotalCrewSpace 0 0 0

        ShipSteerageQuarters ->
            TotalCrewSpace 50 0 0

        ShipStandardQuarters ->
            TotalCrewSpace 0 20 0

        ShipLuxuryQuarters ->
            TotalCrewSpace 0 0 5

        ShipInfantryBay ->
            TotalCrewSpace 0 0 0 --TODO: return to this when working with transporting troops (infantry, cavalry, etc.)

        VehicleWheeledMotiveSystem ->
            TotalCrewSpace 0 0 0

        VehicleTrackedMotiveSystem ->
            TotalCrewSpace 0 0 0

        VehicleHoverMotiveSystem ->
            TotalCrewSpace 0 0 0


-- | Map from installed component to tuple of installed component and component
installedToCompPair :: InstalledComponent -> (InstalledComponent, Component)
installedToCompPair iComp =
    ( iComp, components (installedComponentLevel iComp)
                        (installedComponentComponentId iComp) )


-- | Map from planned component to tuple of planned component and component
plannedToCompPair :: PlannedComponent -> (PlannedComponent, Component)
plannedToCompPair iComp =
    ( iComp, components (plannedComponentLevel iComp)
                        (plannedComponentComponentId iComp) )


-- | Crew requirements for a component
data ComponentCrewReq =
    ComponentCrewReq CrewPosition Double
    deriving (Show, Read, Eq)


componentCrewReqPositionL :: Lens' ComponentCrewReq CrewPosition
componentCrewReqPositionL = lens (\(ComponentCrewReq p _ ) -> p)
                                (\(ComponentCrewReq _ a) p -> ComponentCrewReq p a)


componentCrewReqAmountL :: Lens' ComponentCrewReq Double
componentCrewReqAmountL = lens (\(ComponentCrewReq _ a) -> a)
                              (\(ComponentCrewReq p _) a -> ComponentCrewReq p a)


-- | Nominal crew required to operate given components
compsToCrew :: [Component] -> [CrewRequirement]
compsToCrew comps =
    compReqToCrewReq <$> combinedReqs
    where
        combinedReqs = foldr addReq noReqs compReqs
        compReqs = join $ compToCrewReq <$> comps
        noReqs = (\x -> ComponentCrewReq x 0.0) <$> [minBound..]


-- | Given list of crew requirements, compute total amount of crew needed
-- this includes regular ranks and senior ranks
addSeniorityRanks :: CrewRequirement -> [CrewRequirement]
addSeniorityRanks crew =
    crew : seniorityRanks crew


-- | Given sinle position in crew, compute amount of senior ranks required
seniorityRanks :: CrewRequirement -> [CrewRequirement]
seniorityRanks (CrewRequirement Commander _ _) =
    -- there can be only one commander
    []

seniorityRanks (CrewRequirement p SecondClass a) =
    -- for second class rank, add required seniority ranks
    -- 5 second class crew -> chief
    -- after 10 second class crew -> 1st class for every 5 2nd class
    -- every 5 1st class -> senior
    catMaybes $ cRanks : sRanks : fRanks : []
    where
        amount = unCrewAmount a
        cRanks = if amount >= 5
                    then mkReq p Chief 1
                    else Nothing
        sRanks = mkReq p Senior (CrewAmount (fRankCount `div` 5))
        fRankCount = if amount < 10
                        then 0
                        else amount `div` 5
        fRanks = mkReq p FirstClass $ CrewAmount fRankCount


seniorityRanks _ =
    -- other thank second class ranks aren't used in computation
    []


mkReq :: CrewPosition -> CrewRank -> CrewAmount -> Maybe CrewRequirement
mkReq p r a =
    if a <= 0
        then Nothing
        else Just $ CrewRequirement p r a


-- | Map component crew requirement to crew requirement
-- since amount is double, it's converted to CrewAmount and rounded up
-- ie. Artificer 0.1 -> Artificer 1
compReqToCrewReq :: ComponentCrewReq -> CrewRequirement
compReqToCrewReq (ComponentCrewReq p n) =
    CrewRequirement p SecondClass amount
    where
        amount = CrewAmount $ ceiling n


-- | Add component crew requirement to list of existing ones
-- adding is done by finding matching crew position and adding values together
addReq :: ComponentCrewReq -> [ComponentCrewReq] -> [ComponentCrewReq]
addReq (ComponentCrewReq p a) reqs =
    (match & (traverse . componentCrewReqAmountL +~ a)) ++ noMatch
    where
        (match, noMatch) = partition (\x -> (x ^. componentCrewReqPositionL) == p) reqs


-- | Crew requirements imposed by a component
compToCrewReq :: Component -> [ComponentCrewReq]
compToCrewReq comp =
    case (componentId comp) of
        ShipLongRangeSensors ->
            [ ComponentCrewReq Artificer 0.1
            , ComponentCrewReq Crew 1.0
            , ComponentCrewReq SensorOperator 1.0
            ]

        ShipShortRangeSensors ->
            [ ComponentCrewReq Artificer 0.1
            , ComponentCrewReq Crew 1.0
            , ComponentCrewReq SensorOperator 1.0
            ]

        ShipArmour ->
            []

        ShipHeavyArmour ->
            []

        ShipBridge ->
            [ ComponentCrewReq Artificer 1
            , ComponentCrewReq Crew 5
            , ComponentCrewReq Signaler 1
            , ComponentCrewReq Navigator 1
            , ComponentCrewReq Helmsman 1
            ]

        ShipSupplyPod ->
            []

        ShipStarSail ->
            [ ComponentCrewReq Crew 5 ]

        ShipSteerageQuarters ->
            []

        ShipStandardQuarters ->
            []

        ShipLuxuryQuarters ->
            [ ComponentCrewReq Crew 1
            , ComponentCrewReq Artificer 0.5
            ]

        ShipInfantryBay ->
            [ ComponentCrewReq Crew 2
            , ComponentCrewReq Artificer 0.2
            ]

        VehicleWheeledMotiveSystem ->
            []

        VehicleTrackedMotiveSystem ->
            []

        VehicleHoverMotiveSystem ->
            []


$(deriveJSON defaultOptions ''CrewRequirement)
$(deriveJSON defaultOptions ''CrewSpace)
$(deriveJSON defaultOptions ''QuartersQuality)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 9 } ''UnitStats)

makeLensesFor [("unCrewSpace", "unCrewSpaceL")] ''CrewSpace

makeLensesFor [ ("totalCrewSpaceSteerage", "totalCrewSpaceSteerageL")
              , ("totalCrewSpaceStandard", "totalCrewSpaceStandardL")
              , ("totalCrewSpaceLuxury", "totalCrewSpaceLuxuryL")] ''TotalCrewSpace
