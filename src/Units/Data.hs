{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Units.Data ( ShipName(..), ShipType(..)
                  , CrewPosition(..), CrewSpaceReq(..), CrewRank(..)
                  , CrewAmount(..), VehicleName(..), Band(..)
                  , UnitObservationDetailsJSON(..), StatsReportDetails(..)
                  , TotalCrewSpace(..), CrewSpace(..), SteerageQuarters(..)
                  , StandardQuarters(..), LuxuryQuarters(..)
                  , CrewRequirement(..), UnitName(..), DesignName(..)
                  , unCrewAmountL
                  , crewRequirementPositionL, crewRequirementAmountL
                  , crewRequirementRankL, unCrewSpaceL
                  , totalCrewSpaceSteerageL, totalCrewSpaceStandardL
                  , totalCrewSpaceLuxuryL )
    where

import ClassyPrelude.Yesod   as Import
import Control.Lens ( Lens', lens )
import Control.Lens.TH
import Data.Aeson ( ToJSON(..), withText, withScientific, withObject )
import Data.Aeson.TH
import Data.Scientific ( toBoundedInteger )
import Database.Persist.Sql


newtype DesignName = MkDesignName { unDesignName :: Text }
    deriving (Show, Read, Eq)


instance ToJSON DesignName where
    toJSON = toJSON . unDesignName


instance FromJSON DesignName where
    parseJSON =
        withText "Design name"
            (\x -> return $ MkDesignName x)


instance PersistField DesignName where
    toPersistValue (MkDesignName s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ MkDesignName s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql DesignName where
    sqlType _ = SqlString


instance IsString DesignName where
    fromString = MkDesignName . fromString


-- | Name of ship
newtype ShipName = MkShipName { unShipName :: Text }
    deriving (Show, Read, Eq)


instance ToJSON ShipName where
    toJSON = toJSON . unShipName


instance FromJSON ShipName where
    parseJSON =
        withText "Ship name"
            (\x -> return $ MkShipName x)


instance PersistField ShipName where
    toPersistValue (MkShipName s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ MkShipName s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql ShipName where
    sqlType _ = SqlString


instance IsString ShipName where
    fromString = MkShipName . fromString


-- | Name of vehicle
newtype VehicleName = MkVehicleName { unVehicleName :: Text }
    deriving (Show, Read, Eq)


instance ToJSON VehicleName where
    toJSON = toJSON . unVehicleName


instance FromJSON VehicleName where
    parseJSON =
        withText "vehicle name"
            (\x -> return $ MkVehicleName x)


instance PersistField VehicleName where
    toPersistValue (MkVehicleName s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ MkVehicleName s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql VehicleName where
    sqlType _ = SqlString


instance IsString VehicleName where
    fromString = MkVehicleName . fromString


-- | Name of unit
newtype UnitName = MkUnitName { unUnitName :: Text }
    deriving (Show, Read, Eq)


instance ToJSON UnitName where
    toJSON = toJSON . unUnitName


instance FromJSON UnitName where
    parseJSON =
        withText "unit name"
            (\x -> return $ MkUnitName x)


instance IsString UnitName where
    fromString = MkUnitName . fromString


data ShipType =
    Bawley
    | Bilander
    | BlackwallFrigate
    | Brigantine
    | Caravel
    | Clipper
    | Cog
    | Corvette
    | CraneShip
    | CruiseLiner
    | Freighter
    | Flyboat
    | Frigate
    | Galleon
    | ManOfWar
    | SatelliteLayer
    | Schooner
    | Yawl
    | MobileBase
    | Station
    | Satellite
    deriving (Show, Read, Eq, Bounded, Ord, Enum)
derivePersistField "ShipType"


data CrewPosition =
    Commander
    | Navigator
    | Signaler
    | SensorOperator
    | Gunner
    | Doctor
    | Nurse
    | Driver
    | Helmsman
    | Artificer
    | Crew
    | Passenger
    deriving (Show, Read, Eq, Bounded, Ord, Enum)
derivePersistField "CrewPosition"


data CrewRank =
    SecondClass
    | FirstClass
    | Senior
    | Chief
    deriving (Show, Read, Eq, Bounded, Ord, Enum)
derivePersistField "CrewRank"


-- | Does crew require quarters
data CrewSpaceReq =
    CrewSpaceRequired
    | CrewSpaceOptional
    deriving (Show, Read, Eq, Bounded, Ord, Enum)
derivePersistField "CrewSpaceReq"


newtype CrewAmount = CrewAmount { unCrewAmount :: Int }
    deriving (Show, Read, Eq, Ord, Num)


makeLensesFor [ ("unCrewAmount", "unCrewAmountL") ] ''CrewAmount


instance ToJSON CrewAmount where
    toJSON = toJSON . unCrewAmount


instance FromJSON CrewAmount where
    parseJSON =
        withScientific "crew amount"
                       (\x -> case toBoundedInteger x of
                            Nothing ->
                                mempty

                            Just n ->
                                return $ CrewAmount n)


instance PersistField CrewAmount where
    toPersistValue (CrewAmount n) =
        PersistInt64 $ fromIntegral n

    fromPersistValue (PersistInt64 n) =
        Right $ CrewAmount $ fromIntegral n

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql CrewAmount where
    sqlType _ = SqlInt64


-- | Band denotes distance from a planet or other celestial object
-- BandSurface indicates surface of the object.
-- BandOrbit indicates being on orbit
-- Bands A and B are at immediate vicinity. In case or asteroid fields, these
-- are inside of the field.
-- Bands C, D and E are at close proximity.
-- Bands F and G are in deep space.
data Band
    = BandSurface
    | BandOrbit
    | BandA
    | BandB
    | BandC
    | BandD
    | BandE
    | BandF
    | BandG
    deriving (Show, Read, Eq, Bounded, Ord, Enum)
derivePersistField "Band"


-- | newtype used to classify Text as JSON storing UnitObservationDetails
newtype UnitObservationDetailsJSON =
    MkUnitObservationDetailsJSON { unUnitObservationDetailsJSON :: Text }
    deriving (Show, Read, Eq)


instance PersistField UnitObservationDetailsJSON where
    toPersistValue (MkUnitObservationDetailsJSON s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ MkUnitObservationDetailsJSON s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql UnitObservationDetailsJSON where
    sqlType _ = SqlString


instance ToJSON UnitObservationDetailsJSON where
    toJSON = toJSON . unUnitObservationDetailsJSON


instance FromJSON UnitObservationDetailsJSON where
    parseJSON =
        withText "Unit observation details json"
            (\x -> return $ MkUnitObservationDetailsJSON x)


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


-- | StatsReportDetails is used to record observed stats of unit
-- | Since not all stats are always known, all fields are Maybe a
-- | where Nothing indicates that particular aspect was not observed.
data StatsReportDetails = StatsReportDetails
    { statsReportDetailsMinimumCrew :: Maybe [CrewRequirement]
    , statsReportDetailsNominalCrew :: Maybe [CrewRequirement]
    , statsReportDetailsCrewSpace :: Maybe TotalCrewSpace
    , statsReportDetailsCrewSpaceRequired :: Maybe CrewSpaceReq
    }
    deriving (Show, Read, Eq)
derivePersistField "StatsReportDetails"


-- | StatsReportDetails forms semigroup, where combining a and b will
-- always yield a if it's Just, otherwise b.
-- This is useful when collating multiple stats reports where some values
-- might not be present.
instance Semigroup StatsReportDetails where
    a <> b =
        StatsReportDetails
            { statsReportDetailsMinimumCrew = statsReportDetailsMinimumCrew a <|> statsReportDetailsMinimumCrew b
            , statsReportDetailsNominalCrew = statsReportDetailsNominalCrew a <|> statsReportDetailsNominalCrew b
            , statsReportDetailsCrewSpace = statsReportDetailsCrewSpace a <|> statsReportDetailsCrewSpace b
            , statsReportDetailsCrewSpaceRequired = statsReportDetailsCrewSpaceRequired a <|> statsReportDetailsCrewSpaceRequired b
            }


-- | StatsReportDetails forms a monoid, where zero element is report without any
-- information at all.
instance Monoid StatsReportDetails where
    mempty =
        StatsReportDetails
            { statsReportDetailsMinimumCrew = Nothing
            , statsReportDetailsNominalCrew = Nothing
            , statsReportDetailsCrewSpace = Nothing
            , statsReportDetailsCrewSpaceRequired = Nothing
            }


$(deriveJSON defaultOptions ''ShipType)
$(deriveJSON defaultOptions ''CrewPosition)
$(deriveJSON defaultOptions ''CrewSpaceReq)
$(deriveJSON defaultOptions ''CrewRank)
$(deriveJSON defaultOptions ''Band)
$(deriveJSON defaultOptions ''CrewRequirement)
$(deriveJSON defaultOptions ''CrewSpace)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 18 } ''StatsReportDetails)


makeLensesFor [("unCrewSpace", "unCrewSpaceL")] ''CrewSpace
makeLensesFor [ ("totalCrewSpaceSteerage", "totalCrewSpaceSteerageL")
              , ("totalCrewSpaceStandard", "totalCrewSpaceStandardL")
              , ("totalCrewSpaceLuxury", "totalCrewSpaceLuxuryL")] ''TotalCrewSpace
