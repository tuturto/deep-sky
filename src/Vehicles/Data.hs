{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Vehicles.Data ( ShipName(..), ShipType(..), LandingStatus(..)
                     , CrewPosition(..), CrewSpaceReq(..), CrewRank(..)
                     , CrewAmount(..), unCrewAmountL )
    where

import ClassyPrelude.Yesod   as Import
import Control.Lens.TH
import Data.Aeson ( ToJSON(..), withText, withScientific )
import Data.Aeson.TH
import Data.Scientific ( toBoundedInteger )
import Database.Persist.Sql


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


data LandingStatus =
    ShipOnPlanet
    | ShipInOrbit
    deriving (Show, Read, Eq)
derivePersistField "LandingStatus"


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
    deriving (Show, Read, Eq, Enum, Bounded)
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
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "CrewPosition"


data CrewRank =
    SecondClass
    | FirstClass
    | Senior
    | Chief
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "CrewRank"


-- | Does crew require quarters
data CrewSpaceReq =
    CrewSpaceRequired
    | CrewSpaceOptional
    deriving (Show, Read, Eq)
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


$(deriveJSON defaultOptions ''ShipType)
$(deriveJSON defaultOptions ''LandingStatus)
$(deriveJSON defaultOptions ''CrewPosition)
$(deriveJSON defaultOptions ''CrewSpaceReq)
$(deriveJSON defaultOptions ''CrewRank)
