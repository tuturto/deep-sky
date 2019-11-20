{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


-- | Catch all module for things that don't yet belong to anywhere else
module CustomTypes
    ( SpecialEventStatus(..), BuildingType(..), Role(..)
    , PercentileChance(..), RollResult(..), Bonus(..), Boostable(..)
    , StarDate(..), Age(..), UserIdentity(..), FactionName(..)
    , SystemStatus(..), age, buildingTypeName, roll )
    where

import ClassyPrelude.Yesod   as Import
import Data.Aeson ( ToJSON(..), withScientific, withText )
import Data.Aeson.TH
import Data.Scientific ( toBoundedInteger )
import Database.Persist.Sql
import Data.Monoid ()
import Numeric.Natural ( Natural )
import System.Random


-- | Some news are special events that offer users to make choice how to handle given situation
-- This data type is used to differentiate between different kinds of news
data SpecialEventStatus =
    UnhandledSpecialEvent
    | HandledSpecialEvent
    | NoSpecialEvent
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "SpecialEventStatus"


data BuildingType = SensorStation
    | ResearchComplex
    | Farm
    | ParticleAccelerator
    | NeutronDetector
    | BlackMatterScanner
    | GravityWaveSensor
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "BuildingType"


buildingTypeName :: BuildingType -> Text
buildingTypeName bt =
    case bt of
        SensorStation -> "Sensor Station"
        ResearchComplex -> "Research Complex"
        Farm -> "Farm"
        ParticleAccelerator -> "Particle Accelerator"
        NeutronDetector -> "Neutron Detector"
        BlackMatterScanner -> "Black Matter Scanner"
        GravityWaveSensor -> "Gravity Wave Sensor"


data Role = RoleUser
          | RoleAdministrator
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Role"


newtype PercentileChance =
    PercentileChance { unPercentileChance :: Int }
    deriving (Show, Read, Eq, Ord, Num)


data RollResult =
    Success
    | Failure
    deriving (Show, Read, Eq, Enum, Bounded)


roll :: PercentileChance -> IO RollResult
roll odds = do
    result <- randomRIO (0, 100 :: Int)
    return $ if result <= unPercentileChance odds
        then Success
        else Failure


data Bonus = Bonus Int Double
    deriving (Show, Read, Eq)


instance Semigroup Bonus where
    (<>) (Bonus a0 p0) (Bonus a1 p1) =
        Bonus (a0 + a1) (p0 * p1)


instance Monoid Bonus where
    mempty = Bonus 0 1


-- | Thing that can be modified by a bonus
class Boostable a where

    -- | Apply bonus to a, scaling it accordingly
    applyBonus :: Bonus -> a -> a


newtype StarDate = StarDate { unStarDate :: Int }
    deriving (Show, Read, Eq, Num, Ord)


instance ToJSON StarDate where
    toJSON = toJSON . unStarDate


instance FromJSON StarDate where
    parseJSON =
        withScientific "star date"
            (\x -> case toBoundedInteger x of
                Nothing ->
                    mempty

                Just n ->
                    return $ StarDate n)


instance PersistField StarDate where
    toPersistValue (StarDate n) =
        PersistInt64 $ fromIntegral n

    fromPersistValue (PersistInt64 n) =
        Right $ StarDate $ fromIntegral n

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql StarDate where
    sqlType _ = SqlInt64


newtype Age = Age { unAge :: Natural }
    deriving (Show, Read, Eq, Num, Ord)


instance ToJSON Age where
    toJSON = toJSON . unAge


instance FromJSON Age where
    parseJSON =
        withScientific "age"
            (\x -> case toBoundedInteger x of
                Nothing ->
                    mempty

                Just n ->
                    if n >= 0 then
                        return $ Age $ fromIntegral (n :: Int)

                    else
                        mempty)


-- | Age (ie. difference between two star dates in whole years)
age :: StarDate -> StarDate -> Age
age (StarDate start) (StarDate end) =
    Age $ fromIntegral $ ((end - start) `quot` 10)


newtype UserIdentity = UserIdentity { unUserIdentity :: Text }
    deriving (Show, Read, Eq)


instance IsString UserIdentity where
    fromString = UserIdentity . fromString


instance ToJSON UserIdentity where
    toJSON = toJSON . unUserIdentity


instance FromJSON UserIdentity where
    parseJSON =
        withText "user identity"
            (\x -> return $ UserIdentity x)


instance PersistField UserIdentity where
    toPersistValue (UserIdentity s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ UserIdentity s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql UserIdentity where
    sqlType _ = SqlString


newtype FactionName = MkFactionName { unFactionName :: Text }
    deriving (Show, Read, Eq)


instance IsString FactionName where
    fromString = MkFactionName . fromString


instance ToJSON FactionName where
    toJSON = toJSON . unFactionName


instance FromJSON FactionName where
    parseJSON =
        withText "faction name"
            (\x -> return $ MkFactionName x)


instance PersistField FactionName where
    toPersistValue (MkFactionName s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ MkFactionName s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql FactionName where
    sqlType _ = SqlString


-- | Status of the simulation
data SystemStatus =
    Offline
    | Maintenance
    | Online
    | ProcessingTurn
    deriving (Show, Read, Eq)
derivePersistField "SystemStatus"


$(deriveJSON defaultOptions ''SpecialEventStatus)
$(deriveJSON defaultOptions ''Role)
$(deriveJSON defaultOptions ''BuildingType)
$(deriveJSON defaultOptions ''SystemStatus)
