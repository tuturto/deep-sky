{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


-- | Catch all module for things that don't yet belong to anywhere else
module CustomTypes where

import Data.Aeson.TH
import Database.Persist.TH
import Text.Blaze.Html5 (ToMarkup, toMarkup)
import Data.Text
import ClassyPrelude.Yesod   as Import
import Data.Monoid ()
import System.Random


-- | Some news are special events that offer users to make choice how to handle given situation
-- This data type is used to differentiate between different kinds of news
data SpecialEventStatus =
    UnhandledSpecialEvent
    | HandledSpecialEvent
    | NoSpecialEvent
    deriving (Show, Read, Eq)
derivePersistField "SpecialEventStatus"


data SpectralType = O | B | A | F | G | K | M | L | T
    deriving (Show, Read, Eq)
derivePersistField "SpectralType"


data LuminosityClass = Iap | Ia | Iab | Ib | II | III | IV | V | VI | VII
    deriving (Show, Read, Eq)
derivePersistField "LuminosityClass"



data Coordinates = Coordinates Double Double
    deriving (Show, Eq)


data BuildingType = SensorStation
    | ResearchComplex
    | Farm
    | ParticleAccelerator
    | NeutronDetector
    | BlackMatterScanner
    | GravityWaveSensor
    deriving (Show, Read, Eq)
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


instance ToMarkup BuildingType where
    toMarkup building =
        case building of
            SensorStation       -> toMarkup ("Sensor station" :: Text)
            ResearchComplex     -> toMarkup ("Research complex" :: Text)
            Farm                -> toMarkup ("Farm" :: Text)
            ParticleAccelerator -> toMarkup ("Particle accelerator" :: Text)
            NeutronDetector     -> toMarkup ("Neutron detector" :: Text)
            BlackMatterScanner  -> toMarkup ("Black matter scanner" :: Text)
            GravityWaveSensor   -> toMarkup ("Gravity wave sensor" :: Text)


data ShipType = Satellite
    | Fighter
    | Destroyer
    | Frigate
    | Cruiser
    | BattleShip
    | MobileBase
    | Station
    deriving (Show, Read, Eq)
derivePersistField "ShipType"


instance ToMarkup ShipType where
    toMarkup shipType =
        case shipType of
            Satellite   -> toMarkup ("Satellite" :: Text)
            Fighter     -> toMarkup ("Fighter" :: Text)
            Destroyer   -> toMarkup ("Destroyer" :: Text)
            Frigate     -> toMarkup ("Frigate" :: Text)
            Cruiser     -> toMarkup ("Cruiser" :: Text)
            BattleShip  -> toMarkup ("Battleship" :: Text)
            MobileBase  -> toMarkup ("Mobile base" :: Text)
            Station     -> toMarkup ("Station" :: Text)


data Role = RoleUser
          | RoleAdministrator
    deriving (Show, Read, Eq)
derivePersistField "Role"


instance ToMarkup Role where
    toMarkup RoleUser = toMarkup ("User" :: Text)
    toMarkup RoleAdministrator = toMarkup ("Administrator" :: Text)


-- TODO: move into components?
data ComponentSlot = InnerSlot
    | OuterSlot
    | ArmourSlot
    | SensorSlot
    | WeaponSlot
    | EngineSlot
    deriving (Show, Read, Eq)
derivePersistField "ComponentSlot"


newtype PercentileChance =
    PercentileChance { unPercentileChance :: Int }
    deriving (Show, Read, Eq, Ord, Num)


data RollResult =
    Success
    | Failure
    deriving (Show, Read, Eq)


roll :: PercentileChance -> IO RollResult
roll odds = do
    result <- randomRIO (0, 100 :: Int)
    return $ if result <= unPercentileChance odds
        then Success
        else Failure


-- | Status codes for various events that might affect a planet
data PlanetaryStatus =
    GoodHarvest
    | PoorHarvest
    | GoodMechanicals
    | PoorMechanicals
    | GoodChemicals
    | PoorChemicals
    | KragiiAttack
    deriving (Show, Read, Eq, Enum, Bounded, Ord)
derivePersistField "PlanetaryStatus"


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


$(deriveJSON defaultOptions ''SpecialEventStatus)
$(deriveJSON defaultOptions ''Role)
$(deriveJSON defaultOptions ''Coordinates)
$(deriveJSON defaultOptions ''SpectralType)
$(deriveJSON defaultOptions ''LuminosityClass)
$(deriveJSON defaultOptions ''BuildingType)
$(deriveJSON defaultOptions ''ComponentSlot)
$(deriveJSON defaultOptions ''ShipType)
$(deriveJSON defaultOptions ''PlanetaryStatus)
