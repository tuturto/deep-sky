{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CustomTypes where

import Data.Aeson.TH
import Database.Persist.TH
import Text.Blaze.Html5 (ToMarkup, toMarkup)
import Data.Text

data SpectralType = O | B | A | F | G | K | M | L | T
    deriving (Show, Read, Eq)
derivePersistField "SpectralType"

data LuminosityClass = Iap | Ia | Iab | Ib | II | III | IV | V | VI | VII
    deriving (Show, Read, Eq)
derivePersistField "LuminosityClass"

data Coordinates = Coordinates Double Double
    deriving (Show, Eq)

instance ToMarkup Coordinates where
    toMarkup (Coordinates x y) = toMarkup $ "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

data BuildingType = SensorStation
                  | ResearchComplex
                  | Farm
    deriving (Show, Read, Eq)
derivePersistField "BuildingType"

instance ToMarkup BuildingType where
    toMarkup building = case building of
                        SensorStation   -> toMarkup ("Sensor station" :: Text)
                        ResearchComplex -> toMarkup ("Research complex" :: Text)
                        Farm            -> toMarkup ("Farm" :: Text)

data ComponentType = Sensors
                   | CombatSensors
                   | Armour
                   | FerrousArmour
                   | SubSpaceSensors
                   | TachyonSensors
                   | IonEngine
                   | LifeSupport
                   | CrewQuarters
                   | Bridge
    deriving (Show, Read, Eq)
derivePersistField "ComponentType"

instance ToMarkup ComponentType where
    toMarkup component = 
        case component of
            Sensors         -> toMarkup ("Sensors" :: Text)
            CombatSensors   -> toMarkup ("Combat Sensors" :: Text)
            Armour          -> toMarkup ("Armour" :: Text)
            FerrousArmour   -> toMarkup ("Ferrous Armour" :: Text)
            SubSpaceSensors -> toMarkup ("Sub-Space Sensors" :: Text)
            TachyonSensors  -> toMarkup ("Tachyon Sensors" :: Text)
            IonEngine       -> toMarkup ("Ion Engine" :: Text)
            LifeSupport     -> toMarkup ("Life Support" :: Text)
            CrewQuarters    -> toMarkup ("Crew Quarters" :: Text)
            Bridge          -> toMarkup ("Bridge" :: Text)

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

$(deriveJSON defaultOptions ''Role)
$(deriveJSON defaultOptions ''Coordinates)
$(deriveJSON defaultOptions ''SpectralType)
$(deriveJSON defaultOptions ''LuminosityClass)
$(deriveJSON defaultOptions ''BuildingType)
$(deriveJSON defaultOptions ''ComponentType)
$(deriveJSON defaultOptions ''ShipType)
