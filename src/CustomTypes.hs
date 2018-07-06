{-# LANGUAGE TemplateHaskell #-}

module CustomTypes where

import Database.Persist.TH
import Text.Blaze.Html5

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

data Component = Sensors
               | SubSpaceSensors
               | TachyonSensors
               | IonEngine
               
