{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE LambdaCase                 #-}

module Space.Data
    ( StarName(..), PlanetName(..), StarSystemName(..), SpectralType(..)
    , LuminosityClass(..), Coordinates(..), PlanetaryStatus(..)
    )
    where

import Data.Aeson ( ToJSON(..), withText )
import Data.Aeson.TH
import Database.Persist.Sql
import ClassyPrelude.Yesod   as Import


newtype StarName = MkStarName { unStarName :: Text }
    deriving (Show, Read, Eq)


instance IsString StarName where
    fromString = MkStarName . fromString


instance ToJSON StarName where
    toJSON = toJSON . unStarName


instance FromJSON StarName where
    parseJSON =
        withText "star name"
            (\x -> return $ MkStarName x)


instance PersistField StarName where
    toPersistValue (MkStarName s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ MkStarName s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql StarName where
    sqlType _ = SqlString


newtype PlanetName = MkPlanetName { unPlanetName :: Text }
    deriving (Show, Read, Eq)


instance IsString PlanetName where
    fromString = MkPlanetName . fromString


instance ToJSON PlanetName where
    toJSON = toJSON . unPlanetName


instance FromJSON PlanetName where
    parseJSON =
        withText "planet name"
            (\x -> return $ MkPlanetName x)


instance PersistField PlanetName where
    toPersistValue (MkPlanetName s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ MkPlanetName s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql PlanetName where
    sqlType _ = SqlString


newtype StarSystemName = MkStarSystemName { unStarSystemName :: Text }
    deriving (Show, Read, Eq)


instance IsString StarSystemName where
    fromString = MkStarSystemName . fromString


instance ToJSON StarSystemName where
    toJSON = toJSON . unStarSystemName


instance FromJSON StarSystemName where
    parseJSON =
        withText "star system name"
            (\x -> return $ MkStarSystemName x)


instance PersistField StarSystemName where
    toPersistValue (MkStarSystemName s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ MkStarSystemName s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql StarSystemName where
    sqlType _ = SqlString


data SpectralType = O | B | A | F | G | K | M | L | T
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "SpectralType"


data LuminosityClass = Iap | Ia | Iab | Ib | II | III | IV | V | VI | VII
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "LuminosityClass"


data Coordinates = Coordinates Double Double
    deriving (Show, Read, Eq)


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


$(deriveJSON defaultOptions ''Coordinates)
$(deriveJSON defaultOptions ''SpectralType)
$(deriveJSON defaultOptions ''LuminosityClass)
$(deriveJSON defaultOptions ''PlanetaryStatus)
