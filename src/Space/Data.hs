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
    ( StarName(..), PlanetName(..), StarSystemName(..)
    )
    where

import Data.Aeson ( ToJSON(..), withText )
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
