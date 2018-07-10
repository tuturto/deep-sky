{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module MenuHelpers where

import Model
import Import.NoFoundation
import Database.Persist.Sql (toSqlKey)
import CustomTypes
import Text.Printf (printf)

starDate :: (BaseBackend (YesodPersistBackend site) 
    ~ 
    SqlBackend, 
    YesodPersist site, 
    PersistStoreRead (YesodPersistBackend site)) => 
    HandlerFor site Time
starDate = do
    systemTime <- runDB $ get (toSqlKey 1) 
    let res = case systemTime of
                (Just x) -> x
                Nothing  -> Time 0
    return res

systemNameById :: (BaseBackend (YesodPersistBackend site) 
    ~ 
    SqlBackend, 
    YesodPersist site, 
    PersistStoreRead (YesodPersistBackend site)) => 
    Key StarSystem -> HandlerFor site Text
systemNameById systemId = do
    system <- runDB $ get systemId
    let name = case system of
                        (Just x) -> starSystemName x
                        Nothing  -> "Unknown"
    return name

planetNameById :: (BaseBackend (YesodPersistBackend site) 
    ~ 
    SqlBackend, 
    YesodPersist site, 
    PersistStoreRead (YesodPersistBackend site)) => 
    Key Planet -> HandlerFor site Text
planetNameById planetId = do
    planet <- runDB $ get planetId
    let name = case planet of
                        (Just x) -> planetName x
                        Nothing  -> "Unknown"
    return name

statusBarScore :: (BaseBackend (YesodPersistBackend site) 
    ~ 
    SqlBackend, 
    YesodPersist site, 
    PersistStoreRead (YesodPersistBackend site)) => 
    (Maybe (UserId, User)) -> HandlerFor site (Int, Int, Int)
statusBarScore (Just (_, user)) = do
    faction <- getFaction $ userFactionId user
    return $ getScore faction
statusBarScore _ = do
    return (0, 0, 0)

maybeFaction :: (BaseBackend (YesodPersistBackend site)
    ~
    SqlBackend,
    PersistStoreRead (YesodPersistBackend site), YesodPersist site) =>
    User -> HandlerFor site (Maybe Faction)
maybeFaction user = do
    faction <- getFaction $ userFactionId user
    return faction

getFaction :: (BaseBackend (YesodPersistBackend site) 
    ~ 
    SqlBackend, 
    YesodPersist site, 
    PersistStoreRead (YesodPersistBackend site)) => 
    (Maybe (Key Faction)) -> HandlerFor site (Maybe Faction)
getFaction (Just factionId) = runDB $ get factionId
getFaction _ = do
    return Nothing

getScore :: Maybe Faction -> (Int, Int, Int)
getScore (Just faction) = ((factionBiologicals faction), (factionMechanicals faction), (factionChemicals faction))
getScore _ = (0, 0, 0)

usersRoles :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
    YesodPersist site, PersistQueryRead (YesodPersistBackend site)) =>
    Key User -> HandlerFor site [Role]
usersRoles userId = do
    roles <- runDB $ selectList [ UserRoleUserId ==. userId ] []
    return $ map (\x -> userRoleRole $ entityVal x) roles

isAdmin :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
    PersistQueryRead (YesodPersistBackend site), YesodPersist site) =>
    Key User -> HandlerFor site Bool
isAdmin userId = do
    roles <- usersRoles userId
    return $ any (RoleAdministrator ==) roles

authorizeAdmin :: (BaseBackend (YesodPersistBackend site)
    ~
    SqlBackend,
    YesodPersist site, PersistQueryRead (YesodPersistBackend site)) =>
    Maybe (Key User) -> HandlerFor site AuthResult
authorizeAdmin (Just userId) = do
    checkAdmin <- isAdmin userId
    let res = case checkAdmin of
                True -> Authorized
                _ -> Unauthorized "This part is only for administrators"
    return res
authorizeAdmin _ = do
    return $ Unauthorized "This part is only for administrators"

toDisplayDate :: Int -> [Char]
toDisplayDate date = printf "%.1f" $ (fromIntegral date) * (0.1 :: Double)
