{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleContexts           #-}

module MenuHelpers where

import Model
import Import.NoFoundation
import Database.Persist.Sql (toSqlKey)
import CustomTypes
import Text.Printf (printf)

starDate :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistQueryRead backend) =>
    ReaderT backend m Time
starDate = do
    systemTime <- get (toSqlKey 1) 
    let res = case systemTime of
                (Just x) -> x
                Nothing  -> Time 0
    return res

systemNameById :: (BaseBackend backend ~ SqlBackend,
    PersistStoreRead backend, MonadIO m) =>
    Key StarSystem -> ReaderT backend m Text
systemNameById systemId = do
    system <- get systemId
    let name = case system of
                        (Just x) -> starSystemName x
                        Nothing  -> "Unknown"
    return name

planetNameById :: (BaseBackend backend ~ SqlBackend,
    PersistStoreRead backend, MonadIO m) =>
    Key Planet -> ReaderT backend m Text
planetNameById planetId = do
    planet <- get planetId
    let name = case planet of
                        (Just x) -> planetName x
                        Nothing  -> "Unknown"
    return name

statusBarScore :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistStoreRead backend) =>
    Maybe (a, User) -> ReaderT backend m (RawResources ResourcesAvailable)
statusBarScore (Just (_, user)) = do
    faction <- getMaybeEntity $ userFactionId user
    return $ getScore faction
statusBarScore _ = 
    return mempty

maybeFaction :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistStoreRead backend) =>
    User -> ReaderT backend m (Maybe Faction)
maybeFaction user = 
    getMaybeEntity $ userFactionId user
    

getMaybeEntity :: (PersistEntityBackend record ~ BaseBackend backend,
    PersistEntity record, PersistStoreRead backend, MonadIO m) =>
    Maybe (Key record) -> ReaderT backend m (Maybe record)
getMaybeEntity (Just factionId) = get factionId
getMaybeEntity _ = 
    return Nothing

-- TODO: better name and place
getScore :: Maybe Faction -> RawResources ResourcesAvailable
getScore (Just faction) = RawResources (RawResource $ factionBiologicals faction) (RawResource $ factionMechanicals faction) (RawResource $ factionChemicals faction)
getScore _ = mempty 

usersRoles :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistQueryRead backend) =>
    Key User -> ReaderT backend m [Role]
usersRoles userId = do
    roles <- selectList [ UserRoleUserId ==. userId ] []
    return $ map (userRoleRole . entityVal) roles

isAdmin :: (BaseBackend backend ~ SqlBackend,
    PersistQueryRead backend, MonadIO m) =>
    Key User -> ReaderT backend m Bool
isAdmin userId = do
    roles <- usersRoles userId
    return $ elem RoleAdministrator roles

authorizeAdmin :: (BaseBackend (YesodPersistBackend site)
    ~
    SqlBackend,
    YesodPersist site, PersistQueryRead (YesodPersistBackend site)) =>
    Maybe (Key User) -> HandlerFor site AuthResult
authorizeAdmin (Just userId) = do
    checkAdmin <- runDB $ isAdmin userId
    let res = if checkAdmin then Authorized
                else Unauthorized "This part is only for administrators"
    return res
authorizeAdmin _ = 
    return $ Unauthorized "This part is only for administrators"

toDisplayDate :: Int -> String
toDisplayDate date = printf "%.1f" $ fromIntegral date * (0.1 :: Double)
