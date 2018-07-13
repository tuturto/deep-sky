{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
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
    Maybe (a, User) -> ReaderT backend m (Int, Int, Int)
statusBarScore (Just (_, user)) = do
    faction <- getMaybeEntity $ userFactionId user
    return $ getScore faction
statusBarScore _ = do
    return (0, 0, 0)

maybeFaction :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistStoreRead backend) =>
    User -> ReaderT backend m (Maybe Faction)
maybeFaction user = do
    faction <- getMaybeEntity $ userFactionId user
    return faction

getMaybeEntity :: (PersistEntityBackend record ~ BaseBackend backend,
    PersistEntity record, PersistStoreRead backend, MonadIO m) =>
    Maybe (Key record) -> ReaderT backend m (Maybe record)
getMaybeEntity (Just factionId) = get factionId
getMaybeEntity _ = do
    return Nothing

getScore :: Maybe Faction -> (Int, Int, Int)
getScore (Just faction) = ((factionBiologicals faction), (factionMechanicals faction), (factionChemicals faction))
getScore _ = (0, 0, 0)

usersRoles :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistQueryRead backend) =>
    Key User -> ReaderT backend m [Role]
usersRoles userId = do
    roles <- selectList [ UserRoleUserId ==. userId ] []
    return $ map (\x -> userRoleRole $ entityVal x) roles

isAdmin :: (BaseBackend backend ~ SqlBackend,
    PersistQueryRead backend, MonadIO m) =>
    Key User -> ReaderT backend m Bool
isAdmin userId = do
    roles <- usersRoles userId
    return $ any (RoleAdministrator ==) roles

authorizeAdmin :: (BaseBackend (YesodPersistBackend site)
    ~
    SqlBackend,
    YesodPersist site, PersistQueryRead (YesodPersistBackend site)) =>
    Maybe (Key User) -> HandlerFor site AuthResult
authorizeAdmin (Just userId) = do
    checkAdmin <- runDB $ isAdmin userId
    let res = case checkAdmin of
                True -> Authorized
                _ -> Unauthorized "This part is only for administrators"
    return res
authorizeAdmin _ = do
    return $ Unauthorized "This part is only for administrators"

toDisplayDate :: Int -> [Char]
toDisplayDate date = printf "%.1f" $ (fromIntegral date) * (0.1 :: Double)
