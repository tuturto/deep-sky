{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleContexts           #-}

module MenuHelpers
    ( starDate, systemNameById, planetNameById, statusBarScore, getMaybeEntity
    , getScore, usersRoles, isAdmin, authorizeAdmin, toDisplayDate )
    where

import Model
import Import.NoFoundation
import Database.Persist.Sql (toSqlKey)
import CustomTypes
import Text.Printf (printf)
import Resources ( RawResources(..), ResourcesAvailable(..) )


-- | Current star date of the simulation
starDate :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistQueryRead backend) =>
    ReaderT backend m StarDate
starDate = do
    simulation <- get (toSqlKey 1)
    return $ maybe 0 simulationCurrentTime simulation


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
    dbAvatar <- case userAvatar user of
                    Nothing ->
                        return Nothing

                    Just aId ->
                        get aId
    let fId = join $ personFactionId <$> dbAvatar
    faction <- case fId of
                Nothing ->
                    return Nothing

                Just x ->
                    get x
    return $ getScore faction
statusBarScore _ =
    return mempty


getMaybeEntity :: (PersistEntityBackend record ~ BaseBackend backend,
    PersistEntity record, PersistStoreRead backend, MonadIO m) =>
    Maybe (Key record) -> ReaderT backend m (Maybe record)
getMaybeEntity (Just factionId) = get factionId
getMaybeEntity _ =
    return Nothing

-- TODO: better name and place
getScore :: Maybe Faction -> RawResources ResourcesAvailable
getScore (Just faction) = RawResources (factionMechanicals faction) (factionBiologicals faction) (factionChemicals faction)
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


-- | Format star date for displaying it
toDisplayDate :: StarDate -> String
toDisplayDate date =
    printf "%.1f" $ fromIntegral (unStarDate date) * (0.1 :: Double)
