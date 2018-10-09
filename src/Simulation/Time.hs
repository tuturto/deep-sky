{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Simulation.Time (advanceTime)
    where

import Import
import Data.Maybe (fromJust)
import Database.Persist.Sql (toSqlKey)

-- | advance time stored in database by one (decimal) month
advanceTime :: (BaseBackend backend ~ SqlBackend,
    PersistQueryRead backend, PersistStoreWrite backend, MonadIO m) =>
    ReaderT backend m Time
advanceTime = do
    let timeId = toSqlKey 1
    time <- get timeId
    _ <- update timeId [ TimeCurrentTime =. (timeCurrentTime (fromJust time) + 1)]
    time' <- selectFirst [] []
    let res = case time' of
                (Just t) -> entityVal t
                _ -> Time 0
    return res
