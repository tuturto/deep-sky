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

module Simulation.Time where

import Import
import Database.Persist.Sql (toSqlKey)

-- | advance time stored in database by one (decimal) month
advanceTime :: (BaseBackend backend ~ SqlBackend,
    PersistQueryRead backend, PersistStoreWrite backend, MonadIO m) =>
    ReaderT backend m Time
advanceTime = do
    let timeId = toSqlKey 1
    time <- get timeId
    _ <- case time of
            (Just t) -> update timeId [ TimeCurrentTime =. (timeCurrentTime t + 1)]
    time' <- selectFirst [] []
    let res = case time' of
                (Just t) -> entityVal t
                _ -> Time 0
    return res
