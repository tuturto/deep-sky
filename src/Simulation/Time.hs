{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}

module Simulation.Time (advanceTime)
    where

import Import
import Data.Maybe (fromJust)
import Database.Persist.Sql (toSqlKey)
import CustomTypes ( StarDate(..) )

-- | advance time stored in database by one (decimal) month
advanceTime :: (BaseBackend backend ~ SqlBackend,
    PersistQueryRead backend, PersistStoreWrite backend, MonadIO m) =>
    ReaderT backend m StarDate
advanceTime = do
    let timeId = toSqlKey 1
    time <- get timeId
    _ <- update timeId [ SimulationCurrentTime =. (simulationCurrentTime (fromJust time) + 1)]
    time' <- selectFirst [] []
    let res = case time' of
                (Just t) ->
                    simulationCurrentTime $ entityVal t

                _ ->
                    StarDate 0
    return res
