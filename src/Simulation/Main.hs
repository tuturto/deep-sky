{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Simulation.Main (processTurn)
    where

import Import
import Simulation.Construction ( handleFactionConstruction )
import Simulation.Events ( handleFactionEvents, addSpecialEvents )
import Simulation.Food ( handleFactionFood )
import Simulation.Observations ( handleFactionObservations )
import Simulation.Research ( handleFactionResearch )
import Simulation.Status ( removeExpiredStatuses )
import Simulation.Time


-- | simulate a single step
processTurn :: (BaseBackend backend ~ SqlBackend,
    BackendCompatible SqlBackend backend, PersistUniqueRead backend,
    PersistQueryWrite backend,
    PersistQueryRead backend, PersistStoreWrite backend, MonadIO m) =>
    ReaderT backend m Time
processTurn = do
    newTime <- advanceTime
    _ <- removeExpiredStatuses newTime
    factions <- selectList [] [ Asc FactionId ]
    mapM_ (handleFactionEvents newTime) factions
    mapM_ handleFactionFood factions
    mapM_ (handleFactionConstruction newTime) factions
    mapM_ (handleFactionResearch newTime) factions
    mapM_ (addSpecialEvents newTime) factions
    -- Doing observations should always be done last to ensure players have
    -- recent reports of property they have full control, ie. planets.
    -- Otherwise it's possible that they'll receive reports that are one
    -- turn out of sync.
    mapM_ (handleFactionObservations newTime) factions
    return newTime
