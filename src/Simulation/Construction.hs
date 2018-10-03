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

module Simulation.Construction ( handleFactionConstruction )
    where

import Import

-- | handle construction queues for given faction
handleFactionConstruction :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Time -> Entity Faction -> ReaderT backend m ()
handleFactionConstruction date faction = do
    return ()
