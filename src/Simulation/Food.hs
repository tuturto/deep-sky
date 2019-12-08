{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Simulation.Food ( handleFactionFood, getFoodRequirement, getFoodProduction, foodRequirement
                       , foodProduction, foodBaseProduction, foodProductionBonus )
    where

import Import
import CustomTypes ( BuildingType(..), Bonus(..), applyBonus )
import Resources ( Biological(..), RawResource(..) )
import Space.Data ( PlanetaryStatus(..) )


-- | handle production and consumption of food for given faction
handleFactionFood :: (BaseBackend backend ~ SqlBackend,
    PersistStoreWrite backend, PersistQueryRead backend, MonadIO m) =>
    Entity Faction -> ReaderT backend m ()
handleFactionFood faction = do
    planets <- selectList [ PlanetOwnerId ==. Just (entityKey faction)] []
    lReqBio <- mapM (getFoodRequirement . entityKey) planets
    let reqBio = sum lReqBio
    lProdBio <- mapM (getFoodProduction . entityKey) planets
    let prodBio = sum lProdBio
    let deltaBio = prodBio - reqBio
    _ <- update (entityKey faction) [ FactionBiologicals +=. deltaBio ]
    return ()


-- | calculate amount of food a given planet requires
getFoodRequirement :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistQueryRead backend) =>
    PlanetId -> ReaderT backend m (RawResource Biological)
getFoodRequirement pid = do
    pop <- selectList [ PlanetPopulationPlanetId ==. pid ] []
    let res = foodRequirement $ fmap entityVal pop
    return res


-- | calculate amount of food a given planet produces
getFoodProduction :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistQueryRead backend) =>
    PlanetId -> ReaderT backend m (RawResource Biological)
getFoodProduction pid = do
    buildings <- selectList [ BuildingPlanetId ==. pid ] []
    statuses <- selectList [ PlanetStatusPlanetId ==. pid ] []
    return $ foodProduction (fmap entityVal buildings) (fmap (planetStatusStatus . entityVal) statuses)


-- | calculate amount of food given population requires
foodRequirement :: [PlanetPopulation] -> RawResource Biological
foodRequirement population =
    RawResource $ totalPopulation * 2
        where totalPopulation = sum $ fmap planetPopulationPopulation population


-- | total food production of given set of buildings with planetary statuses taken into account
foodProduction :: [Building] -> [PlanetaryStatus] -> RawResource Biological
foodProduction buildings statuses =
    applyBonus bonus production
        where
            production = sum $ fmap foodBaseProduction buildings
            bonus = mconcat $ fmap foodProductionBonus statuses


-- | calculate amount of food produced by a building
foodBaseProduction :: Building -> RawResource Biological
foodBaseProduction building =
    case buildingType building of
        Farm ->
            RawResource 5

        _    ->
            RawResource 0


-- | Bonus food production caused by planetary status
foodProductionBonus :: PlanetaryStatus -> Bonus
foodProductionBonus GoodHarvest = Bonus 0 1.2
foodProductionBonus PoorHarvest = Bonus 0 0.9
foodProductionBonus _ = Bonus 0 1
