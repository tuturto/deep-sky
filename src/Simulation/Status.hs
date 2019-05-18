{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Simulation.Status ( removeExpiredStatuses )
    where

import Import
import CustomTypes ( PlanetaryStatus(..), StarDate )
import News.Data ( ProductionChangedNews(..), NewsArticle(..), mkNews )
import Resources ( ResourceType(..) )


-- | Remove expired statuses from database and save respective news
-- items signaling ending of statuses.
removeExpiredStatuses :: (MonadIO m, PersistQueryWrite backend,
    BaseBackend backend ~ SqlBackend) =>
    StarDate -> ReaderT backend m [News]
removeExpiredStatuses date = do
    planetNews <- processPlanetStatus date
    insertMany_ planetNews
    return planetNews


-- | Remove expired planet statuses from database and create respective
-- news articles
processPlanetStatus :: (MonadIO m, PersistQueryWrite backend,
    BaseBackend backend ~ SqlBackend) =>
    StarDate -> ReaderT backend m [News]
processPlanetStatus date = do
    loaded <- selectList [ PlanetStatusExpiration <=. Just date ] []
    deleteWhere [ PlanetStatusExpiration <=. Just date ]
    let planetIds = fmap (planetStatusPlanetId . entityVal) loaded
    planets <- selectList [ PlanetId <-. planetIds ] []
    let systemIds = fmap (planetStarSystemId . entityVal) planets
    systems <- selectList [ StarSystemId <-. systemIds ] []
    return $ mapMaybe (expirationNews planets systems date . entityVal) loaded


--TODO: performance with big set of planets and star systems
-- | Create expiration news for specific planet status
-- List of planet entities and list of star system entities are used for caching data
-- These two lists should contain information that given PlanetStatus refers to, in
-- order for this function to be able to retrieve planet's and star system's name
expirationNews :: [Entity Planet] -> [Entity StarSystem] -> StarDate -> PlanetStatus -> Maybe News
expirationNews planets systems date (PlanetStatus pId GoodHarvest _) =
    boostEnded planets systems date pId BiologicalResource Boost

expirationNews planets systems date (PlanetStatus pId PoorHarvest _) =
    boostEnded planets systems date pId BiologicalResource Slowdown

expirationNews planets systems date (PlanetStatus pId GoodMechanicals _) =
    boostEnded planets systems date pId MechanicalResource Boost

expirationNews planets systems date (PlanetStatus pId PoorMechanicals _) =
    boostEnded planets systems date pId MechanicalResource Slowdown

expirationNews planets systems date (PlanetStatus pId GoodChemicals _) =
    boostEnded planets systems date pId ChemicalResource Boost

expirationNews planets systems date (PlanetStatus pId PoorChemicals _) =
    boostEnded planets systems date pId ChemicalResource Slowdown

expirationNews _ _ _ (PlanetStatus _ KragiiAttack _) =
    Nothing


-- | News entry about boost ending
-- Will return Nothing when one of the components (planet or starsystem)
-- isn't found in provided lists or if planet doesn't currently have an
-- owner.
boostEnded :: (SemiSequence seq1, SemiSequence seq2,
    Element seq1 ~ Entity Planet, Element seq2 ~ Entity StarSystem) =>
    seq1 -> seq2 -> StarDate -> Key Planet -> ResourceType -> StatusType -> Maybe News
boostEnded planets systems date pId resource sType =
        mkNews <$> fId
               <*> Just date
               <*> case sType of
                        Boost ->
                            ProductionBoostEnded <$> content
                        Slowdown ->
                            ProductionSlowdownEnded <$> content
    where
        content =  ProductionChangedNews
                        <$> fmap entityKey planet
                        <*> fmap (planetName . entityVal) planet
                        <*> fmap entityKey system
                        <*> fmap (starSystemName . entityVal) system
                        <*> Just resource
                        <*> Just date
        fId = planet >>= (planetOwnerId . entityVal)
        planet = find (\p -> entityKey p == pId) planets
        system = find (\s -> Just (entityKey s) == fmap (planetStarSystemId . entityVal) planet) systems


data StatusType =
    Boost
    | Slowdown
