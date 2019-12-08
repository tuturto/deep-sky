{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Simulation.Research ( handleFactionResearch )
    where

import Import
import Control.Lens ( (+~) )
import System.Random
import Common ( mkUniq, getR, entityValL )
import CustomTypes ( StarDate )
import News.Import ( researchCompleted )
import Queries ( factionBuildings )
import Research.Data ( TotalResearchScore(..), ResearchProduction(..), ResearchCategory(..)
                     , ResearchScore(..), TechTree(..), Research(..), ResearchLimit(..)
                     , isEngineering, isNaturalScience, isSocialScience, topCategory )
import Research.Import ( researchOutput, researchReady, antecedentsAvailable
                       , currentResearchProgressL )
import Research.Tree ( techTree, techMap )



-- | Process all research that faction can do
handleFactionResearch :: (MonadIO m,
    BackendCompatible SqlBackend backend, PersistUniqueRead backend,
    PersistQueryWrite backend, BaseBackend backend ~ SqlBackend) =>
    StarDate -> Entity Faction -> ReaderT backend m ()
handleFactionResearch date faction = do
    production <- totalProduction $ entityKey faction
    current <- selectList [ CurrentResearchFactionId ==. entityKey faction ] []
    let updated = updateProgress production <$> current
    _ <- updateUnfinished updated
    _ <- handleCompleted date updated $ entityKey faction
    _ <- updateAvailableResearch $ entityKey faction
    return ()


-- | total research production of a faction
totalProduction :: (MonadIO m,
    BackendCompatible SqlBackend backend, PersistQueryRead backend,
    PersistUniqueRead backend) =>
    FactionId -> ReaderT backend m (TotalResearchScore ResearchProduction)
totalProduction fId = do
    pnbs <- factionBuildings fId
    let buildings = join $ fmap snd pnbs
    return $ mconcat $ researchOutput . entityVal <$> buildings


-- | removed research that has been completed from current research and
-- insert respective details into completed research. Also remove respective
-- entry from available researches. Create new article for each completed research.
handleCompleted :: (MonadIO m, PersistQueryWrite backend,
    BaseBackend backend ~ SqlBackend) =>
    StarDate -> [Entity CurrentResearch] -> FactionId -> ReaderT backend m ()
handleCompleted date updated fId = do
    let finished = filter (researchReady . entityVal) updated
    let finishedTech = currentResearchType . entityVal <$> finished
    insertMany_ $ currentToCompleted date . entityVal <$> finished
    insertMany_ $ researchCompleted date fId . (currentResearchType . entityVal) <$> finished
    deleteWhere [ CurrentResearchId <-. fmap entityKey finished ]
    deleteWhere [ AvailableResearchType <-. finishedTech
                , AvailableResearchFactionId ==. fId ]


-- | Map current research into completed research
currentToCompleted :: StarDate -> CurrentResearch -> CompletedResearch
currentToCompleted date research =
    CompletedResearch
        { completedResearchType = currentResearchType research
        , completedResearchLevel = 1
        , completedResearchFactionId = currentResearchFactionId research
        , completedResearchDate = date
        }


-- | Current research after given amount of research has been done
updateProgress :: TotalResearchScore ResearchProduction -> Entity CurrentResearch -> Entity CurrentResearch
updateProgress prod curr =
    case researchCategory research of
        Engineering _ ->
            entityValL . currentResearchProgressL +~ engResearch $ curr

        NaturalScience _ ->
            entityValL . currentResearchProgressL +~ natResearch $ curr

        SocialScience _ ->
            entityValL . currentResearchProgressL +~ socResearch $ curr
    where
        research = techMap (currentResearchType . entityVal $ curr)
        engResearch = unResearchScore $ totalResearchScoreEngineering prod
        natResearch = unResearchScore $ totalResearchScoreNatural prod
        socResearch = unResearchScore $ totalResearchScoreSocial prod


-- | Update current research in database for unfinished research
updateUnfinished :: (MonadIO m, PersistStoreWrite backend,
    Traversable t, IsSequence (t (Entity record)), PersistEntity record,
    Element (t (Entity record)) ~ Entity CurrentResearch,
    PersistEntityBackend record ~ BaseBackend backend) =>
    t (Entity record) -> ReaderT backend m (t ())
updateUnfinished updated = do
    let unfinished = filter (not . researchReady . entityVal) updated
    mapM (\x -> replace (entityKey x) (entityVal x)) unfinished


-- | Add new available research when required
updateAvailableResearch :: (MonadIO m, PersistQueryWrite backend,
    BaseBackend backend ~ SqlBackend) =>
    FactionId -> ReaderT backend m ()
updateAvailableResearch fId = do
    available <- selectList [ AvailableResearchFactionId ==. fId ] []
    completed <- selectList [ CompletedResearchFactionId ==. fId ] []
    g <- liftIO newStdGen
    let maxAvailable = ResearchLimit 3
    -- reusing same g should not have adverse effect here
    let engCand = getR g (unResearchLimit maxAvailable) $ newAvailableResearch isEngineering maxAvailable available completed
    let natCand = getR g (unResearchLimit maxAvailable) $ newAvailableResearch isNaturalScience maxAvailable available completed
    let socCand = getR g (unResearchLimit maxAvailable) $ newAvailableResearch isSocialScience maxAvailable available completed
    rewriteAvailableResearch fId $ engCand <> natCand <> socCand


-- | Remove old available research and insert new ones
-- | Research categories of existing researches are used to determine which
-- | available researches should be removed from the database and replaced
-- | with new ones
rewriteAvailableResearch :: (MonadIO m, PersistQueryWrite backend,
    BaseBackend backend ~ SqlBackend) =>
    FactionId -> [Research] -> ReaderT backend m ()
rewriteAvailableResearch fId res = do
    let cats = mkUniq $ fmap (topCategory . researchCategory) res
    unless (null cats) $ do
        deleteWhere [ AvailableResearchFactionId ==. fId
                    , AvailableResearchCategory <-. cats ]
        insertMany_ $ researchToAvailable fId <$> res


researchToAvailable :: FactionId -> Research -> AvailableResearch
researchToAvailable fId res =
    AvailableResearch
        { availableResearchType = researchType res
        , availableResearchCategory = topCategory $ researchCategory res
        , availableResearchFactionId = fId
        }


-- | Figure out if new set of research should be made available to a player. Function
-- factors in current research limit, currently available research and research that has
-- already been completed.
newAvailableResearch :: (ResearchCategory -> Bool) -> ResearchLimit -> [Entity AvailableResearch]
    -> [Entity CompletedResearch] -> [Research]
newAvailableResearch selector limit available completed =
    if ResearchLimit (length specificCategory) >= limit
        then []
        else candidates
    where
        specificCategory = filter (availableResearchFilter selector) available
        knownTech = completedResearchType . entityVal <$> completed
        unlockedResearch = filter (antecedentsAvailable knownTech) $ unTechTree techTree
        unlockedAndUnresearched = filter (\x -> researchType x `notElem` knownTech) unlockedResearch
        candidates = filter (selector . researchCategory) unlockedAndUnresearched


-- | Is given available research entity of certain research category
availableResearchFilter :: (ResearchCategory -> Bool) -> Entity AvailableResearch -> Bool
availableResearchFilter f x =
    f . researchCategory $ res
    where
        res = techMap (availableResearchType $ entityVal x)
