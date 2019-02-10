{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Handler.Research
    ( getApiAvailableResearchR, getApiCurrentResearchR, postApiCurrentResearchR
    , deleteApiCurrentResearchR, getResearchR )
    where

import Import
import qualified Data.Map as Map
import Common ( apiRequireFaction, apiError, apiNotFound )
import Handler.Home ( getNewHomeR )
import Research.Data ( ResearchProgress(..), ResearchScore(..), Research(..)
                     , sameTopCategory )
import Research.Tree ( techMap )


-- | Api to retrieve currently available research
getApiAvailableResearchR :: Handler Value
getApiAvailableResearchR = do
    (_, _, fId) <- apiRequireFaction
    available <- runDB $ selectList [ AvailableResearchFactionId ==. fId ] []
    let tech = availableResearchType . entityVal <$> available
    let research = mapMaybe (`Map.lookup` techMap) tech
    return $ toJSON research


-- | Api to retrieve all research currently in progress
getApiCurrentResearchR :: Handler Value
getApiCurrentResearchR = do
    (_, _, fId) <- apiRequireFaction
    res <- runDB $ loadCurrentResearch fId
    return $ toJSON res


-- | Api to start new research
postApiCurrentResearchR :: Handler Value
postApiCurrentResearchR = do
    (_, _, fId) <- apiRequireFaction
    newRes <- requireJsonBody
    available <- runDB $ selectList [ AvailableResearchFactionId ==. fId ] []
    let validRes = validateNewResearch (entityVal <$> available) newRes
    _ <- either apiError (runDB . saveNewResearch fId) validRes
    res <- runDB $ loadCurrentResearch fId
    return $ toJSON res


-- | Api to delete research currently in progress
deleteApiCurrentResearchR :: Handler Value
deleteApiCurrentResearchR = do
    (_, _, fId) <- apiRequireFaction
    newRes <- requireJsonBody
    current <- runDB $ selectList [ CurrentResearchFactionId ==. fId
                                  , CurrentResearchType ==. (researchType . researchProgressResearch) newRes] []
    _ <- when (null current) apiNotFound
    _ <- runDB $ deleteWhere [ CurrentResearchFactionId ==. fId
                             , CurrentResearchType ==. (researchType . researchProgressResearch) newRes]
    res <- runDB $ loadCurrentResearch fId
    return $ toJSON res


-- | Save new current research in database
-- Remove all current research in progress with same top category
saveNewResearch :: (MonadIO m, PersistStoreWrite backend, PersistQueryRead backend,
    PersistQueryWrite backend, BaseBackend backend ~ SqlBackend) =>
    Key Faction -> (ResearchProgress, Research) -> ReaderT backend m (Key CurrentResearch)
saveNewResearch fId (progress, research) = do
    current <- selectList [ CurrentResearchFactionId ==. fId ] []
    let curRes = fmap (\x -> (entityKey x, Map.lookup (currentResearchType $ entityVal x) techMap)) current
    let matchingCat = mapMaybe (\(key, res) ->
                case res of
                    Nothing ->
                        Nothing

                    Just x ->
                        if researchCategory x `sameTopCategory` researchCategory research
                            then Just key
                            else Nothing) curRes

    deleteWhere [ CurrentResearchId <-. matchingCat
                , CurrentResearchFactionId ==. fId ]
    insert $ CurrentResearch
        { currentResearchType = researchType research
        , currentResearchProgress = unResearchScore $ researchProgressProgress progress
        , currentResearchFactionId = fId
        }


-- | Entry point for loading elm application and moving directly to research view
getResearchR :: Handler Html
getResearchR = getNewHomeR


-- | Validate new research candidate
-- In case of error first element is status code and second message explaining the error
-- In case of success, first element is validated research progress and second one
-- is the matching research from the tech tree
validateNewResearch :: [AvailableResearch] -> ResearchProgress -> Either (Status, Text) (ResearchProgress, Research)
validateNewResearch available res =
    canBeIdentified res
        >>= researchMatchesTechTree
        >>= zeroProgress
        >>= availableResearch available


-- | New research should be one that can be identified in tech tree
canBeIdentified :: ResearchProgress -> Either (Status, Text) (ResearchProgress, Research)
canBeIdentified res =
        case research of
            Just x ->
                Right (res, x)

            Nothing ->
                Left (status400, "Unknown technology in research")
    where
        tech = researchType . researchProgressResearch $ res
        research = Map.lookup tech techMap


-- | Research should match one in tech tree
researchMatchesTechTree :: (ResearchProgress, Research) -> Either (Status, Text) (ResearchProgress, Research)
researchMatchesTechTree val@(progress, res) =
    if researchProgressResearch progress == res
        then Right val
        else Left (status400, "Research doesn't match the one in tree")


-- | New research should have zero progress done
zeroProgress :: (ResearchProgress, Research) -> Either (Status, Text) (ResearchProgress, Research)
zeroProgress val@(progress, _) =
    if researchProgressProgress progress == ResearchScore 0
        then Right val
        else Left (status400, "Research should have zero progress")


-- | New research should be one of those that are available
availableResearch :: [AvailableResearch] -> (ResearchProgress, Research) -> Either (Status, Text) (ResearchProgress, Research)
availableResearch available val@(progress, _) =
    if prop `elem` known
        then Right val
        else Left (status400, "Tried to research unknown technology")
    where
        known = fmap availableResearchType available
        prop = researchType $ researchProgressResearch progress


-- | Load faction's current research
loadCurrentResearch :: (PersistQueryRead backend, MonadIO m,
    BaseBackend backend ~ SqlBackend) =>
    Key Faction -> ReaderT backend m [ResearchProgress]
loadCurrentResearch fId = do
    current <- selectList [ CurrentResearchFactionId ==. fId ] []
    let res = catMaybes $ currentToProgress <$> current
    return res


-- | Map current research into research progress
-- this might result into Nothing, as lookup to techMap can produce Nothing
currentToProgress :: Entity CurrentResearch -> Maybe ResearchProgress
currentToProgress (Entity _ curr) =
    ResearchProgress <$> research <*> Just rLeft
    where
        tech = currentResearchType curr
        research = Map.lookup tech techMap
        rLeft = ResearchScore $ currentResearchProgress curr
