{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Handler.Designer ( getDesignerR, getApiComponentsR, getApiChassisR
                        , getApiDesignR, postApiDesignR, putApiDesignIdR
                        , deleteApiDesignIdR, postApiDoDesignEstimateR )
    where

import Import
import Control.Monad.Random ( evalRand, newStdGen )
import Database.Persist.Sql (toSqlKey)

import Data.Maybe ( fromJust, maybe )
import Common ( apiRequireFaction, mkUniq, apiRequireOpenSimulation
              , apiRequireViewSimulation )
import CustomTypes ( StarDate )
import Dto.Ship ( DesignDto(..), designToDesignDto, toChassisDto
                , componentDtoToPlannedComponent )
import Handler.Home ( getNewHomeR )
import MenuHelpers ( starDate )
import News.Import ( designCreatedNews )
import Queries ( chassisList )
import Research.Data ( Technology )
import Units.Components ( ComponentLevel(..), ComponentId(..), Component
                        , components, requirements )
import Units.Stats ( estimateDesign )


getDesignerR :: Handler Html
getDesignerR = getNewHomeR


-- | Get list of all components that currently logged in user has access to
getApiComponentsR :: Handler Value
getApiComponentsR = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    completed <- runDB $ selectList [ CompletedResearchFactionId ==. fId ] []
    let comps = mkUniq . join $ toComponent <$> completed
    return $ toJSON comps


-- | Given a list of technology requirements, technology map and completed research, figure our which
-- components are available
componentLookup :: [(Maybe Technology, ComponentId)] -> (ComponentLevel -> ComponentId -> Component)
    -> CompletedResearch -> [Component]
componentLookup reqs comps completed =
    (comps level) . snd <$> enabled
    where
        tech = completedResearchType completed
        level = ComponentLevel $ completedResearchLevel completed
        enabled = filter (\(rtech, _) -> maybe True ((==) tech) rtech) reqs


-- | turn completed research into list of components it enables or don't have tech requirement
toComponent :: Entity CompletedResearch -> [Component]
toComponent = (componentLookup requirements components) . entityVal


-- | Get list of all chassis that currently logged in user has access to
getApiChassisR :: Handler Value
getApiChassisR = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    chassisRequirements <- runDB $ chassisList fId
    return $ toJSON $ toChassisDto <$> chassisRequirements


-- | Get details of all designs that currently logged in user has access to
getApiDesignR :: Handler Value
getApiDesignR = do
    -- TODO: use esqueleto and refactor into own function
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    loadedDesigns <- runDB $ selectList [ DesignOwnerId ==. fId ] []
    loadedComponents <- runDB $ selectList [ PlannedComponentDesignId <-. map entityKey loadedDesigns ] []
    let designs = map (\d -> designToDesignDto (entityKey d, entityVal d)
                    $ filter (\c -> plannedComponentDesignId (entityVal c) == entityKey d) loadedComponents)
                    loadedDesigns
    return $ toJSON designs


-- | Create a new design
postApiDesignR :: Handler Value
postApiDesignR = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireOpenSimulation uId
    msg <- requireJsonBody
    if validateSaveDesign msg then
        (do date <- runDB starDate
            savedDesign <- runDB $ saveDesign date msg fId
            return $ toJSON savedDesign)
        else sendResponseStatus status400 ("Validation failed" :: Text)


-- | Update existing design
putApiDesignIdR :: DesignId -> Handler Value
putApiDesignIdR dId = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireOpenSimulation uId
    msg <- requireJsonBody
    if validateSaveDesign msg then
        (do savedDesign <- runDB $ updateDesign dId msg fId
            return $ toJSON savedDesign)
        else sendResponseStatus status400 ("Validation failed" :: Text)


-- | Permanently delete design
deleteApiDesignIdR :: DesignId -> Handler Value
deleteApiDesignIdR dId = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireOpenSimulation uId
    --TODO: allow deleting only faction's designs
    _ <- runDB $ deleteDesign dId
    loadedDesigns <- runDB $ selectList [ DesignOwnerId ==. fId ] []
    loadedComponents <- runDB $ selectList [ PlannedComponentDesignId <-. map entityKey loadedDesigns ] []
    let designs = map (\d -> designToDesignDto (entityKey d, entityVal d)
                    $ filter (\c -> plannedComponentDesignId (entityVal c) == entityKey d) loadedComponents)
                    loadedDesigns
    return $ toJSON designs


-- | Validate that given design is valid
validateSaveDesign :: DesignDto -> Bool
-- TODO: implement validating designs
validateSaveDesign _ = True


-- | Save given design into database and create a news article about it
saveDesign :: (MonadIO m, PersistStoreWrite backend, PersistQueryRead backend,
    BaseBackend backend ~ SqlBackend) =>
    StarDate -> DesignDto -> FactionId -> ReaderT backend m DesignDto
saveDesign date design fId = do
    newId <- insert $ Design (designDtoName design) fId (designDtoChassisId design)
    _ <- mapM (insert . componentDtoToPlannedComponent newId) (designDtoComponents design)
    newDesign <- get newId
    newComponents <- selectList [ PlannedComponentDesignId ==. newId ] []
    let x = designToDesignDto (newId, fromJust newDesign) newComponents
    _ <- insert $ designCreatedNews (Entity newId $ fromJust newDesign) date fId
    return x


-- | Update existing design in database
updateDesign :: (MonadIO m, PersistStoreWrite backend, PersistQueryRead backend,
    PersistQueryWrite backend, BaseBackend backend ~ SqlBackend) =>
    DesignId -> DesignDto -> FactionId -> ReaderT backend m DesignDto
updateDesign dId design fId = do
    _ <- replace dId $ Design (designDtoName design) fId (designDtoChassisId design)
    deleteWhere [ PlannedComponentDesignId ==. dId ]
    _ <- mapM (insert . componentDtoToPlannedComponent dId) (designDtoComponents design)
    newDesign <- get dId
    newComponents <- selectList [ PlannedComponentDesignId ==. dId ] []
    let x = designToDesignDto (dId, fromJust newDesign) newComponents
    return x


-- | Delete design and related components from database
deleteDesign :: (MonadIO m, PersistQueryWrite backend,
                 BaseBackend backend ~ SqlBackend) =>
                DesignId -> ReaderT backend m ()
deleteDesign dId = do
    deleteWhere [ PlannedComponentDesignId ==. dId ]
    delete dId


-- | Estimate stats of a design being worked
postApiDoDesignEstimateR :: Handler Value
postApiDoDesignEstimateR = do
    (uId, _, _, _) <- apiRequireFaction
    _ <- apiRequireOpenSimulation uId
    msg <- requireJsonBody
    let dId = case designDtoId msg of
                Nothing ->
                    toSqlKey 0

                Just n ->
                    n

    dbChassis <- runDB $ get $ designDtoChassisId msg
    chassis <- case dbChassis of
                Nothing ->
                    sendStatusJSON status400 ("Chassis not found" :: Text)

                Just x ->
                    return x

    let comps = fmap (componentDtoToPlannedComponent dId) $ designDtoComponents msg
    g <- liftIO newStdGen
    let estimate = evalRand (estimateDesign chassis comps) g

    return $ toJSON estimate
