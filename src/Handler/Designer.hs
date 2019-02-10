{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Handler.Designer ( getDesignerR, getApiComponentsR, getApiChassisR, getApiDesignR
                        , postApiDesignR, putApiDesignIdR, deleteApiDesignIdR )
    where

import Database.Persist.Sql (fromSqlKey)
import Data.Maybe (fromJust)
import Dto.Ship
import Import
import Components
import News.Import (designCreatedNews)
import MenuHelpers (starDate)
import Common (apiRequireFaction)


getDesignerR :: Handler Html
getDesignerR =
    defaultLayout $ do
        setTitle "Deep Sky - Ship designer"
        addStylesheet $ StaticR css_site_css
        $(widgetFile "shipdesigner")


-- | Get list of all components that currently logged in user has access to
getApiComponentsR :: Handler Value
getApiComponentsR = do
    _ <- apiRequireFaction
    let json = toJSON [ component CidArmour $ CLevel 1
                      , component CidEngine $ CLevel 1
                      , component CidBridge $ CLevel 1
                      , component CidLongRangeSensors $ CLevel 1
                      , component CidSupplyPod $ CLevel 2
                      ]
    return json


-- | Get list of all chassis that currently logged in user has access to
getApiChassisR :: Handler Value
getApiChassisR = do
    _ <- apiRequireFaction
    loadedChassis <- runDB $ selectList [] []
    loadedRequirements <- runDB $ selectList [ RequiredComponentChassisId <-. map entityKey loadedChassis ] []
    let chassis = map (\c -> ChassisDto (entityKey c) (chassisName (entityVal c)) (chassisTonnage (entityVal c))
                             $ map (\r -> ComponentLevel (CLevel $ requiredComponentLevel $ entityVal r)
                                                         $ requiredComponentComponentType $ entityVal r)
                             $ filter (\r -> requiredComponentChassisId (entityVal r) == entityKey c) loadedRequirements)
                      loadedChassis
    return $ toJSON chassis


-- | Get details of all designs that currently logged in user has access to
getApiDesignR :: Handler Value
getApiDesignR = do
    (_, _, fId) <- apiRequireFaction
    loadedDesigns <- runDB $ selectList [ DesignOwnerId ==. fId ] []
    loadedComponents <- runDB $ selectList [ PlannedComponentDesignId <-. map entityKey loadedDesigns ] []
    let designs = map (\d -> designToDesignDto (entityKey d, entityVal d)
                    $ filter (\c -> plannedComponentDesignId (entityVal c) == entityKey d) loadedComponents)
                    loadedDesigns
    return $ toJSON designs


-- | Create a new design
postApiDesignR :: Handler Value
postApiDesignR = do
    (_, _, fId) <- apiRequireFaction
    msg <- requireJsonBody
    if validateSaveDesign msg then
        (do date <- runDB starDate
            savedDesign <- runDB $ saveDesign date msg fId
            return $ toJSON savedDesign)
        else sendResponseStatus status400 ("Validation failed" :: Text)


-- | Update existing design
putApiDesignIdR :: Key Design -> Handler Value
putApiDesignIdR dId = do
    (_, _, fId) <- apiRequireFaction
    msg <- requireJsonBody
    if validateSaveDesign msg then
        (do savedDesign <- runDB $ updateDesign dId msg fId
            return $ toJSON savedDesign)
        else sendResponseStatus status400 ("Validation failed" :: Text)


-- | Permanently delete design
deleteApiDesignIdR :: Key Design -> Handler Value
deleteApiDesignIdR dId = do
    _ <- apiRequireFaction
    _ <- runDB $ deleteDesign dId
    sendResponseStatus status200 $ show $ fromSqlKey dId


-- | Validate that given design is valid
validateSaveDesign :: DesignDto -> Bool
validateSaveDesign _ = True


-- | Save given design into database and create a news article about it
saveDesign :: (MonadIO m, PersistStoreWrite backend, PersistQueryRead backend,
    BaseBackend backend ~ SqlBackend) =>
    Time -> DesignDto -> Key Faction -> ReaderT backend m DesignDto
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
    Key Design -> DesignDto -> Key Faction -> ReaderT backend m DesignDto
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
                Key Design -> ReaderT backend m ()
deleteDesign dId = do
    deleteWhere [ PlannedComponentDesignId ==. dId ]
    delete dId
