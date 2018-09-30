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
import News (makeDesignCreatedNews)
import MenuHelpers (starDate)
import Common (apiRequireFaction)

getDesignerR :: Handler Html
getDesignerR = do
    defaultLayout $ do
        setTitle "Deep Sky - Ship designer"
        addScript $ StaticR js_shipdesigner_js
        addStylesheet $ StaticR css_site_css
        $(widgetFile "shipdesigner")
   
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

getApiChassisR :: Handler Value
getApiChassisR = do
    _ <- apiRequireFaction
    loadedChassis <- runDB $ selectList [] []
    loadedRequirements <- runDB $ selectList [ RequiredComponentChassisId <-. map entityKey loadedChassis ] []
    let chassis = map (\c -> ChassisDto (entityKey c) (chassisName (entityVal c)) (chassisTonnage (entityVal c)) 
                             $ map (\r -> ComponentLevel (CLevel $ requiredComponentLevel $ entityVal r) 
                                                         $ requiredComponentComponentType $ entityVal r)
                             $ filter (\r -> (requiredComponentChassisId (entityVal r)) == entityKey c) loadedRequirements) 
                      loadedChassis
    return $ toJSON chassis

getApiDesignR :: Handler Value
getApiDesignR = do
    (_, _, fId) <- apiRequireFaction
    loadedDesigns <- runDB $ selectList [ DesignOwnerId ==. fId ] []
    loadedComponents <- runDB $ selectList [ PlannedComponentDesignId <-. map entityKey loadedDesigns ] []
    let designs = map (\d -> designToDesignDto (entityKey d, entityVal d)
                    $ filter (\c -> (plannedComponentDesignId (entityVal c)) == (entityKey d)) loadedComponents) 
                    loadedDesigns
    return $ toJSON designs

postApiDesignR :: Handler Value
postApiDesignR = do    
    (_, _, fId) <- apiRequireFaction
    msg <- requireJsonBody 
    case (validateSaveDesign msg) of
        False -> sendResponseStatus status400 ("Validation failed" :: Text)
        True -> do 
            date <- runDB $ starDate
            savedDesign <- runDB $ saveDesign date msg fId            
            return $ toJSON savedDesign

putApiDesignIdR :: Key Design -> Handler Value
putApiDesignIdR dId = do    
    (_, _, fId) <- apiRequireFaction
    msg <- requireJsonBody 
    case (validateSaveDesign msg) of
        False -> sendResponseStatus status400 ("Validation failed" :: Text)
        True -> do 
            savedDesign <- runDB $ updateDesign dId msg fId
            return $ toJSON savedDesign

deleteApiDesignIdR :: Key Design -> Handler Value
deleteApiDesignIdR dId = do
    _ <- apiRequireFaction
    _ <- runDB $ deleteDesign dId
    sendResponseStatus status200 $ show $ fromSqlKey dId

validateSaveDesign :: DesignDto -> Bool
validateSaveDesign _ = True

saveDesign :: (MonadIO m, PersistStoreWrite backend, PersistQueryRead backend,
    BaseBackend backend ~ SqlBackend) =>
    Time -> DesignDto -> Key Faction -> ReaderT backend m DesignDto
saveDesign date design fId = do
    newId <- insert $ Design (designDtoName design) fId (designDtoChassisId design)
    _ <- mapM insert $ map (componentDtoToPlannedComponent newId) (designDtoComponents design)
    newDesign <- get newId
    newComponents <- selectList [ PlannedComponentDesignId ==. newId ] []
    let x = designToDesignDto (newId, fromJust newDesign) newComponents
    _ <- insert $ makeDesignCreatedNews (Entity newId $ fromJust newDesign) date fId
    return x

updateDesign :: (MonadIO m, PersistStoreWrite backend, PersistQueryRead backend,
    PersistQueryWrite backend, BaseBackend backend ~ SqlBackend) =>
    Key Design -> DesignDto -> Key Faction -> ReaderT backend m DesignDto
updateDesign dId design fId = do
    _ <- replace dId $ Design (designDtoName design) fId (designDtoChassisId design)
    _ <- deleteWhere [ PlannedComponentDesignId ==. dId ]
    _ <- mapM insert $ map (componentDtoToPlannedComponent dId) (designDtoComponents design)
    newDesign <- get dId
    newComponents <- selectList [ PlannedComponentDesignId ==. dId ] []
    let x = designToDesignDto (dId, fromJust newDesign) newComponents
    return x

-- | Delete design and related components from database
deleteDesign :: (MonadIO m, PersistQueryWrite backend,
                 BaseBackend backend ~ SqlBackend) =>
                Key Design -> ReaderT backend m ()
deleteDesign dId = do
    _ <- deleteWhere [ PlannedComponentDesignId ==. dId ]
    delete dId