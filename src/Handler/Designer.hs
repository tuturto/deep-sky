{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Designer where

import Database.Persist.Sql (fromSqlKey)
import Data.Maybe (fromJust)
import Dto.Ship
import Import
import Components

getDesignerR :: Handler Html
getDesignerR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    defaultLayout $ do
        setTitle "Deep Sky - Ship designer"
        addScript $ StaticR js_shipdesigner_js
        addStylesheet $ StaticR css_site_css
        $(widgetFile "shipdesigner")
   
getApiComponentsR :: Handler Value
getApiComponentsR = do
    let json = toJSON [ component CidArmour $ CLevel 1
                      , component CidEngine $ CLevel 1
                      , component CidBridge $ CLevel 1
                      , component CidLongRangeSensors $ CLevel 1
                      , component CidSupplyPod $ CLevel 2
                      ]
    return json

getApiChassisR :: Handler Value
getApiChassisR = do
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
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
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    loadedDesigns <- runDB $ selectList [ DesignOwnerId ==. fId ] []
    loadedComponents <- runDB $ selectList [ PlannedComponentDesignId <-. map entityKey loadedDesigns ] []
    let designs = map (\d -> designToDesignDto (entityKey d, entityVal d)
                    $ filter (\c -> (plannedComponentDesignId (entityVal c)) == (entityKey d)) loadedComponents) 
                    loadedDesigns
    return $ toJSON designs

postApiDesignR :: Handler Value
postApiDesignR = do    
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    msg <- requireJsonBody 
    case (validateSaveDesign msg) of
        False -> sendResponseStatus status400 ("Validation failed" :: Text)
        True -> do 
            savedDesign <- runDB $ saveDesign msg fId
            return $ toJSON savedDesign

putApiDesignIdR :: Key Design -> Handler Value
putApiDesignIdR dId = do    
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    msg <- requireJsonBody 
    case (validateSaveDesign msg) of
        False -> sendResponseStatus status400 ("Validation failed" :: Text)
        True -> do 
            savedDesign <- runDB $ updateDesign dId msg fId
            return $ toJSON savedDesign

deleteApiDesignIdR :: Key Design -> Handler Value
deleteApiDesignIdR dId = do
    (_, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    _ <- runDB $ deleteWhere [ PlannedComponentDesignId ==. dId ]
    _ <- runDB $ delete dId
    sendResponseStatus status200 $ show $ fromSqlKey dId

validateSaveDesign :: DesignDto -> Bool
validateSaveDesign _ = True

saveDesign :: (MonadIO m, PersistStoreWrite backend, PersistQueryRead backend,
    BaseBackend backend ~ SqlBackend) =>
    DesignDto -> Key Faction -> ReaderT backend m DesignDto
saveDesign design fId = do
    newId <- insert $ Design (designDtoName design) fId (designDtoChassisId design)
    cIds <- mapM insert $ map (componentDtoToPlannedComponent newId) (designDtoComponents design)
    newDesign <- get newId
    newComponents <- selectList [ PlannedComponentDesignId ==. newId ] []
    let x = case newDesign of
                Just x -> designToDesignDto (newId, x) newComponents
    return x

updateDesign :: (MonadIO m, PersistStoreWrite backend, PersistQueryRead backend,
    PersistQueryWrite backend, BaseBackend backend ~ SqlBackend) =>
    Key Design -> DesignDto -> Key Faction -> ReaderT backend m DesignDto
updateDesign dId design fId = do
    _ <- replace dId $ Design (designDtoName design) fId (designDtoChassisId design)
    _ <- deleteWhere [ PlannedComponentDesignId ==. dId ]
    cIds <- mapM insert $ map (componentDtoToPlannedComponent dId) (designDtoComponents design)
    newDesign <- get dId
    newComponents <- selectList [ PlannedComponentDesignId ==. dId ] []
    let x = designToDesignDto (dId, fromJust newDesign) newComponents
    return x
