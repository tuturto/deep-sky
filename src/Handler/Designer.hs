{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Handler.Designer ( getDesignerR, getApiComponentsR, getApiChassisR, getApiDesignR
                        , postApiDesignR, putApiDesignIdR, deleteApiDesignIdR )
    where

import Import
import Data.Maybe ( fromJust, maybe )
import Common ( apiRequireFaction, mkUniq )
import Vehicles.Components ( ComponentPower(..), ComponentLevel(..), ComponentId(..), Component
                           , components, requirements )
import Dto.Ship ( DesignDto(..), ChassisDto(..), RequiredComponentDto(..), designToDesignDto
                , componentDtoToPlannedComponent )
import Handler.Home ( getNewHomeR )
import MenuHelpers ( starDate )
import News.Import ( designCreatedNews )
import Research.Data ( Technology )
import Queries ( chassisList )


getDesignerR :: Handler Html
getDesignerR = getNewHomeR


-- | Get list of all components that currently logged in user has access to
getApiComponentsR :: Handler Value
getApiComponentsR = do
    (_, _, fId) <- apiRequireFaction
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
    (_, _, fId) <- apiRequireFaction
    chassisRequirements <- runDB $ chassisList fId
    return $ toJSON $ toChassisDto <$> chassisRequirements


-- | Map chassis and required component information into chassis dto
toChassisDto :: (Entity Chassis, [Entity RequiredComponent]) -> ChassisDto
toChassisDto (chassis, reqs) =
    ChassisDto
        { chassisDtoId = entityKey chassis
        , chassisDtoName = chassisName . entityVal $ chassis
        , chassisDtoType = chassisType . entityVal $ chassis
        , chassisDtoMaxTonnage = chassisTonnage . entityVal $ chassis
        , chassisDtoRequiredTypes = toRequirement . entityVal <$> reqs
        , chassisDtoArmourSlots = chassisArmourSlots . entityVal $ chassis
        , chassisDtoInnerSlots = chassisInnerSlots . entityVal $ chassis
        , chassisDtoOuterSlots = chassisOuterSlots . entityVal $ chassis
        , chassisDtoSensorSlots = chassisSensorSlots . entityVal $ chassis
        , chassisDtoWeaponSlots = chassisWeaponSlots . entityVal $ chassis
        , chassisDtoEngineSlots = chassisEngineSlots . entityVal $ chassis
        , chassisDtoMotiveSlots = chassisMotiveSlots . entityVal $ chassis
        , chassisDtoSailSlots = chassisSailSlots . entityVal $ chassis
        }


-- | Map required component to requirement
toRequirement :: RequiredComponent -> RequiredComponentDto
toRequirement comp =
    RequiredComponentDto
        { requiredComponentDtoPower = ComponentPower
                                        { componentPowerLevel = requiredComponentLevel comp
                                        , componentPowerType = requiredComponentComponentType comp
                                        }
        , requiredComponentDtoAmount = requiredComponentAmount comp
        }


-- | Get details of all designs that currently logged in user has access to
getApiDesignR :: Handler Value
getApiDesignR = do
    -- TODO: use esqueleto and refactor into own function
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
    (_, _, fId) <- apiRequireFaction
    _ <- runDB $ deleteDesign dId
    loadedDesigns <- runDB $ selectList [ DesignOwnerId ==. fId ] []
    loadedComponents <- runDB $ selectList [ PlannedComponentDesignId <-. map entityKey loadedDesigns ] []
    let designs = map (\d -> designToDesignDto (entityKey d, entityVal d)
                    $ filter (\c -> plannedComponentDesignId (entityVal c) == entityKey d) loadedComponents)
                    loadedDesigns
    return $ toJSON designs


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
