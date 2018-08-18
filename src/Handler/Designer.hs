{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Designer where

import Data.Aeson (object, (.=), (.:?))
import Import
import Components

getDesignerR :: Handler Html
getDesignerR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    defaultLayout $ do
        setTitle "Deep Sky - Designs"
        $(widgetFile "shipdesigns")

getNewDesignR :: Handler Html
getNewDesignR = do
    (_, user) <- requireAuthPair   
    _ <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> redirect ProfileR
    defaultLayout $ do
        setTitle "Deep Sky - Ship designer"
        addScript $ StaticR js_shipdesigner_js
        addStylesheet $ StaticR css_site_css
        $(widgetFile "shipdesigner")

data ChassisDto = ChassisDto { cdId :: Int
                             , cdName :: Text
                             , cdMaxTonnage :: Int
                             , cdRequiredTypes :: [ComponentLevel]}
    deriving Show

instance ToJSON ChassisDto where
    toJSON (ChassisDto idKey name maxTonnage types) =
        object [ "id" .= idKey
               , "name" .= name
               , "maxTonnage" .= maxTonnage
               , "requiredTypes" .= array types 
               ]
   
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
    let json = toJSON [ ChassisDto 1 "Destroyer" 150 [ ComponentLevel (CLevel 1) BridgeComponent
                                                     , ComponentLevel (CLevel 1) EngineComponent
                                                     , ComponentLevel (CLevel 1) SensorComponent 
                                                     , ComponentLevel (CLevel 1) SupplyComponent 
                                                     ]
                      , ChassisDto 2 "Satellite" 20 []
                      ]
    return json

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

validateSaveDesign :: SaveDesign -> Bool
validateSaveDesign _ = True

saveDesign :: (MonadIO m, PersistStoreWrite backend, PersistQueryRead backend,
    BaseBackend backend ~ SqlBackend) =>
    SaveDesign -> Key Faction -> ReaderT backend m SaveDesign
saveDesign design fId = do
    newId <- insert $ Design (saveDesignName design) fId
    cIds <- mapM insert $ map (saveComponentToPlannetComponent newId) (saveDesignComponents design)
    newDesign <- get newId
    newComponents <- selectList [ PlannedComponentDesignId ==. newId ] []
    let x = case newDesign of
                Just x -> designToSaveDesign (newId, x) []
    return x

designToSaveDesign :: (Key Design, Design) -> [ PlannedComponent ] -> SaveDesign
designToSaveDesign (newId, design) comps = 
    SaveDesign (Just newId) 0 (designName design) []

saveComponentToPlannetComponent :: Key Design -> SaveInstalledComponent -> PlannedComponent
saveComponentToPlannetComponent dId (SaveInstalledComponent (SaveComponent cId level) amount) =
    PlannedComponent dId cId level amount

data ShipDesign = ShipDesign
    { designId :: Maybe (Key Design)
    , sdesignName :: Text
    , designChassisId :: Int
    } deriving Show

instance ToJSON ShipDesign where
    toJSON (ShipDesign dId name chassis) =
        object [ "id" .= dId
               , "name" .= name
               , "chassisId" .= chassis
               ]

data SaveComponent = SaveComponent
    { saveComponentId :: ComponentId
    , saveComponentLevel :: Int
    } deriving Show

data SaveInstalledComponent = SaveInstalledComponent
    { saveInstalledComponentComponents :: SaveComponent
    , saveInstalledComponentAmount :: Int
    } deriving Show

data SaveDesign = SaveDesign
    { saveDesignId :: Maybe (Key Design)
    , saveDesignChassisId :: Int
    , saveDesignName :: Text
    , saveDesignComponents :: [ SaveInstalledComponent ]
    } deriving Show

instance FromJSON SaveComponent where
    parseJSON (Object v) =
        SaveComponent <$> v .: "id"
                      <*> v .: "level"
    parseJSON _ = mzero

instance ToJSON SaveComponent where
    toJSON (SaveComponent cId level) =
        object [ "id" .= cId
               , "level" .= level
               ]

instance FromJSON SaveInstalledComponent where
    parseJSON (Object v) =
        SaveInstalledComponent <$> v .: "component"
                               <*> v .: "amount"
    parseJSON _ = mzero

instance ToJSON SaveInstalledComponent where
    toJSON (SaveInstalledComponent comp amount) =
        object [ "component" .= comp
               , "amount" .= amount 
               ]

instance FromJSON SaveDesign where
    parseJSON (Object v) =
        SaveDesign <$> v .:? "id"
                   <*> v .: "chassisId"
                   <*> v .: "name"
                   <*> v .: "components"
    parseJSON _ = mzero

instance ToJSON SaveDesign where
    toJSON (SaveDesign dId chassisId name components) =
        object [ "id" .= dId
               , "chassisId" .= chassisId
               , "name" .= name
               , "components" .= components
               ]
