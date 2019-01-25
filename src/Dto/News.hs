{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Dto.News ( NewsDto(..), NewsArticleDto(..), StarFoundNewsDto(..), PlanetFoundNewsDto(..)
                , UserWrittenNewsDto(..), DesignCreatedNewsDto(..)
                , ConstructionFinishedNewsDto(..), UserNewsIconDto(..), SpecialNewsDto(..)
                , KragiiWormsEventDto(..), UserOptionDto(..), KragiiWormsChoiceDto(..), KragiiNewsDto(..)
                , ProductionChangedNewsDto(..)
                ) where

import Import
import Data.Aeson ( object, (.=), (.!=), (.:?), withObject )
import Data.Aeson.TH ( deriveJSON, defaultOptions, constructorTagModifier, fieldLabelModifier )
import Resources ( ResourceType(..) )


-- | Data transfer object for news that mainly just wraps id and contents together
data NewsDto = NewsDto
    { newsDtoId :: Key News
    , newsContents :: NewsArticleDto
    , newsIcon :: Text
    }
    deriving (Show, Read, Eq)


-- | data transfer object for various types of news
data NewsArticleDto =
    StarFoundDto StarFoundNewsDto
    | PlanetFoundDto PlanetFoundNewsDto
    | UserWrittenDto UserWrittenNewsDto
    | DesignCreatedDto DesignCreatedNewsDto
    | ConstructionFinishedDto ConstructionFinishedNewsDto
    | ProductionBoostStartedDto ProductionChangedNewsDto
    | ProductionSlowdownStartedDto ProductionChangedNewsDto
    | ProductionBoostEndedDto ProductionChangedNewsDto
    | ProductionSlowdownEndedDto ProductionChangedNewsDto
    | KragiiDto KragiiNewsDto
    | SpecialDto SpecialNewsDto
    deriving (Show, Read, Eq)


-- | data transfer object for all kinds of special news
data SpecialNewsDto =
    KragiiEventDto KragiiWormsEventDto
    deriving (Show, Read, Eq)


-- | Data transfer object for kragii attack special event
data KragiiWormsEventDto = KragiiWormsEventDto
    { kragiiWormsDtoPlanetId :: Key Planet
    , kragiiWormsDtoPlanetName :: Text
    , kragiiWormsDtoSystemId :: Key StarSystem
    , kragiiWormsDtoSystemName :: Text
    , kragiiWormsDtoOptions :: [UserOptionDto KragiiWormsChoiceDto]
    , kragiiWormsDtoChoice :: Maybe KragiiWormsChoiceDto
    , kragiiWormsDtoDate :: Int
    }
    deriving (Show, Read, Eq)


-- | data transfer object for user choice regarding kragii attack
data KragiiWormsChoiceDto = EvadeWormsDto
    | AttackWormsDto
    | TameWormsDto
    deriving (Show, Read, Eq)


-- | data transfer option for general user choice regarding special event
data UserOptionDto a =
    UserOptionDto { userOptionDtoTitle :: Text
                  , userOptionDtoExplanation :: [Text]
                  , userOptionDtoChoice :: a
                  }
    deriving (Show, Read, Eq)


-- | data transfer object for news that tell resolution of kragii attack
data KragiiNewsDto = KragiiNewsDto
    { kragiiNewsDtoPlanetId :: Key Planet
    , kragiiNewsDtoPlanetName :: Text
    , kragiiNewsDtoSystemId :: Key StarSystem
    , kragiiNewsDtoSystemName :: Text
    , kragiiNewsDtoResolution :: Text
    , kragiiNewsDtoDate :: Int
    }
    deriving (Show, Read, Eq)


-- | data transfer object for star found news
data StarFoundNewsDto = StarFoundNewsDto
    { starFoundNewsDtoStarName :: Text
    , starFoundNewsDtoSystemName :: Text
    , starFoundNewsDtoSystemId :: Key StarSystem
    , starFoundNewsDtoDate :: Int
    }
    deriving (Show, Read, Eq)


-- | data transfer object for planet found news
data PlanetFoundNewsDto = PlanetFoundNewsDto
    { planetFoundNewsDtoPlanetName :: Text
    , planetFoundNewsDtoSystemName :: Text
    , planetFoundNewsDtoSystemId :: Key StarSystem
    , planetFoundNewsDtoPlanetId :: Key Planet
    , planetFoundNewsDtoDate :: Int
    }
    deriving (Show, Read, Eq)


-- | data transfer object for user written news
data UserWrittenNewsDto = UserWrittenNewsDto
    { userWrittenNewsDtoContent :: Text
    , userWrittenNewsDtoDate :: Int
    , userWrittenNewsDtoUser :: Text
    , userWrittenNewsDtoIcon :: UserNewsIconDto
    }
    deriving (Show, Read, Eq)


-- | Icon displayed on user written news
data UserNewsIconDto =
    GenericUserNewsDto
    | JubilationUserNewsDto
    | CatUserNewsDto
    deriving (Show, Read, Eq, Enum, Bounded)


-- | data transfer object for design created news
data DesignCreatedNewsDto = DesignCreatedNewsDto
    { designCreatedNewsDtoDesignId :: Key Design
    , designCreatedNewsDtoName :: Text
    , designCreatedNewsDtoDate :: Int
    }
    deriving (Show, Read, Eq)


-- | data transfer object for construction finished news
data ConstructionFinishedNewsDto = ConstructionFinishedNewsDto
    { constructionFinishedNewsDtoPlanetName :: Maybe Text
    , constructionFinishedNewsDtoPlanetId :: Maybe (Key Planet)
    , constructionFinishedNewsDtoSystemName :: Text
    , constructionFinishedNewsDtoSystemId :: Key StarSystem
    , constructionFinishedNewsDtoConstructionName :: Text
    , constructionFinishedNewsDtoBuildingId :: Maybe (Key Building)
    , constructionFinishedNewsDtoShipId :: Maybe (Key Ship)
    , constructionFinishedNewsDtoDate :: Int
    }
    deriving (Show, Read, Eq)


-- | Turn NewsDto into JSON
instance ToJSON NewsDto where
    toJSON (NewsDto { newsDtoId = nId
                    , newsContents = contents
                    , newsIcon = icon }) =
        object [ "id" .= nId
               , "contents" .= contents
               , "tag" .= jsonTag contents
               , "icon" .= icon
               , "starDate" .= newsStarDate contents
               ]


-- | tag is used to distinguish different types of news articles in their JSON
-- representation. Note that this mapping doesn't correspond 1:1 with the types
-- as some DTOs (construction finished for example) will be serialized to JSON
-- differently depending on the data they contain (building vs. ship)
jsonTag :: NewsArticleDto -> Text
jsonTag news =
    case news of
        StarFoundDto _ ->
            "StarFound"

        PlanetFoundDto _ ->
            "PlanetFound"

        UserWrittenDto _ ->
            "UserWritten"

        DesignCreatedDto _ ->
            "DesignCreated"

        ConstructionFinishedDto (ConstructionFinishedNewsDto { constructionFinishedNewsDtoBuildingId = mbId}) ->
            case mbId of
                Just _ ->
                    "BuildingFinished"

                Nothing ->
                    "ShipFinished"

        ProductionBoostStartedDto _ ->
            "ProductionBoostStarted"

        ProductionSlowdownStartedDto _ ->
            "ProductionSlowdownStarted"

        ProductionBoostEndedDto _ ->
            "ProductionBoostEnded"

        ProductionSlowdownEndedDto _ ->
            "ProductionSlowdownEnded"

        SpecialDto (KragiiEventDto _) ->
            "KragiiEvent"

        KragiiDto _ ->
            "KragiiResolution"


instance ToJSON NewsArticleDto where
    toJSON news =
        case news of
            StarFoundDto dto -> toJSON dto
            PlanetFoundDto dto -> toJSON dto
            UserWrittenDto dto -> toJSON dto
            DesignCreatedDto dto -> toJSON dto
            ConstructionFinishedDto dto -> toJSON dto
            ProductionBoostStartedDto dto -> toJSON dto
            ProductionSlowdownStartedDto dto -> toJSON dto
            ProductionBoostEndedDto dto -> toJSON dto
            ProductionSlowdownEndedDto dto -> toJSON dto
            KragiiDto dto -> toJSON dto
            SpecialDto (KragiiEventDto dto) -> toJSON dto


instance FromJSON NewsArticleDto where
    parseJSON = withObject "contents" $ \o -> do
        tag <- o .: "tag"
        asum [ do
                guard (tag == ("StarFound" :: String))
                contents <- o .: "contents"
                return $ StarFoundDto contents
             , do
                guard (tag == ("PlanetFound" :: String))
                contents <- o .: "contents"
                return $ PlanetFoundDto contents
             , do
                guard (tag == ("UserWritten" :: String))
                contents <- o .: "contents"
                return $ UserWrittenDto contents
             , do
                guard (tag == ("DesignCreated" :: String))
                contents <- o .: "contents"
                return $ DesignCreatedDto contents
             , do
                guard (tag == ("BuildingFinished" :: String)
                       || tag == ("ShipFinished" :: String))
                contents <- o .: "contents"
                return $ ConstructionFinishedDto contents
            , do
                guard (tag == ("ProductionBoostStarted" :: String))
                contents <- o .: "contents"
                return $ ProductionBoostStartedDto contents
            , do
                guard (tag == ("ProductionSlowdownStarted" :: String))
                contents <- o .: "contents"
                return $ ProductionSlowdownStartedDto contents
            , do
                guard (tag == ("ProductionBoostEnded" :: String))
                contents <- o .: "contents"
                return $ ProductionBoostEndedDto contents
            , do
                guard (tag == ("ProductionSlowdownEnded" :: String))
                contents <- o .: "contents"
                return $ ProductionSlowdownEndedDto contents
            , do
                guard (tag == ("KragiiEvent" :: String))
                contents <- o .: "contents"
                return $ SpecialDto (KragiiEventDto contents)
             ]


instance ToJSON SpecialNewsDto where
    toJSON dto =
        case dto of
            KragiiEventDto x -> toJSON x


-- | map planet found news into JSON
instance ToJSON PlanetFoundNewsDto where
    toJSON (PlanetFoundNewsDto { planetFoundNewsDtoPlanetName = pName
                               , planetFoundNewsDtoSystemId = sId
                               , planetFoundNewsDtoPlanetId = pId
                               , planetFoundNewsDtoSystemName = sName
                               }) =
        object [ "planetName" .= pName
               , "systemName" .= sName
               , "planetId" .= pId
               , "systemId" .= sId
               ]


instance FromJSON PlanetFoundNewsDto where
    parseJSON (Object b) =
        PlanetFoundNewsDto <$> b .: "planetName"
                           <*> b .: "systemName"
                           <*> b .: "systemId"
                           <*> b .: "planetId"
                           <*> b .: "starDate"
    parseJSON _ = mzero


-- | map star found news into JSON
instance ToJSON StarFoundNewsDto where
    toJSON (StarFoundNewsDto { starFoundNewsDtoStarName = sName
                             , starFoundNewsDtoSystemName = sysName
                             , starFoundNewsDtoSystemId = sId
                             }) =
        object [ "starName" .= sName
               , "systemName" .= sysName
               , "systemId" .= sId
               ]


instance FromJSON StarFoundNewsDto where
    parseJSON (Object b) =
        StarFoundNewsDto <$> b .: "starName"
                         <*> b .: "systemName"
                         <*> b .: "systemId"
                         <*> b .: "starDate"
    parseJSON _ = mzero


instance ToJSON UserWrittenNewsDto where
    toJSON (UserWrittenNewsDto { userWrittenNewsDtoContent = content
                               , userWrittenNewsDtoUser = userName
                               , userWrittenNewsDtoDate = sDate
                               , userWrittenNewsDtoIcon = icon
                               }) =
        object [ "content" .= content
               , "userName" .= userName
               , "starDate" .= sDate
               , "icon" .= icon
               ]


instance FromJSON UserWrittenNewsDto where
    parseJSON (Object b) =
        UserWrittenNewsDto <$> b .: "content"
                           <*> b .: "starDate"
                           <*> b .:? "userName" .!= ""
                           <*> b .: "icon"
    parseJSON _ = mzero


instance ToJSON DesignCreatedNewsDto where
    toJSON (DesignCreatedNewsDto { designCreatedNewsDtoDesignId = dId
                                 , designCreatedNewsDtoName = dName
                                 , designCreatedNewsDtoDate = sDate
                                 }) =
        object [ "designId" .= dId
               , "name" .= dName
               , "starDate" .= sDate
               ]


instance FromJSON DesignCreatedNewsDto where
    parseJSON (Object b) =
        DesignCreatedNewsDto <$> b .: "designId"
                             <*> b .: "name"
                             <*> b .: "starDate"
    parseJSON _ = mzero


instance ToJSON ConstructionFinishedNewsDto where
    toJSON (ConstructionFinishedNewsDto { constructionFinishedNewsDtoPlanetName = mpName
                                        , constructionFinishedNewsDtoPlanetId = mpId
                                        , constructionFinishedNewsDtoSystemName = sName
                                        , constructionFinishedNewsDtoSystemId = sId
                                        , constructionFinishedNewsDtoConstructionName = cName
                                        , constructionFinishedNewsDtoBuildingId = mbId
                                        , constructionFinishedNewsDtoShipId = msId
                                        }) =
        case mbId of
            Just _ ->
                object [ "planetName" .= mpName
                       , "planetId" .= mpId
                       , "systemName" .= sName
                       , "systemId" .= sId
                       , "constructionName" .= cName
                       , "buildingId" .= mbId
                       ]

            Nothing ->
                object [ "planetName" .= mpName
                       , "planetId" .= mpId
                       , "systemName" .= sName
                       , "systemId" .= sId
                       , "constructionName" .= cName
                       , "shipId" .= msId
                       ]


instance FromJSON ConstructionFinishedNewsDto where
    parseJSON (Object b) =
        ConstructionFinishedNewsDto <$> b .:? "planetName"
                                    <*> b .:? "planetId"
                                    <*> b .: "systemName"
                                    <*> b .: "systemId"
                                    <*> b .: "constructionName"
                                    <*> b .:? "buildingId"
                                    <*> b .:? "shipId"
                                    <*> b .: "starDate"
    parseJSON _ = mzero


data ProductionChangedNewsDto = ProductionChangedNewsDto
    { productionChangedNewsDtoPlanetId :: Key Planet
    , productionChangedNewsDtoPlanetName :: Text
    , productionChangedNewsDtoSystemId :: Key StarSystem
    , productionChangedNewsDtoSystemName :: Text
    , productionChangedNewsDtoType :: ResourceType
    , productionChangedNewsDtoDate :: Int
    }
    deriving (Show, Read, Eq)


{-| Star date of the event that is being reported in this news article
-}
newsStarDate :: NewsArticleDto -> Int
newsStarDate article =
    case article of
        StarFoundDto details ->
            starFoundNewsDtoDate details

        PlanetFoundDto details ->
            planetFoundNewsDtoDate details

        UserWrittenDto details ->
            userWrittenNewsDtoDate details

        DesignCreatedDto details ->
            designCreatedNewsDtoDate details

        ConstructionFinishedDto details ->
            constructionFinishedNewsDtoDate details

        ProductionBoostStartedDto details ->
            productionChangedNewsDtoDate details

        ProductionSlowdownStartedDto details ->
            productionChangedNewsDtoDate details

        ProductionBoostEndedDto details ->
            productionChangedNewsDtoDate details

        ProductionSlowdownEndedDto details ->
            productionChangedNewsDtoDate details

        KragiiDto details ->
            kragiiNewsDtoDate details

        SpecialDto (KragiiEventDto details) ->
            kragiiWormsDtoDate details


$(deriveJSON defaultOptions { constructorTagModifier = \x -> take (length x - 3) x } ''UserNewsIconDto)
$(deriveJSON defaultOptions { fieldLabelModifier = \x -> drop 14 x } ''KragiiWormsEventDto)
$(deriveJSON defaultOptions { fieldLabelModifier = \x -> drop 13 x } ''UserOptionDto)
$(deriveJSON defaultOptions { constructorTagModifier = \x -> take (length x - 3) x } ''KragiiWormsChoiceDto)
$(deriveJSON defaultOptions { fieldLabelModifier = \x -> drop 13 x } ''KragiiNewsDto)
$(deriveJSON defaultOptions { fieldLabelModifier = \x -> drop 24 x } ''ProductionChangedNewsDto)
