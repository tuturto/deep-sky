{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Dto.News
    ( NewsDto(..), NewsArticleDto(..), StarFoundNewsDto(..)
    , PlanetFoundNewsDto(..), UserWrittenNewsDto(..), DesignCreatedNewsDto(..)
    , ConstructionFinishedNewsDto(..), UserNewsIconDto(..), SpecialNewsDto(..)
    , KragiiWormsEventDto(..), UserOptionDto(..), KragiiWormsChoiceDto(..)
    , KragiiNewsDto(..), ProductionChangedNewsDto(..)
    , ResearchCompletedNewsDto(..), ScurryingSoundsNewsDto(..)
    , ScurryingSoundsEventDto(..), ScurryingSoundsChoiceDto(..)
    , NamingPetNewsDto(..), NamingPetChoiceDto(..), NamingPetEventDto(..)
    ) where

import Import
import Data.Aeson ( object, (.=), (.!=), (.:?), withObject )
import Data.Aeson.TH ( deriveJSON, defaultOptions, constructorTagModifier, fieldLabelModifier )
import Events.Import ( EventResolveType(..) )
import CustomTypes ( StarDate )
import Research.Data ( Technology )
import People.Data ( PersonName(..), PetType(..), PetName(..) )
import Resources ( ResourceType(..) )
import Space.Data ( PlanetName(..), StarName(..), StarSystemName(..) )


-- | Data transfer object for news that mainly just wraps id and contents together
data NewsDto = NewsDto
    { newsDtoId :: !(Key News)
    , newsContents :: !NewsArticleDto
    , newsIcon :: !Text
    } deriving (Show, Read, Eq)


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
    | ResearchCompletedDto ResearchCompletedNewsDto
    | KragiiDto KragiiNewsDto
    | ScurryingSoundsDto ScurryingSoundsNewsDto
    | NamingPetDto NamingPetNewsDto
    | SpecialDto SpecialNewsDto
    deriving (Show, Read, Eq)


-- | data transfer object for all kinds of special news
data SpecialNewsDto =
    KragiiEventDto KragiiWormsEventDto
    | MkScurryingSoundsEventDto ScurryingSoundsEventDto
    | MkNamingPetEventDto NamingPetEventDto
    deriving (Show, Read, Eq)


-- | Data transfer object for kragii attack special event
data KragiiWormsEventDto = KragiiWormsEventDto
    { kragiiWormsDtoPlanetId :: !(Key Planet)
    , kragiiWormsDtoPlanetName :: !PlanetName
    , kragiiWormsDtoSystemId :: !(Key StarSystem)
    , kragiiWormsDtoSystemName :: !StarSystemName
    , kragiiWormsDtoOptions :: ![UserOptionDto KragiiWormsChoiceDto]
    , kragiiWormsDtoChoice :: !(Maybe KragiiWormsChoiceDto)
    , kragiiWormsDtoFactionId :: !(Key Faction)
    , kragiiWormsDtoDate :: !StarDate
    , kragiiWormsDtoResolveType :: !(Maybe EventResolveType)
    } deriving (Show, Read, Eq)


-- | Data transfer object for scurrying sounds special event
data ScurryingSoundsEventDto = ScurryingSoundsEventDto
    { scurryingSoundsEventDtoDate :: !StarDate
    , scurryingSoundsEventDtoPersonId :: !(Key Person)
    , scurryingSoundsEventDtoOptions :: ![UserOptionDto ScurryingSoundsChoiceDto]
    , scurryingSoundsEventDtoChoice :: !(Maybe ScurryingSoundsChoiceDto)
    , scurryingSoundsEventDtoResolveType :: !(Maybe EventResolveType)
    } deriving (Show, Read, Eq)


-- | data transfer object for user choice regarding kragii attack
data KragiiWormsChoiceDto =
    EvadeWormsDto
    | AttackWormsDto
    | TameWormsDto
    deriving (Show, Read, Eq)


data ScurryingSoundsChoiceDto =
    GetCatDto
    | TameRatDto
    | GetRidSomehowElseDto
    deriving (Show, Read, Eq)


data NamingPetEventDto = NamingPetEventDto
    { namingPetEventDtoPersonId :: !(Key Person)
    , namingPetEventDtoPetId :: !(Key Pet)
    , namingPetEventDtoPetType :: !PetType
    , namingPetEventDtoDate :: !StarDate
    , namingPetEventDtoOptions :: ![UserOptionDto NamingPetChoiceDto]
    , namingPetEventDtoChoice :: !(Maybe NamingPetChoiceDto)
    , namingPetEventDtoResolveType :: !(Maybe EventResolveType)
    } deriving (Show, Read, Eq)


data NamingPetChoiceDto =
    GiveNameDto PetName
    | LetSomeoneElseDecideDto
    deriving (Show, Read, Eq)


-- | data transfer option for general user choice regarding special event
data UserOptionDto a = UserOptionDto
    { userOptionDtoTitle :: Text
    , userOptionDtoExplanation :: [Text]
    , userOptionDtoChoice :: a
    } deriving (Show, Read, Eq)


-- | data transfer object for news that tell resolution of kragii attack
data KragiiNewsDto = KragiiNewsDto
    { kragiiNewsDtoPlanetId :: !(Key Planet)
    , kragiiNewsDtoPlanetName :: !PlanetName
    , kragiiNewsDtoSystemId :: !(Key StarSystem)
    , kragiiNewsDtoSystemName :: !StarSystemName
    , kragiiNewsDtoResolution :: !Text
    , kragiiNewsDtoFactionId :: !(Key Faction)
    , kragiiNewsDtoDate :: !StarDate
    } deriving (Show, Read, Eq)


data ScurryingSoundsNewsDto = ScurryingSoundsNewsDto
    { scurryingSoundsNewsDtoExplanation :: !Text
    , scurryingSoundsNewsDtoPetId :: !(Maybe (Key Pet))
    , scurryingSoundsNewsDtoPetType :: !(Maybe PetType)
    , scurryingSoundsNewsDtoDate :: !StarDate
    } deriving (Show, Read, Eq)


data NamingPetNewsDto = NamingPetNewsDto
    { namingPetNewsDtoExplanation :: !Text
    , namingPetNewsDtoPetId :: !(Key Pet)
    , namingPetNewsDtoPetType :: !PetType
    , namingPetNewsDtoDate :: !StarDate
    } deriving (Show, Read, Eq)


-- | data transfer object for star found news
data StarFoundNewsDto = StarFoundNewsDto
    { starFoundNewsDtoStarName :: !StarName
    , starFoundNewsDtoSystemName :: !StarSystemName
    , starFoundNewsDtoSystemId :: !(Key StarSystem)
    , starFoundNewsDtoDate :: !StarDate
    } deriving (Show, Read, Eq)


-- | data transfer object for planet found news
data PlanetFoundNewsDto = PlanetFoundNewsDto
    { planetFoundNewsDtoPlanetName :: !PlanetName
    , planetFoundNewsDtoSystemName :: !StarSystemName
    , planetFoundNewsDtoSystemId :: !(Key StarSystem)
    , planetFoundNewsDtoPlanetId :: !(Key Planet)
    , planetFoundNewsDtoDate :: !StarDate
    } deriving (Show, Read, Eq)


-- | data transfer object for user written news
data UserWrittenNewsDto = UserWrittenNewsDto
    { userWrittenNewsDtoContent :: !Text
    , userWrittenNewsDtoDate :: !StarDate
    , userWrittenNewsDtoUser :: !PersonName
    , userWrittenNewsDtoIcon :: !UserNewsIconDto
    } deriving (Show, Read, Eq)


-- | Icon displayed on user written news
data UserNewsIconDto =
    GenericUserNewsDto
    | JubilationUserNewsDto
    | CatUserNewsDto
    deriving (Show, Read, Eq, Enum, Bounded)


-- | data transfer object for design created news
data DesignCreatedNewsDto = DesignCreatedNewsDto
    { designCreatedNewsDtoDesignId :: !(Key Design)
    , designCreatedNewsDtoName :: !Text
    , designCreatedNewsDtoDate :: !StarDate
    } deriving (Show, Read, Eq)


-- | data transfer object for construction finished news
data ConstructionFinishedNewsDto = ConstructionFinishedNewsDto
    { constructionFinishedNewsDtoPlanetName :: !(Maybe PlanetName)
    , constructionFinishedNewsDtoPlanetId :: !(Maybe (Key Planet))
    , constructionFinishedNewsDtoSystemName :: !StarSystemName
    , constructionFinishedNewsDtoSystemId :: !(Key StarSystem)
    , constructionFinishedNewsDtoConstructionName :: !Text
    , constructionFinishedNewsDtoBuildingId :: !(Maybe (Key Building))
    , constructionFinishedNewsDtoShipId :: !(Maybe (Key Ship))
    , constructionFinishedNewsDtoDate :: !StarDate
    } deriving (Show, Read, Eq)


-- | Turn NewsDto into JSON
instance ToJSON NewsDto where
    toJSON NewsDto { newsDtoId = nId
                   , newsContents = contents
                   , newsIcon = icon } =
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

        ConstructionFinishedDto ConstructionFinishedNewsDto { constructionFinishedNewsDtoBuildingId = mbId} ->
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

        ResearchCompletedDto _ ->
            "ResearchCompleted"

        SpecialDto (KragiiEventDto _) ->
            "KragiiEvent"

        SpecialDto (MkScurryingSoundsEventDto _) ->
            "ScurryingSoundsEvent"

        KragiiDto _ ->
            "KragiiResolution"

        ScurryingSoundsDto _ ->
            "ScurryingSoundsResolution"

        NamingPetDto _ ->
            "NamingPetResolution"

        SpecialDto (MkNamingPetEventDto _) ->
            "NamingPetEvent"


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
            ResearchCompletedDto dto -> toJSON dto
            KragiiDto dto -> toJSON dto
            ScurryingSoundsDto dto -> toJSON dto
            NamingPetDto dto -> toJSON dto
            SpecialDto (KragiiEventDto dto) -> toJSON dto
            SpecialDto (MkScurryingSoundsEventDto dto) -> toJSON dto
            SpecialDto (MkNamingPetEventDto dto) -> toJSON dto


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
            , do
                guard (tag == ("ScurryingSoundsEvent" :: String))
                contents <- o .: "contents"
                return $ SpecialDto (MkScurryingSoundsEventDto contents)
            , do
                guard (tag == ("NamingPetEvent" :: String))
                contents <- o .: "contents"
                return $ SpecialDto (MkNamingPetEventDto contents)
             ]


instance ToJSON SpecialNewsDto where
    toJSON dto =
        case dto of
            KragiiEventDto x ->
                toJSON x

            MkScurryingSoundsEventDto x ->
                toJSON x

            MkNamingPetEventDto x ->
                toJSON x




instance ToJSON UserWrittenNewsDto where
    toJSON UserWrittenNewsDto { userWrittenNewsDtoContent = content
                              , userWrittenNewsDtoUser = userName
                              , userWrittenNewsDtoDate = sDate
                              , userWrittenNewsDtoIcon = icon
                              } =
        object [ "content" .= content
               , "userName" .= userName
               , "starDate" .= sDate
               , "icon" .= icon
               ]


instance FromJSON UserWrittenNewsDto where
    parseJSON (Object b) =
        UserWrittenNewsDto <$> b .: "content"
                           <*> b .: "starDate"
                           <*> b .:? "userName" .!= (SimpleName "" Nothing)
                           <*> b .: "icon"
    parseJSON _ = mzero


instance ToJSON ConstructionFinishedNewsDto where
    toJSON ConstructionFinishedNewsDto { constructionFinishedNewsDtoPlanetName = mpName
                                       , constructionFinishedNewsDtoPlanetId = mpId
                                       , constructionFinishedNewsDtoSystemName = sName
                                       , constructionFinishedNewsDtoSystemId = sId
                                       , constructionFinishedNewsDtoConstructionName = cName
                                       , constructionFinishedNewsDtoBuildingId = mbId
                                       , constructionFinishedNewsDtoShipId = msId
                                       } =
        case mbId of
            Just _ ->
                object [ "PlanetName" .= mpName
                       , "PlanetId" .= mpId
                       , "SystemName" .= sName
                       , "SystemId" .= sId
                       , "ConstructionName" .= cName
                       , "BuildingId" .= mbId
                       ]

            Nothing ->
                object [ "PlanetName" .= mpName
                       , "planetId" .= mpId
                       , "SystemName" .= sName
                       , "SystemId" .= sId
                       , "ConstructionName" .= cName
                       , "ShipId" .= msId
                       ]


instance FromJSON ConstructionFinishedNewsDto where
    parseJSON (Object b) =
        ConstructionFinishedNewsDto <$> b .:? "PlanetName"
                                    <*> b .:? "PlanetId"
                                    <*> b .: "SystemName"
                                    <*> b .: "SystemId"
                                    <*> b .: "ConstructionName"
                                    <*> b .:? "BuildingId"
                                    <*> b .:? "ShipId"
                                    <*> b .: "StarDate"
    parseJSON _ = mzero


data ProductionChangedNewsDto = ProductionChangedNewsDto
    { productionChangedNewsDtoPlanetId :: !(Key Planet)
    , productionChangedNewsDtoPlanetName :: !PlanetName
    , productionChangedNewsDtoSystemId :: !(Key StarSystem)
    , productionChangedNewsDtoSystemName :: !StarSystemName
    , productionChangedNewsDtoType :: !ResourceType
    , productionChangedNewsDtoDate :: !StarDate
    }
    deriving (Show, Read, Eq)


data ResearchCompletedNewsDto = ResearchCompletedNewsDto
    { researchCompletedNewsDtoTechnology :: !Technology
    , researchCompletedNewsDtoName :: !Text
    , researchCompletedNewsDtoDate :: !StarDate
    }
    deriving (Show, Read, Eq)


{-| Star date of the event that is being reported in this news article
-}
newsStarDate :: NewsArticleDto -> StarDate
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

        ResearchCompletedDto details ->
            researchCompletedNewsDtoDate details

        KragiiDto details ->
            kragiiNewsDtoDate details

        ScurryingSoundsDto details ->
            scurryingSoundsNewsDtoDate details

        NamingPetDto details ->
            namingPetNewsDtoDate details

        SpecialDto (KragiiEventDto details) ->
            kragiiWormsDtoDate details

        SpecialDto (MkScurryingSoundsEventDto details) ->
            scurryingSoundsEventDtoDate details

        SpecialDto (MkNamingPetEventDto details) ->
            namingPetEventDtoDate details


$(deriveJSON defaultOptions { constructorTagModifier = \x -> take (length x - 3) x } ''UserNewsIconDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 14 } ''KragiiWormsEventDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 13 } ''UserOptionDto)
$(deriveJSON defaultOptions { constructorTagModifier = \x -> take (length x - 3) x } ''KragiiWormsChoiceDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 13 } ''KragiiNewsDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 24 } ''ProductionChangedNewsDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 24 } ''ResearchCompletedNewsDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 22 } ''ScurryingSoundsNewsDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 23 } ''ScurryingSoundsEventDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 16 } ''NamingPetNewsDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 17 } ''NamingPetEventDto)
$(deriveJSON defaultOptions { constructorTagModifier = \x -> take (length x - 3) x } ''ScurryingSoundsChoiceDto)
$(deriveJSON defaultOptions { constructorTagModifier = \x -> take (length x - 3) x } ''NamingPetChoiceDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 20 } ''DesignCreatedNewsDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 18 } ''PlanetFoundNewsDto)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 16 } ''StarFoundNewsDto)
