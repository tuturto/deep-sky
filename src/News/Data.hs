{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}

module News.Data ( NewsArticle(..), UserNewsIcon(..), StarFoundNews(..)
                 , PlanetFoundNews(..), UserWrittenNews(..), DesignCreatedNews(..)
                 , ConstructionFinishedNews(..), SpecialNews(..), ProductionChangedNews(..)
                 , ResearchCompletedNews(..), mkFactionNews, mkFactionSpecialNews
                 , mkPersonalNews, mkPersonalSpecialNews, resolveType
                 )
    where

import Import
import Data.Aeson.TH
import Data.Aeson.Text ( encodeToLazyText )
import Common ( ToDto(..), FromDto(..) )
import CustomTypes ( SpecialEventStatus(..), StarDate )
import Dto.Icons ( IconMapper(..) )
import Dto.News ( NewsDto(..), NewsArticleDto(..), StarFoundNewsDto(..), PlanetFoundNewsDto(..)
                , UserWrittenNewsDto(..), DesignCreatedNewsDto(..), ConstructionFinishedNewsDto(..)
                , UserNewsIconDto(..), SpecialNewsDto(..), KragiiWormsEventDto(..)
                , ProductionChangedNewsDto(..), ResearchCompletedNewsDto(..)
                , ScurryingSoundsEventDto(..), NamingPetEventDto(..), UserOptionDto(..)
                , NamingPetChoiceDto(..)
                )
import Events.Import ( UserOption(..), EventResolveType(..) )
import qualified Events.Import as EI ( resolveType )
import Events.Kragii ( KragiiWormsEvent(..), KragiiWormsChoice(..), KragiiNews(..) )
import Events.Pets ( ScurryingSoundsEvent(..), ScurryingSoundsChoice(..), ScurryingSoundsNews(..)
                   , NamingPetEvent(..), NamingPetChoice(..), NamingPetNews(..)
                   )
import People.Data ( PersonName, PetName )
import Research.Data ( Technology, Research(..) )
import Research.Tree ( techMap )
import Resources ( ResourceType )
import Space.Data ( PlanetName(..), StarName(..), StarSystemName(..) )


-- | All possible news articles
data NewsArticle =
    StarFound StarFoundNews
    | PlanetFound PlanetFoundNews
    | UserWritten UserWrittenNews
    | DesignCreated DesignCreatedNews
    | ConstructionFinished ConstructionFinishedNews
    | ProductionBoostStarted ProductionChangedNews
    | ProductionSlowdownStarted ProductionChangedNews
    | ProductionBoostEnded ProductionChangedNews
    | ProductionSlowdownEnded ProductionChangedNews
    | ResearchCompleted ResearchCompletedNews
    | KragiiResolution KragiiNews
    | ScurryingSoundsResolution ScurryingSoundsNews
    | NamingPetResolution NamingPetNews
    | Special SpecialNews


-- | News announcing discovery of a new star
data StarFoundNews = StarFoundNews
    { starFoundNewsStarName :: !StarName
    , starFoundNewsSystemName :: !StarSystemName
    , starFoundNewsSystemId :: !(Key StarSystem)
    , starFoundNewsDate :: !StarDate
    }


instance ToDto StarFoundNews StarFoundNewsDto where
    toDto news =
        StarFoundNewsDto { starFoundNewsDtoStarName = starFoundNewsStarName news
                         , starFoundNewsDtoSystemName = starFoundNewsSystemName news
                         , starFoundNewsDtoSystemId = starFoundNewsSystemId news
                         , starFoundNewsDtoDate = starFoundNewsDate news
                         }


instance FromDto StarFoundNews StarFoundNewsDto where
    fromDto dto =
        StarFoundNews { starFoundNewsStarName = starFoundNewsDtoStarName dto
                      , starFoundNewsSystemName = starFoundNewsDtoSystemName dto
                      , starFoundNewsSystemId = starFoundNewsDtoSystemId dto
                      , starFoundNewsDate = starFoundNewsDtoDate dto
                      }


-- | News announcing discovery of a new planet
data PlanetFoundNews = PlanetFoundNews
    { planetFoundNewsPlanetName :: !PlanetName
    , planetFoundNewsSystemName :: !StarSystemName
    , planetFoundNewsSystemId :: !(Key StarSystem)
    , planetFoundNewsPlanetId :: !(Key Planet)
    , planetFoundNewsDate :: !StarDate
    }


instance ToDto PlanetFoundNews PlanetFoundNewsDto where
    toDto news =
        PlanetFoundNewsDto { planetFoundNewsDtoPlanetName = planetFoundNewsPlanetName news
                           , planetFoundNewsDtoSystemName = planetFoundNewsSystemName news
                           , planetFoundNewsDtoSystemId = planetFoundNewsSystemId news
                           , planetFoundNewsDtoPlanetId = planetFoundNewsPlanetId news
                           , planetFoundNewsDtoDate = planetFoundNewsDate news
                           }


instance FromDto PlanetFoundNews PlanetFoundNewsDto where
    fromDto dto =
        PlanetFoundNews { planetFoundNewsPlanetName = planetFoundNewsDtoPlanetName dto
                        , planetFoundNewsSystemName = planetFoundNewsDtoSystemName dto
                        , planetFoundNewsSystemId = planetFoundNewsDtoSystemId dto
                        , planetFoundNewsPlanetId = planetFoundNewsDtoPlanetId dto
                        , planetFoundNewsDate = planetFoundNewsDtoDate dto
                        }


-- | User supplied news
data UserWrittenNews = UserWrittenNews
    { userWrittenNewsContent :: !Text
    , userWrittenNewsIcon :: !UserNewsIcon
    , userWrittenNewsDate :: !StarDate
    , userWrittenNewsUser :: !PersonName
    }


instance ToDto UserWrittenNews UserWrittenNewsDto where
    toDto news =
        UserWrittenNewsDto { userWrittenNewsDtoContent = userWrittenNewsContent news
                           , userWrittenNewsDtoDate = userWrittenNewsDate  news
                           , userWrittenNewsDtoUser = userWrittenNewsUser news
                           , userWrittenNewsDtoIcon = toDto $ userWrittenNewsIcon news
                           }


instance FromDto UserWrittenNews UserWrittenNewsDto where
    fromDto dto =
        UserWrittenNews { userWrittenNewsContent = userWrittenNewsDtoContent dto
                        , userWrittenNewsIcon = fromDto $ userWrittenNewsDtoIcon dto
                        , userWrittenNewsDate = userWrittenNewsDtoDate dto
                        , userWrittenNewsUser = userWrittenNewsDtoUser dto
                        }


-- | News announcing creation of a new design
data DesignCreatedNews = DesignCreatedNews
    { designCreatedNewsDesignId :: !(Key Design)
    , designCreatedNewsName :: !Text
    , designCreatedDate :: !StarDate
    }


instance ToDto DesignCreatedNews DesignCreatedNewsDto where
    toDto news =
        DesignCreatedNewsDto { designCreatedNewsDtoDesignId = designCreatedNewsDesignId news
                             , designCreatedNewsDtoName = designCreatedNewsName news
                             , designCreatedNewsDtoDate = designCreatedDate news
                             }


instance FromDto DesignCreatedNews DesignCreatedNewsDto where
    fromDto dto =
        DesignCreatedNews { designCreatedNewsDesignId = designCreatedNewsDtoDesignId dto
                          , designCreatedNewsName = designCreatedNewsDtoName dto
                          , designCreatedDate = designCreatedNewsDtoDate dto
                          }


data ConstructionFinishedNews = ConstructionFinishedNews
    { constructionFinishedNewsPlanetName :: !(Maybe PlanetName)
    , constructionFinishedNewsPlanetId :: !(Maybe (Key Planet))
    , constructionFinishedNewsSystemName :: !StarSystemName
    , constructionFinishedNewsSystemId :: !(Key StarSystem)
    , constructionFinishedConstructionName :: !Text
    , constructionFinishedBuildingId :: !(Maybe (Key Building))
    , constructionFinishedShipId :: !(Maybe (Key Ship))
    , constructionFinishedDate :: !StarDate
    }


instance ToDto ConstructionFinishedNews ConstructionFinishedNewsDto where
    toDto news =
        ConstructionFinishedNewsDto { constructionFinishedNewsDtoPlanetName = constructionFinishedNewsPlanetName news
                                    , constructionFinishedNewsDtoPlanetId = constructionFinishedNewsPlanetId news
                                    , constructionFinishedNewsDtoSystemName = constructionFinishedNewsSystemName news
                                    , constructionFinishedNewsDtoSystemId = constructionFinishedNewsSystemId news
                                    , constructionFinishedNewsDtoConstructionName = constructionFinishedConstructionName news
                                    , constructionFinishedNewsDtoBuildingId = constructionFinishedBuildingId news
                                    , constructionFinishedNewsDtoShipId = constructionFinishedShipId news
                                    , constructionFinishedNewsDtoDate = constructionFinishedDate news
                                    }


instance FromDto ConstructionFinishedNews ConstructionFinishedNewsDto where
    fromDto dto =
        ConstructionFinishedNews { constructionFinishedNewsPlanetName = constructionFinishedNewsDtoPlanetName dto
                                 , constructionFinishedNewsPlanetId = constructionFinishedNewsDtoPlanetId dto
                                 , constructionFinishedNewsSystemName = constructionFinishedNewsDtoSystemName dto
                                 , constructionFinishedNewsSystemId = constructionFinishedNewsDtoSystemId dto
                                 , constructionFinishedConstructionName = constructionFinishedNewsDtoConstructionName dto
                                 , constructionFinishedBuildingId = constructionFinishedNewsDtoBuildingId dto
                                 , constructionFinishedShipId = constructionFinishedNewsDtoShipId dto
                                 , constructionFinishedDate = constructionFinishedNewsDtoDate dto
                                 }


data ProductionChangedNews = ProductionChangedNews
    { productionChangedNewsPlanetId :: !(Key Planet)
    , productionChangedNewsPlanetName :: !PlanetName
    , productionChangedNewsSystemId :: !(Key StarSystem)
    , productionChangedNewsSystemName :: !StarSystemName
    , productionChangedNewsType :: !ResourceType
    , productionChangedNewsDate :: !StarDate
    }


instance ToDto ProductionChangedNews ProductionChangedNewsDto where
    toDto news = ProductionChangedNewsDto
        { productionChangedNewsDtoPlanetId = productionChangedNewsPlanetId news
        , productionChangedNewsDtoPlanetName = productionChangedNewsPlanetName news
        , productionChangedNewsDtoSystemId = productionChangedNewsSystemId news
        , productionChangedNewsDtoSystemName = productionChangedNewsSystemName news
        , productionChangedNewsDtoType = productionChangedNewsType news
        , productionChangedNewsDtoDate = productionChangedNewsDate news
        }


instance FromDto ProductionChangedNews ProductionChangedNewsDto where
    fromDto dto = ProductionChangedNews
        { productionChangedNewsPlanetId = productionChangedNewsDtoPlanetId dto
        , productionChangedNewsPlanetName = productionChangedNewsDtoPlanetName dto
        , productionChangedNewsSystemId = productionChangedNewsDtoSystemId dto
        , productionChangedNewsSystemName = productionChangedNewsDtoSystemName dto
        , productionChangedNewsType = productionChangedNewsDtoType dto
        , productionChangedNewsDate = productionChangedNewsDtoDate dto
        }


data ResearchCompletedNews = ResearchCompletedNews
    { researchCompletedNewsTechnology :: !Technology
    , researchCompletedNewsData :: !StarDate
    }


instance ToDto ResearchCompletedNews ResearchCompletedNewsDto where
    toDto news = ResearchCompletedNewsDto
        { researchCompletedNewsDtoTechnology = tech
        , researchCompletedNewsDtoName = name
        , researchCompletedNewsDtoDate = researchCompletedNewsData news
        }
        where
            tech = researchCompletedNewsTechnology news
            name = researchName . techMap $ tech

instance FromDto ResearchCompletedNews ResearchCompletedNewsDto where
    fromDto dto = ResearchCompletedNews
        { researchCompletedNewsTechnology = researchCompletedNewsDtoTechnology dto
        , researchCompletedNewsData = researchCompletedNewsDtoDate dto
        }


instance ToDto ((Key News, NewsArticle), IconMapper NewsArticleDto) NewsDto where
    toDto ((nId, article), icons) =
        let
            content = toDto article
        in
        NewsDto { newsDtoId = nId
                , newsContents = content
                , newsIcon = runIconMapper icons content
                }


instance FromDto NewsArticle NewsArticleDto where
    fromDto dto =
        case dto of
            StarFoundDto content ->
                StarFound $ fromDto content

            PlanetFoundDto content ->
                PlanetFound $ fromDto content

            UserWrittenDto content ->
                UserWritten $ fromDto content

            DesignCreatedDto content ->
                DesignCreated $ fromDto content

            ConstructionFinishedDto content ->
                ConstructionFinished $ fromDto content

            ProductionBoostStartedDto content ->
                ProductionBoostStarted $ fromDto content

            ProductionSlowdownStartedDto content ->
                ProductionSlowdownStarted $ fromDto content

            ProductionBoostEndedDto content ->
                ProductionBoostEnded $ fromDto content

            ProductionSlowdownEndedDto content ->
                ProductionSlowdownEnded $ fromDto content

            ResearchCompletedDto content ->
                ResearchCompleted $ fromDto content

            KragiiDto content ->
                KragiiResolution $ fromDto content

            ScurryingSoundsDto content ->
                ScurryingSoundsResolution $ fromDto content

            NamingPetDto content ->
                NamingPetResolution $ fromDto content

            SpecialDto content ->
                Special $ fromDto content


instance ToDto NewsArticle NewsArticleDto where
    toDto news =
        case news of
            (StarFound x) -> StarFoundDto $ toDto x
            (PlanetFound x) -> PlanetFoundDto $ toDto x
            (UserWritten x) -> UserWrittenDto $ toDto x
            (DesignCreated x) -> DesignCreatedDto $ toDto x
            (ConstructionFinished x) -> ConstructionFinishedDto $ toDto x
            (ProductionBoostStarted x) -> ProductionBoostStartedDto $ toDto x
            (ProductionSlowdownStarted x) -> ProductionSlowdownStartedDto $ toDto x
            (ProductionBoostEnded x) -> ProductionBoostEndedDto $ toDto x
            (ProductionSlowdownEnded x) -> ProductionSlowdownEndedDto $ toDto x
            (ResearchCompleted x) -> ResearchCompletedDto $ toDto x
            (KragiiResolution x) -> KragiiDto $ toDto x
            (ScurryingSoundsResolution x) -> ScurryingSoundsDto $ toDto x
            (NamingPetResolution x) -> NamingPetDto $ toDto x
            (Special x) -> SpecialDto $ toDto x


-- | Special news that require player interaction
data SpecialNews
    = KragiiWorms KragiiWormsEvent [UserOption KragiiWormsChoice] (Maybe KragiiWormsChoice)
    | ScurryingSounds ScurryingSoundsEvent [(UserOption ScurryingSoundsChoice)] (Maybe ScurryingSoundsChoice)
    | NamingPet NamingPetEvent [(UserOption NamingPetChoice)] (Maybe NamingPetChoice)
    deriving (Show, Read, Eq)


-- | How event in special news is resolved
resolveType :: NewsArticle -> Maybe EventResolveType
resolveType (Special article) =
    case article of
        (KragiiWorms event _ _) ->
            Just $ EI.resolveType event

        (ScurryingSounds event _ _) ->
            Just $ EI.resolveType event

        (NamingPet event _ _) ->
            Just $ EI.resolveType event

resolveType _ =
    Nothing


instance ToDto SpecialNews SpecialNewsDto where
    toDto (KragiiWorms event options choice) =
        KragiiEventDto $ KragiiWormsEventDto
            { kragiiWormsDtoPlanetId = kragiiWormsPlanetId event
            , kragiiWormsDtoPlanetName = kragiiWormsPlanetName event
            , kragiiWormsDtoSystemId = kragiiWormsSystemId event
            , kragiiWormsDtoSystemName = kragiiWormsSystemName event
            , kragiiWormsDtoOptions = fmap toDto options
            , kragiiWormsDtoChoice = fmap toDto choice
            , kragiiWormsDtoFactionId = kragiiWormsFactionId event
            , kragiiWormsDtoDate = kragiiWormsDate event
            , kragiiWormsDtoResolveType = Just $ EI.resolveType event
            }

    toDto (ScurryingSounds event options choice) =
        MkScurryingSoundsEventDto $ ScurryingSoundsEventDto
            { scurryingSoundsEventDtoPersonId = scurryingSoundsEventPersonId event
            , scurryingSoundsEventDtoDate = scurryingSoundsEventDate event
            , scurryingSoundsEventDtoOptions = toDto <$> options
            , scurryingSoundsEventDtoChoice = toDto <$> choice
            , scurryingSoundsEventDtoResolveType = Just $ EI.resolveType event
            }

    toDto (NamingPet event options choice) =
        MkNamingPetEventDto $ NamingPetEventDto
            { namingPetEventDtoPersonId = namingPetEventPersonId event
            , namingPetEventDtoPetId = namingPetEventPetId event
            , namingPetEventDtoPetType = namingPetEventPetType event
            , namingPetEventDtoDate = namingPetEventDate event
            , namingPetEventDtoOptions = toDto <$> options
            , namingPetEventDtoChoice = toDto <$> choice
            , namingPetEventDtoResolveType = Just $ EI.resolveType event
            }


instance FromDto SpecialNews SpecialNewsDto where
    fromDto (KragiiEventDto dto) =
        KragiiWorms ( KragiiWormsEvent
                        { kragiiWormsPlanetId = kragiiWormsDtoPlanetId dto
                        , kragiiWormsPlanetName = kragiiWormsDtoPlanetName dto
                        , kragiiWormsSystemId = kragiiWormsDtoSystemId dto
                        , kragiiWormsSystemName = kragiiWormsDtoSystemName dto
                        , kragiiWormsFactionId = kragiiWormsDtoFactionId dto
                        , kragiiWormsDate = kragiiWormsDtoDate dto
                        } )
                    []
                    (fromDto <$> kragiiWormsDtoChoice dto)

    fromDto (MkScurryingSoundsEventDto dto) =
        ScurryingSounds ( ScurryingSoundsEvent
                            { scurryingSoundsEventPersonId = scurryingSoundsEventDtoPersonId dto
                            , scurryingSoundsEventDate = scurryingSoundsEventDtoDate dto } )
                        []
                        (fromDto <$> scurryingSoundsEventDtoChoice dto)

    fromDto (MkNamingPetEventDto dto) =
        NamingPet ( NamingPetEvent
                    { namingPetEventPersonId = namingPetEventDtoPersonId dto
                    , namingPetEventPetId = namingPetEventDtoPetId dto
                    , namingPetEventPetType = namingPetEventDtoPetType dto
                    , namingPetEventDate = namingPetEventDtoDate dto
                    , namingPetNameOptions =  mapMaybe foo (namingPetEventDtoOptions dto)
                    } )
                  []
                  (fromDto <$> namingPetEventDtoChoice dto)


foo :: UserOptionDto NamingPetChoiceDto -> Maybe PetName
foo dto =
    case userOptionDtoChoice dto of
        GiveNameDto name ->
            Just name

        LetSomeoneElseDecideDto ->
            Nothing


-- | Icon for user created news
data UserNewsIcon =
    GenericUserNews
    | JubilationUserNews
    | CatUserNews
    deriving (Show, Read, Eq)


instance ToDto UserNewsIcon UserNewsIconDto where
    toDto icon =
        case icon of
            GenericUserNews ->
                GenericUserNewsDto

            JubilationUserNews ->
                JubilationUserNewsDto

            CatUserNews ->
                CatUserNewsDto


instance FromDto UserNewsIcon UserNewsIconDto where
    fromDto icon =
        case icon of
            GenericUserNewsDto ->
                GenericUserNews

            JubilationUserNewsDto ->
                JubilationUserNews

            CatUserNewsDto ->
                CatUserNews


-- | Helper function for creating News that aren't special events and haven't been dismissed
mkFactionNews :: Key Faction -> StarDate -> NewsArticle -> News
mkFactionNews fId date content =
    News { newsContent = toStrict $ encodeToLazyText content
         , newsFactionId = Just fId
         , newsPersonId = Nothing
         , newsDate = date
         , newsDismissed = False
         , newsSpecialEvent = NoSpecialEvent
         }


-- | Helper function for creating News that aren't special events and haven't been dismissed
mkPersonalNews :: Key Person -> StarDate -> NewsArticle -> News
mkPersonalNews pId date content =
    News { newsContent = toStrict $ encodeToLazyText content
         , newsFactionId = Nothing
         , newsPersonId = Just pId
         , newsDate = date
         , newsDismissed = False
         , newsSpecialEvent = NoSpecialEvent
         }


-- | Helper function for creating News that are special events and haven't been handled
mkFactionSpecialNews :: StarDate -> Key Faction -> SpecialNews -> News
mkFactionSpecialNews date fId content =
    News { newsContent = toStrict $ encodeToLazyText $ Special content
         , newsFactionId = Just fId
         , newsPersonId = Nothing
         , newsDate = date
         , newsDismissed = False
         , newsSpecialEvent = UnhandledSpecialEvent
         }


-- | Helper function for creating News that are special events and haven't been handled
mkPersonalSpecialNews :: StarDate -> Key Person -> SpecialNews -> News
mkPersonalSpecialNews date pId content =
    News { newsContent = toStrict $ encodeToLazyText $ Special content
         , newsFactionId = Nothing
         , newsPersonId = Just pId
         , newsDate = date
         , newsDismissed = False
         , newsSpecialEvent = UnhandledSpecialEvent
         }


$(deriveJSON defaultOptions ''StarFoundNews)
$(deriveJSON defaultOptions ''PlanetFoundNews)
$(deriveJSON defaultOptions ''UserWrittenNews)
$(deriveJSON defaultOptions ''DesignCreatedNews)
$(deriveJSON defaultOptions ''ConstructionFinishedNews)
$(deriveJSON defaultOptions ''UserNewsIcon)
$(deriveJSON defaultOptions ''SpecialNews)
$(deriveJSON defaultOptions ''NewsArticle)
$(deriveJSON defaultOptions ''ProductionChangedNews)
$(deriveJSON defaultOptions ''ResearchCompletedNews)
