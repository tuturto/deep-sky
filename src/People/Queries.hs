{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}

module People.Queries
    ( PersonLocationSum(..), OnPlanetData(..), OnUnitData(..)
    , getPersonLocation )
    where

import Import
import Data.Aeson ( withObject )
import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import qualified Database.Esqueleto as E
import Report ( CollatedPlanetReport(..), collateReport )
import Space.Data ( PlanetName(..) )
import Vehicles.Data ( CrewPosition(..) )


-- | Domain object for person location
data PersonLocationSum =
    OnPlanet OnPlanetData
    | OnUnit OnUnitData
    | UnknownLocation
    deriving (Show, Read, Eq)


data OnPlanetData = OnPlanetData
    { onPlanetDataPersonId :: !(Key Person)
    , onPlanetDataPlanetId :: !(Key Planet)
    , onPlanetDataStarSystemId :: !(Key StarSystem)
    , onPlanetDataPlanetName :: !PlanetName
    }
    deriving (Show, Read, Eq)


data OnUnitData = OnUnitData
    { onUnitDataPersonId :: !(Key Person)
    , onUnitDataUnitId :: !(Key Unit)
    , onUnitDataCrewPosition :: !CrewPosition
    }
    deriving (Show, Read, Eq)


instance ToJSON PersonLocationSum where
    toJSON (OnPlanet details) =
        object [ "Tag" .= ("OnPlanet" :: Text)
               , "Contents" .= details
               ]

    toJSON (OnUnit details) =
        object [ "Tag" .= ("OnUnit" :: Text)
               , "Contents" .= details
               ]

    toJSON (UnknownLocation) =
        object [ "Tag" .= ("UnknownLocation" :: Text) ]


instance FromJSON PersonLocationSum where
    parseJSON = withObject "contents" $ \o -> do
        tag <- o .: "Tag"
        asum [ do
                guard (tag == ("OnPlanet" :: Text))
                contents <- o .: "Contents"
                return $ OnPlanet contents
             , do
                guard (tag == ("OnUnit" :: Text))
                contents <- o .: "Contents"
                return $ OnUnit contents
             , do
                guard (tag == ("UnknownLocation" :: Text))
                return UnknownLocation
             ]


-- | Get person location and activity from database
-- in case information is not found, return Nothing
getPersonLocation :: (MonadIO m, BaseBackend backend ~ SqlBackend,
    BackendCompatible SqlBackend backend,
    PersistQueryRead backend, PersistUniqueRead backend) =>
    Key Faction -> Key Person -> ReaderT backend m (Maybe PersonLocationSum)
getPersonLocation fId pId = do
    res <- E.select $
            E.from $ \(personLocation `E.LeftOuterJoin` personOnPlanet `E.LeftOuterJoin` personOnUnit) -> do
                E.on (personOnUnit E.?. PersonOnUnitId E.==. personLocation E.?. PersonLocationOnUnit)
                E.on (personOnPlanet E.?. PersonOnPlanetId E.==. personLocation E.?. PersonLocationOnPlanet)
                E.where_ (personOnUnit E.?. PersonOnUnitPersonId E.==. (E.val $ Just pId)
                         E.||. personOnPlanet E.?. PersonOnPlanetPersonId E.==. (E.val $ Just pId))
                return (personLocation, personOnPlanet, personOnUnit)

    pRep <- mapM (getPlanetReport fId) (getPlanetId res)

    return $ locationResToLocationSum res pRep


-- | Map joined entitites into possible planet id
getPlanetId :: [(Maybe (Entity PersonLocation), Maybe (Entity PersonOnPlanet), Maybe (Entity PersonOnUnit))]
    -> Maybe PlanetId
getPlanetId ((Just _, Just personOnPlanet, Nothing):_) =
    (Just . personOnPlanetPlanetId . entityVal) personOnPlanet

getPlanetId _ =
    Nothing


-- | Retrieve current planet report for faction
getPlanetReport :: (MonadIO m, BaseBackend backend ~ SqlBackend,
    BackendCompatible SqlBackend backend,
    PersistQueryRead backend, PersistUniqueRead backend) =>
    Key Faction -> Key Planet -> ReaderT backend m CollatedPlanetReport
getPlanetReport fId pId = do
    pReps <- selectList [ PlanetReportFactionId ==. fId
                        , PlanetReportPlanetId ==. pId ] [ Desc PlanetReportDate ]

    return $ collateReport $ fmap entityVal pReps


-- | Translate data loaded from database into Maybe PersonLocationSum
-- makes assumption that only zero or one entries are given
-- multiple entries in the list will result Nothing
locationResToLocationSum :: [(Maybe (Entity PersonLocation), Maybe (Entity PersonOnPlanet), Maybe (Entity PersonOnUnit))]
    -> Maybe CollatedPlanetReport
    -> Maybe PersonLocationSum
locationResToLocationSum ((Just _, Just personOnPlanet, Nothing):_)
                         (Just (CollatedPlanetReport { cprName = Just pName
                                                     , cprSystemId = sId})) =
    Just $ OnPlanet details
    where
        details = OnPlanetData
                    { onPlanetDataPersonId = personOnPlanetPersonId poPlanet
                    , onPlanetDataPlanetId = personOnPlanetPlanetId poPlanet
                    , onPlanetDataStarSystemId = sId
                    , onPlanetDataPlanetName = pName
                    }
        poPlanet = entityVal personOnPlanet

locationResToLocationSum ((Just _, Nothing, Just personOnUnit):_) _ =
    Just $ OnUnit details
    where
        details = OnUnitData
                    { onUnitDataPersonId = personOnUnitPersonId poUnit
                    , onUnitDataUnitId = personOnUnitUnitId poUnit
                    , onUnitDataCrewPosition = personOnUnitPosition poUnit
                    }
        poUnit = entityVal personOnUnit

locationResToLocationSum _ _ =
    Just UnknownLocation


$(deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''OnPlanetData)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''OnUnitData)
