{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TemplateHaskell            #-}

module Report ( createPlanetReports, createStarReports, createStarLaneReports, createSystemReport
              , collateReports, collateReport, spectralInfo, createPlanetStatusReport
              , planetStatusIconMapper
              , CollatedPlanetReport(..), CollatedStarReport(..), CollatedStarLaneReport(..)
              , CollatedBuildingReport(..), CollatedPopulationReport(..), CollatedStarSystemReport(..)
              , CollatedBaseReport(..), CollatedPlanetStatusReport(..) )
    where

import Import
import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import Data.Monoid ()
import Database.Persist.Sql (toSqlKey)

import CustomTypes
import Dto.Icons ( IconMapper(..) )


-- | Class to transform a report stored in db to respective collated report
class ReportTransform a b | a -> b where
    fromReport :: a -> b


-- | Class to indicate if two reports are about same entity
class Grouped a where
    sameGroup :: a -> a -> Bool


data CollatedStarSystemReport = CollatedStarSystemReport
    { cssrSystemId :: Key StarSystem
    , cssrName     :: Maybe Text
    , cssrLocation :: Coordinates
    , cssrDate     :: Int
    } deriving Show


instance Semigroup CollatedStarSystemReport where
    (<>) a b = CollatedStarSystemReport
                { cssrSystemId = cssrSystemId a
                , cssrName = cssrName a <|> cssrName b
                , cssrLocation = cssrLocation a
                , cssrDate = max (cssrDate a) (cssrDate b)
                }


instance Monoid CollatedStarSystemReport where
    mempty = CollatedStarSystemReport
                { cssrSystemId = toSqlKey 0
                , cssrName = Nothing
                , cssrLocation = Coordinates 0 0
                , cssrDate = 0
                }


instance ReportTransform StarSystemReport CollatedStarSystemReport where
    fromReport report =
        CollatedStarSystemReport
            { cssrSystemId = starSystemReportStarSystemId report
            , cssrName = starSystemReportName report
            , cssrLocation = Coordinates (starSystemReportCoordX report) (starSystemReportCoordY report)
            , cssrDate = starSystemReportDate report
            }


instance Grouped StarSystemReport where
    sameGroup a b =
        starSystemReportStarSystemId a == starSystemReportStarSystemId b


data CollatedStarReport = CollatedStarReport {
      csrStarId          :: Key Star
    , csrSystemId        :: Key StarSystem
    , csrName            :: Maybe Text
    , csrSpectralType    :: Maybe SpectralType
    , csrLuminosityClass :: Maybe LuminosityClass
    , csrDate            :: Int
} deriving Show


instance Semigroup CollatedStarReport where
    (<>) a b = CollatedStarReport (csrStarId a)
                                  (csrSystemId a)
                                  (csrName a <|> csrName b)
                                  (csrSpectralType a <|> csrSpectralType b)
                                  (csrLuminosityClass a <|> csrLuminosityClass b)
                                  (max (csrDate a) (csrDate b))


instance Monoid CollatedStarReport where
    mempty = CollatedStarReport (toSqlKey 0) (toSqlKey 0) Nothing Nothing Nothing 0


instance ReportTransform StarReport CollatedStarReport where
    fromReport report =
        CollatedStarReport (starReportStarId report)
                           (starReportStarSystemId report)
                           (starReportName report)
                           (starReportSpectralType report)
                           (starReportLuminosityClass report)
                           (starReportDate report)


instance Grouped StarReport where
    sameGroup a b =
        starReportStarId a == starReportStarId b


instance ToJSON CollatedStarReport where
  toJSON CollatedStarReport { csrStarId = rId
                            , csrSystemId = rSId
                            , csrName = rName
                            , csrSpectralType = rSpectral
                            , csrLuminosityClass = rLuminosity
                            , csrDate = rDate } =
    object [ "id" .= rId
           , "systemId" .= rSId
           , "name" .= rName
           , "spectralType" .= rSpectral
           , "luminosityClass" .= rLuminosity
           , "date" .= rDate ]


data CollatedPlanetReport = CollatedPlanetReport
    { cprPlanetId :: Key Planet
    , cprSystemId :: Key StarSystem
    , cprOwnerId  :: Maybe (Key Faction)
    , cprName     :: Maybe Text
    , cprPosition :: Maybe Int
    , cprGravity  :: Maybe Double
    , cprDate     :: Int
    } deriving Show


instance Semigroup CollatedPlanetReport where
    (<>) a b = CollatedPlanetReport (cprPlanetId a)
                                    (cprSystemId a)
                                    (cprOwnerId a <|> cprOwnerId b)
                                    (cprName a <|> cprName b)
                                    (cprPosition a <|> cprPosition b)
                                    (cprGravity a <|> cprGravity b)
                                    (max (cprDate a) (cprDate b))


instance Monoid CollatedPlanetReport where
    mempty = CollatedPlanetReport (toSqlKey 0) (toSqlKey 0) Nothing Nothing Nothing Nothing 0


instance ReportTransform PlanetReport CollatedPlanetReport where
    fromReport report =
        CollatedPlanetReport (planetReportPlanetId report)
                             (planetReportStarSystemId report)
                             (planetReportOwnerId report)
                             (planetReportName report)
                             (planetReportPosition report)
                             (planetReportGravity report)
                             (planetReportDate report)


instance Grouped PlanetReport where
    sameGroup a b =
        planetReportPlanetId a == planetReportPlanetId b


instance ToJSON CollatedPlanetReport where
  toJSON CollatedPlanetReport { cprPlanetId = rId
                              , cprSystemId = rSId
                              , cprOwnerId = rOId
                              , cprName = rName
                              , cprPosition = rPosition
                              , cprGravity = rGravity
                              , cprDate = rDate } =
    object [ "id" .= rId
           , "systemId" .= rSId
           , "name" .= rName
           , "position" .= rPosition
           , "gravity" .= rGravity
           , "ownerId" .= rOId
           , "date" .= rDate ]


data CollatedPopulationReport = CollatedPopulationReport
    { cpopPlanetId   :: Key Planet
    , cpopRaceId     :: Maybe (Key Race)
    , cpopRace       :: Maybe Text
    , cpopPopulation :: Maybe Int
    , cpopDate       :: Int
    } deriving Show


instance ToJSON CollatedPopulationReport where
    toJSON CollatedPopulationReport { cpopPlanetId = pId
                                    , cpopRaceId = rRaceId
                                    , cpopRace = rRace
                                    , cpopPopulation = rPop
                                    , cpopDate = rDate } =
        object [ "planetId" .= pId
               , "raceId" .= rRaceId
               , "race" .= rRace
               , "inhabitants" .= rPop
               , "date" .= rDate ]


instance Semigroup CollatedPopulationReport where
    (<>) a b = CollatedPopulationReport (cpopPlanetId a)
                                        (cpopRaceId a <|> cpopRaceId b)
                                        (cpopRace a <|> cpopRace b)
                                        (cpopPopulation a <|> cpopPopulation b)
                                        (max (cpopDate a) (cpopDate b))


instance Monoid CollatedPopulationReport where
    mempty = CollatedPopulationReport (toSqlKey 0) Nothing Nothing Nothing 0


instance ReportTransform (PlanetPopulationReport, Maybe Race) CollatedPopulationReport where
    fromReport (report, pRace) =
        CollatedPopulationReport (planetPopulationReportPlanetId report)
                                 (planetPopulationReportRaceId report)
                                 (fmap raceName pRace)
                                 (planetPopulationReportPopulation report)
                                 (planetPopulationReportDate report)


instance Grouped (PlanetPopulationReport, Maybe Race) where
    sameGroup (a, _) (b, _) =
        planetPopulationReportPlanetId a == planetPopulationReportPlanetId b &&
            planetPopulationReportRaceId a == planetPopulationReportRaceId b


data CollatedPlanetStatusReport = CollatedPlanetStatusReport
    { collatedPlanetStatusReportPlanetId :: Key Planet
    , collatedPlanetStatusReportStatus :: [PlanetaryStatusInfo]
    , collatedPlanetStatusReportDate :: Int
    }
    deriving (Show, Read, Eq)


instance Semigroup CollatedPlanetStatusReport where
    (<>) a _ = a


instance Monoid CollatedPlanetStatusReport where
        mempty = CollatedPlanetStatusReport
                    { collatedPlanetStatusReportPlanetId = (toSqlKey 0)
                    , collatedPlanetStatusReportStatus = []
                    , collatedPlanetStatusReportDate = 0
                    }


instance Grouped (PlanetStatusReport, IconMapper PlanetaryStatus) where
    sameGroup (a, _) (b, _) =
        planetStatusReportPlanetId a == planetStatusReportPlanetId b


instance ReportTransform (PlanetStatusReport, IconMapper PlanetaryStatus) CollatedPlanetStatusReport where
    fromReport (report, icons) =
        CollatedPlanetStatusReport
            { collatedPlanetStatusReportPlanetId = planetStatusReportPlanetId report
            , collatedPlanetStatusReportStatus = fmap (statusToInfo icons) $ planetStatusReportStatus report
            , collatedPlanetStatusReportDate = planetStatusReportDate report
            }


statusToInfo :: IconMapper PlanetaryStatus -> PlanetaryStatus -> PlanetaryStatusInfo
statusToInfo icons status =
    PlanetaryStatusInfo
        { planetaryStatusInfoStatus = status
        , planetaryStatusInfoDescription = statusDescription status
        , planetaryStatusInfoIcon = runIconMapper icons status
        }


data PlanetaryStatusInfo = PlanetaryStatusInfo
    { planetaryStatusInfoStatus :: PlanetaryStatus
    , planetaryStatusInfoDescription :: Text
    , planetaryStatusInfoIcon :: Text
    }
    deriving (Show, Read, Eq)


data CollatedStarLaneReport = CollatedStarLaneReport
    { cslStarLaneId      :: Key StarLane
    , cslSystemId1       :: Key StarSystem
    , cslSystemId2       :: Key StarSystem
    , cslStarSystemName1 :: Maybe Text
    , cslStarSystemName2 :: Maybe Text
    , cslDate            :: Int
    } deriving Show


instance Semigroup CollatedStarLaneReport where
    (<>) a b = CollatedStarLaneReport
                { cslStarLaneId = cslStarLaneId a
                , cslSystemId1 = cslSystemId1 a
                , cslSystemId2 = cslSystemId2 a
                , cslStarSystemName1 = cslStarSystemName1 a <|> cslStarSystemName1 b
                , cslStarSystemName2 = cslStarSystemName2 a <|> cslStarSystemName2 b
                , cslDate = max (cslDate a) (cslDate b)
                }


instance Monoid CollatedStarLaneReport where
    mempty = CollatedStarLaneReport
                { cslStarLaneId = toSqlKey 0
                , cslSystemId1 = toSqlKey 0
                , cslSystemId2 = toSqlKey 0
                , cslStarSystemName1 = Nothing
                , cslStarSystemName2 = Nothing
                , cslDate = 0
                }


instance ReportTransform StarLaneReport CollatedStarLaneReport where
    fromReport report = CollatedStarLaneReport
                { cslStarLaneId = starLaneReportStarLaneId report
                , cslSystemId1 = starLaneReportStarSystem1 report
                , cslSystemId2 = starLaneReportStarSystem2 report
                , cslStarSystemName1 = starLaneReportStarSystemName1 report
                , cslStarSystemName2 = starLaneReportStarSystemName2 report
                , cslDate = starLaneReportDate report
                }


instance Grouped StarLaneReport where
    sameGroup a b =
        starLaneReportStarSystem1 a == starLaneReportStarSystem1 b &&
        starLaneReportStarSystem2 a == starLaneReportStarSystem2 b


data CollatedBaseReport = CollatedBaseReport {
      cbsPlanetReport   :: CollatedPlanetReport
    , cbsStarSystemName :: Text
} deriving Show


data CollatedBuildingReport = CollatedBuildingReport
    { cbrBuildingId   :: Key Building
    , cbrPlanetId     :: Key Planet
    , cbrType         :: Maybe BuildingType
    , cbrLevel        :: Maybe Int
    , cbrDamage       :: Maybe Double
    , cbrDate         :: Int
    } deriving Show


instance Semigroup CollatedBuildingReport where
    (<>) a b = CollatedBuildingReport
                { cbrBuildingId = cbrBuildingId a
                , cbrPlanetId = cbrPlanetId a
                , cbrType = cbrType a <|> cbrType b
                , cbrLevel = cbrLevel a <|> cbrLevel b
                , cbrDamage = cbrDamage a <|> cbrDamage b
                , cbrDate = max (cbrDate a) (cbrDate b)
                }


instance Monoid CollatedBuildingReport where
    mempty = CollatedBuildingReport
                { cbrBuildingId = toSqlKey 0
                , cbrPlanetId = toSqlKey 0
                , cbrType = Nothing
                , cbrLevel = Nothing
                , cbrDamage = Nothing
                , cbrDate = 0
                }


instance ReportTransform BuildingReport CollatedBuildingReport where
    fromReport report =
        CollatedBuildingReport
                { cbrBuildingId = buildingReportBuildingId report
                , cbrPlanetId = buildingReportPlanetId report
                , cbrType = buildingReportType report
                , cbrLevel = buildingReportLevel report
                , cbrDamage = buildingReportDamage report
                , cbrDate = buildingReportDate report
                }


instance Grouped BuildingReport where
    sameGroup a b =
        buildingReportBuildingId a == buildingReportBuildingId b


instance ToJSON CollatedBuildingReport where
  toJSON CollatedBuildingReport { cbrBuildingId = bId
                                , cbrPlanetId = pId
                                , cbrType = rType
                                , cbrLevel = rLevel
                                , cbrDamage = rDamage
                                , cbrDate = rDate
                                } =
    object [ "id" .= bId
           , "planetId" .= pId
           , "type" .= rType
           , "level" .= rLevel
           , "damage" .= rDamage
           , "date" .= rDate ]


spectralInfo :: Maybe SpectralType -> Maybe LuminosityClass -> Text
spectralInfo Nothing Nothing     = ""
spectralInfo (Just st) Nothing   = pack $ show st
spectralInfo Nothing (Just lc)   = pack $ show lc
spectralInfo (Just st) (Just lc) = pack $ show st ++ show lc


-- | Combine list of reports and form a single collated report
--   Resulting report will have facts from the possibly partially empty reports
--   If a fact is not present for a given field, Nothing is left there
collateReport :: (Monoid a, ReportTransform b a) => [b] -> a
collateReport reports = mconcat $ fmap fromReport reports


-- | Combine list of reports and form a list of collated reports
--   Each reported entity is given their own report
collateReports :: (Grouped b, Monoid a, ReportTransform b a) => [b] -> [a]
collateReports [] = []
collateReports s@(x:_) = collateReport itemsOfKind : collateReports restOfItems
    where split = span (sameGroup x) s
          itemsOfKind = fst split
          restOfItems = snd split


createStarReports :: (BaseBackend backend ~ SqlBackend,
    PersistQueryRead backend, MonadIO m) =>
    Key StarSystem -> Key Faction -> ReaderT backend m [CollatedStarReport]
createStarReports systemId factionId = do
    loadedStarReports <- selectList [ StarReportStarSystemId ==. systemId
                                    , StarReportFactionId ==. factionId ] [ Asc StarReportId
                                                                          , Asc StarReportDate ]
    return $ collateReports $ fmap entityVal loadedStarReports


createSystemReport :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistQueryRead backend) =>
    Key StarSystem -> Key Faction -> ReaderT backend m CollatedStarSystemReport
createSystemReport systemId factionId = do
    systemReports <- selectList [ StarSystemReportStarSystemId ==. systemId
                                , StarSystemReportFactionId ==. factionId ] [ Asc StarSystemReportDate ]
    return $ collateReport $ fmap entityVal systemReports


createPlanetReports :: (BaseBackend backend ~ SqlBackend,
    MonadIO m, PersistQueryRead backend) =>
    Key StarSystem -> Key Faction -> ReaderT backend m [CollatedPlanetReport]
createPlanetReports systemId factionId = do
    planets <- selectList [ PlanetStarSystemId ==. systemId ] []
    loadedPlanetReports <-  selectList [ PlanetReportPlanetId <-. fmap entityKey planets
                                       , PlanetReportFactionId ==. factionId ] [ Asc PlanetReportPlanetId
                                                                               , Asc PlanetReportDate ]
    return $ collateReports $ fmap entityVal loadedPlanetReports


createPlanetStatusReport :: ( BaseBackend backend ~ SqlBackend
                            , MonadIO m, PersistQueryRead backend) =>
                            (Route App -> Text) -> Key Planet -> Key Faction -> ReaderT backend m [CollatedPlanetStatusReport]
createPlanetStatusReport render planetId factionId = do
    statuses <- selectList [ PlanetStatusReportPlanetId ==. planetId
                           , PlanetStatusReportFactionId ==. factionId ]
                           [ Asc PlanetStatusReportDate ]
    let icons = planetStatusIconMapper render
    return $ collateReports $ fmap (\x -> (entityVal x, icons)) statuses


createStarLaneReports :: (BaseBackend backend ~ SqlBackend,
    MonadIO m, PersistQueryRead backend) =>
    Key StarSystem -> Key Faction -> ReaderT backend m [CollatedStarLaneReport]
createStarLaneReports systemId factionId = do
    loadedLaneReports <- selectList ([ StarLaneReportStarSystem1 ==. systemId
                                     , StarLaneReportFactionId ==. factionId ]
                                 ||. [ StarLaneReportStarSystem2 ==. systemId
                                     , StarLaneReportFactionId ==. factionId ]) []
    return $ rearrangeStarLanes systemId $ collateReports $ fmap entityVal loadedLaneReports


rearrangeStarLanes :: Key StarSystem -> [CollatedStarLaneReport] -> [CollatedStarLaneReport]
rearrangeStarLanes systemId = fmap arrangeStarLane
    where arrangeStarLane starLane = if systemId == cslSystemId1 starLane
                                        then starLane
                                        else CollatedStarLaneReport (toSqlKey 0)
                                                                    (cslSystemId2 starLane)
                                                                    (cslSystemId1 starLane)
                                                                    (cslStarSystemName2 starLane)
                                                                    (cslStarSystemName1 starLane)
                                                                    (cslDate starLane)

instance ToJSON CollatedStarSystemReport where
  toJSON CollatedStarSystemReport { cssrSystemId = rId
                                  , cssrName = rName
                                  , cssrLocation = rLocation
                                  , cssrDate = rDate } =
    object [ "id" .= rId
           , "name" .= rName
           , "location" .= rLocation
           , "date" .= rDate ]


planetStatusIconMapper :: (Route App -> Text) -> IconMapper PlanetaryStatus
planetStatusIconMapper render =
    IconMapper $ \icon ->
        case icon of
            GoodHarvest ->
                render $ StaticR images_statuses_wheat_up_png

            PoorHarvest ->
                render $ StaticR images_statuses_wheat_down_png

            GoodMechanicals ->
                render $ StaticR images_statuses_cog_up_png

            PoorMechanicals ->
                render $ StaticR images_statuses_cog_down_png

            GoodChemicals ->
                render $ StaticR images_statuses_droplets_up_png

            PoorChemicals ->
                render $ StaticR images_statuses_droplets_down_png

            KragiiAttack ->
                render $ StaticR images_statuses_hydra_png


statusDescription :: PlanetaryStatus -> Text
statusDescription status =
    case status of
        GoodHarvest -> "Harvest is plentiful"
        PoorHarvest -> "Harvest is poor"
        GoodMechanicals -> "Mech industry is booming"
        PoorMechanicals -> "Downturn in mech industry"
        GoodChemicals -> "Chem industry is booming"
        PoorChemicals -> "Downturn in chem industry"
        KragiiAttack -> "Kragii infestation in planet!"


$(deriveJSON defaultOptions { fieldLabelModifier = \x -> drop 19 x } ''PlanetaryStatusInfo)
$(deriveJSON defaultOptions { fieldLabelModifier = \x -> drop 26 x } ''CollatedPlanetStatusReport)
