{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Report where

import Import
import CustomTypes
import Database.Persist.Sql (toSqlKey)
import Data.Aeson.TH

data CollatedStarSystemReport = CollatedStarSystemReport {
      cssrSystemId :: Key StarSystem
    , cssrName     :: Maybe Text
    , cssrLocation :: Coordinates
    , cssrDate     :: Int
} deriving Show

data CollatedStarReport = CollatedStarReport {
      csrStarId          :: Key Star
    , csrSystemId        :: Key StarSystem
    , csrName            :: Maybe Text    
    , csrSpectralType    :: Maybe SpectralType
    , csrLuminosityClass :: Maybe LuminosityClass
    , csrDate            :: Int
} deriving Show

data CollatedPlanetReport = CollatedPlanetReport 
    { cprPlanetId :: Key Planet
    , cprSystemId :: Key StarSystem
    , cprOwnerId  :: Maybe (Key Faction)
    , cprName     :: Maybe Text
    , cprPosition :: Maybe Int
    , cprGravity  :: Maybe Double
    , cprDate     :: Int
    } deriving Show

data CollatedPopulationReport = CollatedPopulationReport
    { cpopPlanetId   :: Key Planet
    , cpopRaceId     :: Maybe (Key Race)
    , cpopRace       :: Maybe Text
    , cpopPopulation :: Maybe Int
    , cpopDate       :: Int
    } deriving Show

data CollatedStarLaneReport = CollatedStarLaneReport {
      cslStarLaneId      :: Key StarLane
    , cslSystemId1       :: Key StarSystem
    , cslSystemId2       :: Key StarSystem
    , cslStarSystemName1 :: Maybe Text
    , cslStarSystemName2 :: Maybe Text
    , cslDate            :: Int
} deriving Show

data CollatedBaseReport = CollatedBaseReport {
      cbsPlanetReport   :: CollatedPlanetReport
    , cbsStarSystemName :: Text
} deriving Show

data CollatedBuildingReport = CollatedBuildingReport {
      cbrBuildingId   :: Key Building
    , cbrPlanetId     :: Key Planet
    , cbrType :: Maybe BuildingType
    , cbrLevel        :: Maybe Int
    , cbrConstruction :: Maybe Double
    , cbrDamage       :: Maybe Double
    , cbrDate         :: Int
} deriving Show

combine :: Maybe a -> Maybe a -> Maybe a
combine (Just _) b@(Just _) = b
combine a@(Just _) Nothing  = a
combine Nothing b@(Just _)  = b
combine Nothing Nothing     = Nothing

spectralInfo :: Maybe SpectralType -> Maybe LuminosityClass -> Text
spectralInfo Nothing Nothing     = ""
spectralInfo (Just st) Nothing   = pack $ show st
spectralInfo Nothing (Just lc)   = pack $ show lc
spectralInfo (Just st) (Just lc) = pack $ show st ++ (show lc)

collateSystem :: [StarSystemReport] -> CollatedStarSystemReport
collateSystem = foldr fn initial
    where initial = CollatedStarSystemReport (toSqlKey 0) Nothing (Coordinates 0 0) 0
          fn val acc = CollatedStarSystemReport (starSystemReportStarSystemId val)
                                                (combine (starSystemReportName val) (cssrName acc))
                                                (Coordinates (starSystemReportCoordX val) (starSystemReportCoordY val))
                                                (max (starSystemReportDate val) (cssrDate acc))

collateSystems :: [StarSystemReport] -> [CollatedStarSystemReport]
collateSystems [] = []
collateSystems s@(x:_) = (collateSystem itemsOfKind) : (collateSystems restOfItems)
    where split = span comparer s
          comparer = \a -> (starSystemReportStarSystemId a) == (starSystemReportStarSystemId x)
          itemsOfKind = fst split
          restOfItems = snd split

collateStar :: [StarReport] -> CollatedStarReport
collateStar = foldr fn initial
    where initial = CollatedStarReport (toSqlKey 0) (toSqlKey 0) Nothing Nothing Nothing 0
          fn val acc = CollatedStarReport (starReportStarId val)
                                          (starReportStarSystemId val)
                                          (combine (starReportName val) (csrName acc))
                                          (combine (starReportSpectralType val) (csrSpectralType acc))
                                          (combine (starReportLuminosityClass val) (csrLuminosityClass acc))
                                          (max (starReportDate val) (csrDate acc))

collateStars :: [StarReport] -> [CollatedStarReport]
collateStars [] = []
collateStars s@(x:_) = (collateStar itemsOfKind) : (collateStars restOfItems)
    where split = span comparer s
          comparer = \a -> (starReportStarId a) == (starReportStarId x)
          itemsOfKind = fst split
          restOfItems = snd split

collatePlanet :: [PlanetReport] -> CollatedPlanetReport
collatePlanet = foldr fn initial
    where initial = CollatedPlanetReport (toSqlKey 0) (toSqlKey 0) Nothing Nothing Nothing Nothing 0
          fn val acc = CollatedPlanetReport (planetReportPlanetId val)
                                            (planetReportStarSystemId val)
                                            (planetReportOwnerId val)
                                            (combine (planetReportName val) (cprName acc))
                                            (combine (planetReportPosition val) (cprPosition acc))
                                            (combine (planetReportGravity val) (cprGravity acc))
                                            (max (planetReportDate val) (cprDate acc))

collatePlanets :: [PlanetReport] -> [CollatedPlanetReport]
collatePlanets [] = []
collatePlanets s@(x:_) = (collatePlanet itemsOfKind) : (collatePlanets restOfItems)
    where split = span comparer s
          comparer = \a -> (planetReportPlanetId a) == (planetReportPlanetId x)
          itemsOfKind = fst split
          restOfItems = snd split

collatePopulation :: [PlanetPopulationReport] -> CollatedPopulationReport
collatePopulation = foldr fn initial
    where initial = CollatedPopulationReport (toSqlKey 0) Nothing Nothing Nothing 0
          fn val acc = CollatedPopulationReport (planetPopulationReportPlanetId val)
                                                (combine (planetPopulationReportRaceId val) (cpopRaceId acc))
                                                (cpopRace acc)
                                                (combine (planetPopulationReportPopulation val) (cpopPopulation acc))
                                                (max (planetPopulationReportDate val) (cpopDate acc))

collatePopulations :: [PlanetPopulationReport] -> [CollatedPopulationReport]
collatePopulations [] = []
collatePopulations s@(x:_) = (collatePopulation itemsOfKind) : (collatePopulations restOfItems)
    where split = span comparer s
          comparer a = (planetPopulationReportPlanetId a) == (planetPopulationReportPlanetId x) &&
                       (planetPopulationReportFactionId a) == (planetPopulationReportFactionId x)
          itemsOfKind = fst split
          restOfItems = snd split

collateStarLane :: [StarLaneReport] -> CollatedStarLaneReport
collateStarLane = foldr fn initial
    where initial = CollatedStarLaneReport (toSqlKey 0) (toSqlKey 0) (toSqlKey 0) Nothing Nothing 0
          fn val acc = CollatedStarLaneReport (starLaneReportStarLaneId val)
                                              (starLaneReportStarSystem1 val)
                                              (starLaneReportStarSystem2 val)
                                              (combine (starLaneReportStarSystemName1 val) (cslStarSystemName1 acc))
                                              (combine (starLaneReportStarSystemName2 val) (cslStarSystemName2 acc))
                                              (max (starLaneReportDate val) (cslDate acc))

collateStarLanes :: [StarLaneReport] -> [CollatedStarLaneReport]
collateStarLanes [] = []
collateStarLanes s@(x:_) = (collateStarLane itemsOfKind) : (collateStarLanes restOfItems)
    where split = span comparer s
          comparer = \a -> (starLaneReportStarSystem1 a) == (starLaneReportStarSystem1 x) &&
                           (starLaneReportStarSystem2 a) == (starLaneReportStarSystem2 x)
          itemsOfKind = fst split
          restOfItems = snd split

collateBuilding :: [BuildingReport] -> CollatedBuildingReport
collateBuilding = foldr fn initial
    where initial = CollatedBuildingReport (toSqlKey 0) (toSqlKey 0) Nothing Nothing Nothing Nothing 0
          fn val acc = CollatedBuildingReport (buildingReportBuildingId val)
                                              (buildingReportPlanetId val)
                                              (combine (buildingReportType val) (cbrType acc))
                                              (combine (buildingReportLevel val) (cbrLevel acc))
                                              (combine (buildingReportConstruction val) (cbrConstruction acc))
                                              (combine (buildingReportDamage val) (cbrDamage acc))
                                              (max (buildingReportDate val) (cbrDate acc))

collateBuildings :: [BuildingReport] -> [CollatedBuildingReport]
collateBuildings [] = []
collateBuildings s@(x:_) = (collateBuilding itemsOfKind) : (collateBuildings restOfItems)
    where split = span comparer s
          comparer = \a -> (buildingReportBuildingId a) == (buildingReportBuildingId x)
          itemsOfKind = fst split
          restOfItems = snd split

createStarReports :: (BaseBackend backend ~ SqlBackend,
    PersistQueryRead backend, MonadIO m) =>
    Key StarSystem -> Key Faction -> ReaderT backend m [CollatedStarReport]
createStarReports systemId factionId = do
    loadedStarReports <- selectList [ StarReportStarSystemId ==. systemId
                                    , StarReportFactionId ==. factionId ] [ Asc StarReportId
                                                                          , Asc StarReportDate ]
    return $ collateStars $ map entityVal loadedStarReports

createSystemReport :: (BaseBackend backend ~ SqlBackend, MonadIO m,
    PersistQueryRead backend) =>
    Key StarSystem -> Key Faction -> ReaderT backend m CollatedStarSystemReport
createSystemReport systemId factionId = do
    systemReports <- selectList [ StarSystemReportStarSystemId ==. systemId
                                , StarSystemReportFactionId ==. factionId ] [ Asc StarSystemReportDate ]
    return $ collateSystem $ map entityVal systemReports

createPlanetReports :: (BaseBackend backend ~ SqlBackend,
    MonadIO m, PersistQueryRead backend) =>
    Key StarSystem -> Key Faction -> ReaderT backend m [CollatedPlanetReport]
createPlanetReports systemId factionId = do
    planets <- selectList [ PlanetStarSystemId ==. systemId ] []
    loadedPlanetReports <-  selectList [ PlanetReportPlanetId <-. (map entityKey planets) 
                                       , PlanetReportFactionId ==. factionId ] [ Asc PlanetReportPlanetId
                                                                               , Asc PlanetReportDate ]
    return $ collatePlanets $ map entityVal loadedPlanetReports

createStarLaneReports :: (BaseBackend backend ~ SqlBackend,
    MonadIO m, PersistQueryRead backend) =>
    Key StarSystem -> Key Faction -> ReaderT backend m [CollatedStarLaneReport]
createStarLaneReports systemId factionId = do
    loadedLaneReports <- selectList ([ StarLaneReportStarSystem1 ==. systemId
                                     , StarLaneReportFactionId ==. factionId ]
                                 ||. [ StarLaneReportStarSystem2 ==. systemId 
                                     , StarLaneReportFactionId ==. factionId ]) []
    return $ rearrangeStarLanes systemId $ collateStarLanes $ map entityVal loadedLaneReports

rearrangeStarLanes :: Key StarSystem -> [CollatedStarLaneReport] -> [CollatedStarLaneReport]
rearrangeStarLanes systemId = map arrangeStarLane
    where arrangeStarLane starLane = if systemId == (cslSystemId1 starLane)
                                        then starLane
                                        else CollatedStarLaneReport (toSqlKey 0)
                                                                    (cslSystemId2 starLane)
                                                                    (cslSystemId1 starLane)
                                                                    (cslStarSystemName2 starLane)
                                                                    (cslStarSystemName1 starLane)
                                                                    (cslDate starLane)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''CollatedStarSystemReport)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''CollatedPlanetReport)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''CollatedBaseReport)
