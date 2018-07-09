{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}

module Report where

import Import (Handler, entityVal, entityKey, SelectOpt(..), (==.), (<-.), (||.), selectList, runDB)
import CustomTypes
import Model
import Data.Text (Text, pack)
import Database.Persist.Sql (toSqlKey)
import Data.Aeson.TH

data CollatedStarSystemReport = CollatedStarSystemReport {
      cssrSystemId :: Key StarSystem
    , cssrName     :: Maybe Text
    , cssrLocation :: Coordinates
    , cssrDate     :: Double
} deriving Show

data CollatedStarReport = CollatedStarReport {
      csrSystemId        :: Key StarSystem
    , csrName            :: Maybe Text    
    , csrSpectralType    :: Maybe SpectralType
    , csrLuminosityClass :: Maybe LuminosityClass
    , csrDate            :: Double
} deriving Show

data CollatedPlanetReport = CollatedPlanetReport {
      cprPlanetId :: Key Planet
    , cprSystemId :: Key StarSystem
    , cprOwnerId  :: Maybe (Key User)
    , cprName     :: Maybe Text
    , cprPosition :: Maybe Int
    , cprGravity  :: Maybe Double
    , cprDate     :: Double
} deriving Show

data CollatedStarLaneReport = CollatedStarLaneReport {
      cslSystemId1       :: Key StarSystem
    , cslSystemId2       :: Key StarSystem
    , cslStarSystemName1 :: Maybe Text
    , cslStarSystemName2 :: Maybe Text
    , cslDate            :: Double
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
    , cbrDate         :: Double
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
    where initial = CollatedStarReport (toSqlKey 0) Nothing Nothing Nothing 0
          fn val acc = CollatedStarReport (starReportStarSystemId val)
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

collateStarLane :: [StarLaneReport] -> CollatedStarLaneReport
collateStarLane = foldr fn initial
    where initial = CollatedStarLaneReport (toSqlKey 0) (toSqlKey 0) Nothing Nothing 0
          fn val acc = CollatedStarLaneReport (starLaneReportStarSystem1 val)
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

createStarReports :: Key StarSystem -> Key Faction -> Handler [CollatedStarReport]
createStarReports systemId factionId = do
    loadedStarReports <- runDB $ selectList [ StarReportStarSystemId ==. systemId
                                            , StarReportFactionId ==. factionId ] [ Asc StarReportId
                                                                                  , Asc StarReportDate ]
    return $ collateStars $ map entityVal loadedStarReports

createSystemReport :: Key StarSystem -> Key Faction -> Handler CollatedStarSystemReport
createSystemReport systemId factionId = do
    systemReports <- runDB $ selectList [ StarSystemReportStarSystemId ==. systemId
                                        , StarSystemReportFactionId ==. factionId ] [ Asc StarSystemReportDate ]
    return $ collateSystem $ map entityVal systemReports

createPlanetReports :: Key StarSystem -> Key Faction -> Handler [CollatedPlanetReport]
createPlanetReports systemId factionId = do
    planets <- runDB $ selectList [ PlanetStarSystemId ==. systemId ] []
    loadedPlanetReports <- runDB $ selectList [ PlanetReportPlanetId <-. (map entityKey planets) 
                                              , PlanetReportFactionId ==. factionId ] [ Asc PlanetReportPlanetId
                                                                                      , Asc PlanetReportDate ]
    return $ collatePlanets $ map entityVal loadedPlanetReports

createStarLaneReports :: Key StarSystem -> Key Faction -> Handler [CollatedStarLaneReport]
createStarLaneReports systemId factionId = do
    loadedLaneReports <- runDB $ selectList ([ StarLaneReportStarSystem1 ==. systemId
                                             , StarLaneReportFactionId ==. factionId ]
                                         ||. [ StarLaneReportStarSystem2 ==. systemId 
                                             , StarLaneReportFactionId ==. factionId ]) []
    return $ rearrangeStarLanes systemId $ collateStarLanes $ map entityVal loadedLaneReports

rearrangeStarLanes :: Key StarSystem -> [CollatedStarLaneReport] -> [CollatedStarLaneReport]
rearrangeStarLanes systemId = map arrangeStarLane
    where arrangeStarLane starLane = if systemId == (cslSystemId1 starLane)
                                        then starLane
                                        else CollatedStarLaneReport (cslSystemId2 starLane)
                                                                    (cslSystemId1 starLane)
                                                                    (cslStarSystemName2 starLane)
                                                                    (cslStarSystemName1 starLane)
                                                                    (cslDate starLane)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''CollatedStarSystemReport)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''CollatedPlanetReport)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''CollatedBaseReport)
