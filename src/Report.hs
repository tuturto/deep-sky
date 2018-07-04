{-# LANGUAGE OverloadedStrings          #-}

module Report where

import CustomTypes
import Model
import Data.Time.Calendar (Day(..))
import Data.Text (Text, pack)
import Database.Persist.Sql (toSqlKey)
import Data.List (sortBy)

data CollatedSolarSystemReport = CollatedSolarSystemReport {
      cssrSystemId :: Key SolarSystem
    , cssrName     :: Maybe Text
    , cssrLocation :: Coordinates
    , cssrDate     :: Day
} deriving Show

data CollatedStarReport = CollatedStarReport {
      csrSystemId        :: Key SolarSystem
    , csrName            :: Maybe Text    
    , csrSpectralType    :: Maybe SpectralType
    , csrLuminosityClass :: Maybe LuminosityClass
    , csrDate            :: Day
} deriving Show

data CollatedPlanetReport = CollatedPlanetReport {
      cprPlanetId :: Key Planet
    , cprSystemId :: Key SolarSystem
    , cprName     :: Maybe Text
    , cprPosition :: Maybe Int
    , cprGravity  :: Maybe Double
    , cprDate     :: Day
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

collateSystem :: [SolarSystemReport] -> CollatedSolarSystemReport
collateSystem systems = foldr fn initial systems
    where initial = CollatedSolarSystemReport (toSqlKey 0) Nothing (Coordinates 0 0) $ ModifiedJulianDay 1
          fn val acc = CollatedSolarSystemReport (solarSystemReportSystemId val)
                                                 (combine (solarSystemReportName val) (cssrName acc))
                                                 (Coordinates (solarSystemReportCoordY val) (solarSystemReportCoordX val))
                                                 (max (solarSystemReportDate val) (cssrDate acc))

collateSystems :: [SolarSystemReport] -> [CollatedSolarSystemReport]
collateSystems [] = []
collateSystems s@(x:xs) = (collateSystem itemsOfKind) : (collateSystems restOfItems)
    where split = span comparer s
          comparer = \a -> (solarSystemReportSystemId a) == (solarSystemReportSystemId x)
          itemsOfKind = fst split
          restOfItems = snd split

collateStar :: [StarReport] -> CollatedStarReport
collateStar stars = foldr fn initial stars
    where initial = CollatedStarReport (toSqlKey 0) Nothing Nothing Nothing $ ModifiedJulianDay 1
          fn val acc = CollatedStarReport (starReportSystemId val)
                                          (combine (starReportName val) (csrName acc))
                                          (combine (starReportSpectralType val) (csrSpectralType acc))
                                          (combine (starReportLuminosityClass val) (csrLuminosityClass acc))
                                          (max (starReportDate val) (csrDate acc))

collateStars :: [StarReport] -> [CollatedStarReport]
collateStars [] = []
collateStars s@(x:xs) = (collateStar itemsOfKind) : (collateStars restOfItems)
    where split = span comparer s
          comparer = \a -> (starReportSystemId a) == (starReportSystemId x)
          itemsOfKind = fst split
          restOfItems = snd split

collatePlanet :: [PlanetReport] -> CollatedPlanetReport
collatePlanet planets = foldr fn initial planets
    where initial = CollatedPlanetReport (toSqlKey 0) (toSqlKey 0) Nothing Nothing Nothing $ ModifiedJulianDay 1
          fn val acc = CollatedPlanetReport (planetReportPlanetId val)
                                            (planetReportSystemId val)
                                            (combine (planetReportName val) (cprName acc))
                                            (combine (planetReportPosition val) (cprPosition acc))
                                            (combine (planetReportGravity val) (cprGravity acc))
                                            (max (planetReportDate val) (cprDate acc))

collatePlanets :: [PlanetReport] -> [CollatedPlanetReport]
collatePlanets [] = []
collatePlanets s@(x:xs) = (collatePlanet itemsOfKind) : (collatePlanets restOfItems)
    where split = span comparer s
          comparer = \a -> (planetReportPlanetId a) == (planetReportPlanetId x)
          itemsOfKind = fst split
          restOfItems = snd split
