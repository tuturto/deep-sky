{-# LANGUAGE OverloadedStrings          #-}

module Report where

import CustomTypes
import Model
import Data.Time.Calendar (Day(..))
import Data.Text (Text)
import Database.Persist.Sql (toSqlKey)
import Data.List (sortBy)

data CollatedSolarSystemReport = CollatedSolarSystemReport {
      cssrSystemId :: Key SolarSystem
    , cssrName :: Maybe Text
    , cssrLocation :: Coordinates
    , cssrDate :: Day
} deriving Show

data CollatedStarReport = CollatedStarReport {
      csrName :: Maybe Text
    , csrSpectralType :: Maybe SpectralType
    , csrLuminosityClass :: Maybe LuminosityClass
    , csrDate :: Day
} deriving Show

data CollatedPlanetReport = CollatedPlanetReport {
      cprName :: Maybe Text
    , cprPosition :: Maybe Int
    , cprGravity :: Maybe Double
    , cprDate :: Day
} deriving Show

combine :: Maybe a -> Maybe a -> Maybe a
combine (Just _) b@(Just _) = b
combine a@(Just _) Nothing  = a
combine Nothing b@(Just _)  = b
combine Nothing Nothing     = Nothing

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
    where initial = CollatedStarReport Nothing Nothing Nothing $ ModifiedJulianDay 1
          fn val acc = CollatedStarReport (combine (starReportName val) (csrName acc))
                                          (combine (starReportSpectralType val) (csrSpectralType acc))
                                          (combine (starReportLuminosityClass val) (csrLuminosityClass acc))
                                          (max (starReportDate val) (csrDate acc))

collatePlanet :: [PlanetReport] -> CollatedPlanetReport
collatePlanet planets = foldr fn initial planets
    where initial = CollatedPlanetReport Nothing Nothing Nothing $ ModifiedJulianDay 1
          fn val acc = CollatedPlanetReport (combine (planetReportName val) (cprName acc))
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
