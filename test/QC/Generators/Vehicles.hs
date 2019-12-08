module QC.Generators.Vehicles ( positiveCrewRequirements, positiveCrewRequirement
                              , anyBand, anyUnitLocation, anyShipLocation
                              , anyVehicleLocation, anyDesignName )
    where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()

import QC.Generators.Database ( randomPlanetKey, randomStarSystemKey )
import Units.Data ( CrewPosition(..), CrewRank(..), CrewAmount(..)
                  , CrewRequirement(..), Band(..), DesignName(..) )
import Units.Reports ( UnitLocation(..), ShipLocation(..)
                     , VehicleLocation(..) )


positiveCrewRequirement :: CrewPosition -> Gen CrewRequirement
positiveCrewRequirement position = do
    amount <- arbitrary `suchThat` \x -> x > 0
    return $ CrewRequirement position SecondClass (CrewAmount amount)


positiveCrewRequirements :: Gen [CrewRequirement]
positiveCrewRequirements = do
    let positions = [minBound..]
    mapM positiveCrewRequirement positions


anyBand :: Gen Band
anyBand = do
    elements [minBound..]


anyUnitLocation :: Gen UnitLocation
anyUnitLocation =
    oneof [ do
                loc <- anyShipLocation
                return $ ShipLocation loc
          , do
                loc <- anyVehicleLocation
                return $ VehicleLocation loc
          ]


anyShipLocation :: Gen ShipLocation
anyShipLocation = do
    band <- anyBand
    pId <- randomPlanetKey
    sId <- randomStarSystemKey
    oneof [ return $ PlanetarySpace pId band
          , return $ SystemSpace sId band
          ]


anyVehicleLocation :: Gen VehicleLocation
anyVehicleLocation = do
    pId <- randomPlanetKey
    return $ VehicleOnPlanet pId


anyDesignName :: Gen DesignName
anyDesignName = do
    name <- arbitrary
    return $ MkDesignName name
