{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Units.Lenses ( shipNameL, shipDesignIdL, shipChassisIdL
                    , shipStarSystemIdL, shipPlanetIdL, shipBandL
                    , shipOwnerIdL, vehicleNameL, vehicleDesignIdL
                    , vehicleChassisIdL, vehiclePlanetIdL, vehicleOwnerIdL
                    , unitNameL, _Ship, _Vehicle )
    where

import Import
import Control.Lens ( Lens', Prism', lens, prism, (.~), (&) )
import Control.Lens.TH
import Units.Data ( UnitName(..), VehicleName(..), ShipName(..) )
import Units.Queries ( Unit'(..) )


makeLensesFor [ ("shipName", "shipNameL")
              , ("shipDesignId", "shipDesignIdL")
              , ("shipChassisId", "shipChassisIdL")
              , ("shipStarSystemId", "shipStarSystemIdL")
              , ("shipPlanetId", "shipPlanetIdL")
              , ("shipBand", "shipBandL")
              , ("shipOwnerId", "shipOwnerIdL")
              ] ''Ship

makeLensesFor [ ("vehicleName", "vehicleNameL")
              , ("vehicleDesignId", "vehicleDesignIdL")
              , ("vehicleChassisId", "vehicleChassisIdL")
              , ("vehiclePlanetId", "vehiclePlanetIdL")
              , ("vehicleOwnerId", "vehicleOwnerIdL")
              ] ''Vehicle


_Ship :: Prism' Unit' Ship
_Ship =
    prism Ship' $ \x -> case x of
        Ship' a ->
            Right a

        _ ->
            Left x


_Vehicle :: Prism' Unit' Vehicle
_Vehicle =
    prism Vehicle' $ \x -> case x of
        Vehicle' a ->
            Right a

        _ ->
            Left x


unitNameL :: Lens' Unit' UnitName
unitNameL = lens (\unit ->
                    case unit of
                        Vehicle' vehicle ->
                            MkUnitName $ unVehicleName $ vehicleName vehicle

                        Ship' ship ->
                            MkUnitName $ unShipName $ shipName ship)

                 (\unit name ->
                     case unit of
                        Ship' _ ->
                            unit & (_Ship . shipNameL) .~ (MkShipName $ unUnitName name)

                        Vehicle' _ ->
                            unit & (_Vehicle . vehicleNameL) .~ (MkVehicleName $ unUnitName name))
