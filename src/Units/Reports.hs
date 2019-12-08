{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}


module Units.Reports ( UnitReport(..), ShipReportDetails(..)
                     , VehicleReportDetails(..), UnitLocation(..)
                     , ShipLocation(..), VehicleLocation(..)
                     , UnitObservationDetails(..), ownerReport
                     , otherReport, deserializeObservation
                     , deserializeObservations
                     , ownerShipReport, ownerVehicleReport, otherShipReport
                     , otherVehicleReport, _ShipLocation, _VehicleLocation
                     , shipLocationL, vehicleLocationL, unitLocation )
    where

import Import
import Control.Lens ( Prism', Lens', prism, (^?), _Just, lens, (.~), (&), (^.) )
import Control.Lens.TH
import Data.Aeson ( decode, withObject )
import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import Data.ByteString.Builder( toLazyByteString )
import Data.Text.Encoding ( encodeUtf8Builder )
import People.Data ( PersonName(..) )
import Units.Data ( ShipName, VehicleName, Band, StatsReportDetails(..)
                  , UnitObservationDetailsJSON(..), Band(..), DesignName(..)
                  , CrewPosition(..), CrewRank(..) )
import Units.Lenses ( shipBandL, shipPlanetIdL, shipStarSystemIdL
                    , vehiclePlanetIdL )
import Units.Queries ( Unit'(..) )


data UnitReport
    = ShipReport ShipReportDetails
    | VehicleReport VehicleReportDetails
    deriving (Show, Read, Eq)


instance ToJSON UnitReport where
    toJSON obj =
        case obj of
            ShipReport details ->
                object [ "Tag" .= ("ShipReport" :: Text)
                       , "Contents" .= details
                       ]

            VehicleReport details ->
                object [ "Tag" .= ("VehicleReport" :: Text)
                       , "Contents" .= details
                       ]


data ShipReportDetails = ShipReportDetails
    { shipReportDetailsName :: !ShipName
    , shipReportDetailsDesignName :: !DesignName
    , shipReportDetailsStats :: !StatsReportDetails
    , shipReportDetailsLocation :: !(Maybe ShipLocation)
    , shipReportDetailsCrew :: ![CrewReport]
    }
    deriving (Show, Read, Eq)


data VehicleReportDetails = VehicleReportDetails
    { vehicleReportDetailsName :: !VehicleName
    , vehicleReportDetailsDesignName :: !DesignName
    , vehicleReportDetailsStats :: !StatsReportDetails
    , vehicleReportDetailsLocation :: !(Maybe VehicleLocation)
    , vehicleReportDetailsCrew :: ![CrewReport]
    } deriving (Show, Read, Eq)


data CrewReport = CrewReport
    { crewReportPersonId :: !PersonId
    , crewReportName :: !PersonName
    , crewReportPosition :: !CrewPosition
    , crewReportRank :: !CrewRank
    } deriving (Show, Read, Eq)


-- | Details of observed unit
-- | This is stored in database as UnitObservationDetailsJSON and
-- | then deserialized into UnitObservationDetails when constructing
-- | report on unit observations
data UnitObservationDetails = UnitObservationDetails
    { unitObservationDetailsLocation :: !(Maybe UnitLocation) }
    deriving (Show, Read, Eq)


instance Semigroup UnitObservationDetails where
    a <> b =
        UnitObservationDetails
            { unitObservationDetailsLocation = unitObservationDetailsLocation a <|> unitObservationDetailsLocation b }


instance Monoid UnitObservationDetails where
    mempty =
        UnitObservationDetails
            { unitObservationDetailsLocation = Nothing }


data UnitLocation
    = ShipLocation ShipLocation
    | VehicleLocation VehicleLocation
    deriving (Show, Read, Eq)


instance ToJSON UnitLocation where
    toJSON loc =
        case loc of
            ShipLocation l ->
                object [ "Tag" .= ("ShipLocation" :: Text)
                       , "Contents" .= l
                       ]

            VehicleLocation l ->
                object [ "Tag" .= ("VehicleLocation" :: Text)
                       , "Contents" .= l
                       ]


instance FromJSON UnitLocation where
    parseJSON = withObject "unit location" $ \o -> do
        tag <- o .: "Tag"
        asum [ do
                guard (tag == ("ShipLocation" :: Text))
                c <- o .: "Contents"
                return $ ShipLocation c
             , do
                guard (tag == ("VehicleLocation" :: Text))
                c <- o .: "Contents"
                return $ VehicleLocation c
             ]


_ShipLocation :: Prism' UnitLocation ShipLocation
_ShipLocation =
    prism ShipLocation $ \x -> case x of
        ShipLocation a ->
            Right a

        _ ->
            Left x


_VehicleLocation :: Prism' UnitLocation VehicleLocation
_VehicleLocation =
    prism VehicleLocation $ \x -> case x of
        VehicleLocation a ->
            Right a

        _ ->
            Left x


-- | Location of an unit
unitLocation :: Unit' -> Maybe UnitLocation
unitLocation unit =
    case unit of
        Ship' ship ->
            ShipLocation <$> ship ^. shipLocationL

        Vehicle' vehicle ->
            VehicleLocation <$> vehicle ^. vehicleLocationL


data ShipLocation
    = PlanetarySpace PlanetId Band
    | SystemSpace StarSystemId Band
    deriving (Show, Read, Eq)


instance ToJSON ShipLocation where
    toJSON loc =
        case loc of
            PlanetarySpace pId band ->
                object [ "Tag" .= ("PlanetarySpace" :: Text)
                       , "PlanetId" .= pId
                       , "Band" .= band
                       ]

            SystemSpace sId band ->
                object [ "Tag" .= ("SystemSpace" :: Text)
                       , "SystemId" .= sId
                       , "Band" .= band
                       ]


instance FromJSON ShipLocation where
    parseJSON = withObject "ship location" $ \o -> do
        tag <- o .: "Tag"
        asum [ do
                guard (tag == ("PlanetarySpace" :: Text))
                pId <- o .: "PlanetId"
                band <- o .: "Band"
                return $ PlanetarySpace pId band
             , do
                guard (tag == ("SystemSpace" :: Text))
                sId <- o .: "SystemId"
                band <- o .: "Band"
                return $ SystemSpace sId band
             ]


-- | Location of a ship
shipLocationL :: Lens' Ship (Maybe ShipLocation)
shipLocationL =
    lens (\s ->
            case (shipStarSystemId s, (shipPlanetId s, shipBand s)) of
                (Nothing, (Just pId, b)) ->
                    Just $ PlanetarySpace pId b

                (Just sId, (Nothing, b)) ->
                    Just $ SystemSpace sId b

                _ ->
                    Nothing
            )
         (\s l -> case l of
                    Just (PlanetarySpace pId b) ->
                        s & shipStarSystemIdL .~ Nothing
                          & shipPlanetIdL .~ (Just pId)
                          & shipBandL .~ b

                    Just (SystemSpace sId b) ->
                        s & shipStarSystemIdL .~ (Just sId)
                          & shipPlanetIdL .~ Nothing
                          & shipBandL .~ b

                    Nothing ->
                        s & shipStarSystemIdL .~ Nothing
                          & shipPlanetIdL .~ Nothing
            )


data VehicleLocation
    = VehicleOnPlanet PlanetId
    deriving (Show, Read, Eq)


instance ToJSON VehicleLocation where
    toJSON loc =
        case loc of
            VehicleOnPlanet pId ->
                object [ "Tag" .= ("VehicleOnPlanet" :: Text)
                       , "PlanetId" .= pId
                       ]


instance FromJSON VehicleLocation where
    parseJSON = withObject "vehicle location" $ \o -> do
        tag <- o .: "Tag"
        asum [ do
                guard (tag == ("VehicleOnPlanet" :: Text))
                pId <- o .: "PlanetId"
                return $ VehicleOnPlanet pId
             ]


-- | Location of a vehicle
vehicleLocationL :: Lens' Vehicle (Maybe VehicleLocation)
vehicleLocationL =
    lens (\v -> case vehiclePlanetId v of
                    Just pId ->
                        Just $ VehicleOnPlanet pId

                    Nothing ->
                        Nothing)

         (\v l -> case l of
                    Just (VehicleOnPlanet pId) ->
                        v & vehiclePlanetIdL .~ Just pId

                    Nothing ->
                        v & vehiclePlanetIdL .~ Nothing)


-- lenses are created here because of how template haskell parses the file

makeLensesFor [ ("unitObservationDetailsLocation", "unitObservationDetailsLocationL")
              ] ''UnitObservationDetails


-- | Report of unit based on data available to owner
ownerReport ::
    Unit'
    -> [StatsReportDetails]
    -> Design
    -> [(CrewAssignment, Person)]
    -> UnitReport
ownerReport unit statReports design crew =
    case unit of
        Ship' ship ->
            ShipReport $ ownerShipReport ship statReports design crew

        Vehicle' vehicle ->
            VehicleReport $ ownerVehicleReport vehicle statReports design crew


ownerShipReport ::
    Ship
    -> [StatsReportDetails]
    -> Design
    -> [(CrewAssignment, Person)]
    -> ShipReportDetails
ownerShipReport ship statReports design crew =
    ShipReportDetails
        { shipReportDetailsName = shipName ship
        , shipReportDetailsDesignName = designName design
        , shipReportDetailsStats = mconcat statReports
        , shipReportDetailsLocation = ship ^. shipLocationL
        , shipReportDetailsCrew = assignmentToReport <$> crew
        }


ownerVehicleReport ::
    Vehicle
    -> [StatsReportDetails]
    -> Design
    -> [(CrewAssignment, Person)]
    -> VehicleReportDetails
ownerVehicleReport vehicle statReports design crew =
    VehicleReportDetails
        { vehicleReportDetailsName = vehicleName vehicle
        , vehicleReportDetailsDesignName = designName design
        , vehicleReportDetailsStats = mconcat statReports
        , vehicleReportDetailsLocation = vehicle ^. vehicleLocationL
        , vehicleReportDetailsCrew = assignmentToReport <$> crew
        }


assignmentToReport :: (CrewAssignment, Person) -> CrewReport
assignmentToReport (assignment, person) =
    CrewReport
    { crewReportPersonId = crewAssignmentPersonId assignment
    , crewReportName = personName person
    , crewReportPosition = crewAssignmentPosition assignment
    , crewReportRank = crewAssignmentRank assignment
    }



-- | Report of unit based on gathered data
otherReport :: Unit' -> [StatsReportDetails] -> [UnitObservationDetails] -> Design -> UnitReport
otherReport unit statReports observations design =
    case unit of
        Ship' ship ->
            ShipReport $ otherShipReport ship statReports observations design

        Vehicle' vehicle ->
            VehicleReport $ otherVehicleReport vehicle statReports observations design


otherShipReport :: Ship -> [StatsReportDetails] -> [UnitObservationDetails] -> Design -> ShipReportDetails
otherShipReport ship statReports observations design =
    ShipReportDetails
        { shipReportDetailsName = shipName ship
        , shipReportDetailsDesignName = designName design
        , shipReportDetailsStats = mconcat statReports
        , shipReportDetailsLocation = obs ^? unitObservationDetailsLocationL . _Just . _ShipLocation
        , shipReportDetailsCrew = []
        }
    where
        obs = mconcat observations


otherVehicleReport :: Vehicle -> [StatsReportDetails] -> [UnitObservationDetails] -> Design -> VehicleReportDetails
otherVehicleReport vehicle statReports observations design =
    VehicleReportDetails
        { vehicleReportDetailsName = vehicleName vehicle
        , vehicleReportDetailsDesignName = designName design
        , vehicleReportDetailsStats = mconcat statReports
        , vehicleReportDetailsLocation = obs ^? unitObservationDetailsLocationL . _Just . _VehicleLocation
        , vehicleReportDetailsCrew = []
        }
    where
        obs = mconcat observations


-- | Map UnitObservationDetailsJSON to Maybe UnitObservationDetails
-- | When deserializing JSON fails, Nothing is returned
deserializeObservation :: UnitObservationDetailsJSON -> Maybe UnitObservationDetails
deserializeObservation =
    decode . toLazyByteString . encodeUtf8Builder . unUnitObservationDetailsJSON


-- | Map list of UnitObservationDetailsJSON to list of UnitObservationDetails
-- | In cases where deserialization of JSON fails, value is discarded and
-- | only succesfull cases are returned.
deserializeObservations :: [UnitObservationDetailsJSON] -> [UnitObservationDetails]
deserializeObservations entries =
    catMaybes $ deserializeObservation <$> entries


$(deriveJSON defaultOptions { fieldLabelModifier = drop 17 } ''ShipReportDetails)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 20 } ''VehicleReportDetails)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 22 } ''UnitObservationDetails)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''CrewReport)
