{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}


module Vehicles.Queries ( Unit'(..), getUnit )
    where

import Import
import qualified Database.Esqueleto as E

-- | Domain model for Units
data Unit' =
    Ship' Ship
    | Vehicle' Vehicle
    deriving (Show, Read, Eq)


-- | Try to load unit from database
getUnit :: (MonadIO m, BackendCompatible SqlBackend backend,
    PersistQueryRead backend, PersistUniqueRead backend) =>
    Key Unit -> ReaderT backend m (Maybe Unit')
getUnit uId = do
    res <- E.select $
                E.from $ \(unit `E.LeftOuterJoin` vehicle `E.LeftOuterJoin` ship) -> do
                    E.on (ship E.?. ShipId E.==. E.just (unit E.^. UnitShipId))
                    E.on (vehicle E.?. VehicleId E.==. E.just (unit E.^. UnitVehicleId))
                    E.where_ (unit E.^. UnitId E.==. E.val uId)
                    return (unit, vehicle, ship)
    return $ unitResToUnit res


-- | Transform result of sql-join into domain data type
-- This function is partial and will thrown an error if the results of
-- sql-join are not consistent. There should be exactly zero or one entries in
-- list, with exactly one Just for sub-unit.
unitResToUnit :: [(Entity Unit, Maybe (Entity Vehicle), Maybe (Entity Ship))] -> Maybe Unit'
unitResToUnit [] =
    Nothing

unitResToUnit ((_, Just vehicle, Nothing):[]) =
    Just $ Vehicle' $ entityVal vehicle

unitResToUnit ((_, Nothing, Just ship):[]) =
    Just $ Ship' $ entityVal ship

unitResToUnit ((_, _, _):[]) =
    error "Mismatched sub units"

unitResToUnit (_:_:_) =
    error "More than one sub unit in database with given key"
