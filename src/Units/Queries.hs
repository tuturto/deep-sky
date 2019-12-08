{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}


module Units.Queries ( Unit'(..), getUnit, unitOwnerId, unitDesignId
                     , getFactionUnits, getUnitCrew )
    where

import Import
import qualified Database.Esqueleto as E

-- | Domain model for Units
data Unit' =
    Ship' Ship
    | Vehicle' Vehicle
    deriving (Show, Read, Eq)


-- | Owner of the unit
unitOwnerId :: Unit' -> PersonId
unitOwnerId unit =
    case unit of
        Ship' ship ->
            shipOwnerId ship

        Vehicle' vehicle ->
            vehicleOwnerId vehicle


unitDesignId :: Unit' -> DesignId
unitDesignId unit =
    case unit of
        Ship' ship ->
            shipDesignId ship

        Vehicle' vehicle ->
            vehicleDesignId vehicle


-- | Try to load unit from database
getUnit :: (MonadIO m, BackendCompatible SqlBackend backend,
    PersistQueryRead backend, PersistUniqueRead backend) =>
    UnitId -> ReaderT backend m (Maybe Unit')
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


-- | Load all units that are owned by people belonging to a given faction
getFactionUnits :: (MonadIO m, BackendCompatible SqlBackend backend,
    PersistQueryRead backend, PersistUniqueRead backend) =>
    FactionId -> ReaderT backend m [(UnitId, Unit')]
getFactionUnits fId = do
    res <- E.select $
                E.from $ \(unit `E.LeftOuterJoin` vehicle
                            `E.LeftOuterJoin` ship
                            `E.LeftOuterJoin` shipOwner
                            `E.LeftOuterJoin` vehicleOwner) -> do
                    E.on (vehicle E.?. VehicleOwnerId E.==. vehicleOwner E.?. PersonId)
                    E.on (ship E.?. ShipOwnerId E.==. shipOwner E.?. PersonId)
                    E.on (ship E.?. ShipId E.==. E.just (unit E.^. UnitShipId))
                    E.on (vehicle E.?. VehicleId E.==. E.just (unit E.^. UnitVehicleId))
                    E.where_ (shipOwner E.?. PersonFactionId E.==. (E.just (E.just (E.val fId)))
                             E.||. vehicleOwner E.?. PersonFactionId E.==. (E.just (E.just (E.val fId))))
                    E.orderBy [ E.asc ( unit E.^. UnitId) ]
                    return (unit, vehicle, ship)

    let grouped = groupBy (\(a, _, _) (b, _, _) ->
                            (entityKey a) == (entityKey b )) res
    return $ mapMaybe unitResToUnitEntity grouped


unitResToUnitEntity :: [(Entity Unit, Maybe (Entity Vehicle), Maybe (Entity Ship))] -> Maybe (UnitId, Unit')
unitResToUnitEntity res@((unit, _, _):_) =
    fmap ((,) (entityKey unit) ) $ unitResToUnit res

unitResToUnitEntity [] =
    Nothing


getUnitCrew :: (MonadIO m, BackendCompatible SqlBackend backend,
    PersistQueryRead backend, PersistUniqueRead backend) =>
    UnitId -> ReaderT backend m [(CrewAssignment, Person)]
getUnitCrew uId = do
    res <- E.select $
            E.from $ \(assignment `E.InnerJoin` person) -> do
                E.on (person E.^. PersonId E.==. assignment E.^. CrewAssignmentPersonId)
                E.where_ (assignment E.^. CrewAssignmentUnitId E.==. (E.val uId))
                return (assignment, person)
    return $ (\(a, p) -> (entityVal a, entityVal p)) <$> res
