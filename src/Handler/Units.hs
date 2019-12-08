{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Handler.Units
    ( getApiUnitsR, getApiUnitR, postApiUnitR, putApiUnitR, getUnitR )
    where

import Import
import Data.Maybe ( fromJust )

import Common ( apiRequireFaction, apiRequireViewSimulation, apiNotFound )
import Handler.Home ( getNewHomeR )
import Units.Data ( StatsReportDetails(..) )
import Units.Queries ( Unit'(..), getUnit, unitOwnerId, unitDesignId, getUnitCrew )
import Units.Reports ( UnitObservationDetails(..), UnitReport(..), ownerReport
                     , otherReport, deserializeObservations )


-- | serve elm client
getUnitR :: UnitId -> Handler Html
getUnitR _ =
    getNewHomeR


-- details of multiple units
getApiUnitsR :: Handler Value
getApiUnitsR = do
    (uId, _, _, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    skipParamM <- lookupGetParam "skip"
    takeParamM <- lookupGetParam "take"
    let skipParam = maybe (0 :: Integer) id (join $ fmap readMay skipParamM)
    let takeParam = maybe (100 :: Integer) id (join $ fmap readMay takeParamM)
    -- TODO: unit owner: own, vassal, allied, other
    -- TODO: load units / unit observations
    -- TODO: construct return message
    undefined


-- details of a single unit
getApiUnitR :: UnitId -> Handler Value
getApiUnitR unitId = do
    (uId, _, avatar, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    maybeUnit <- runDB $ getUnit unitId
    when (isNothing maybeUnit) apiNotFound

    let unit = fromJust maybeUnit
    let dId = unitDesignId unit
    design <- runDB $ get dId

    report <- runDB $ unitReport fId avatar (unitId, unit) $ fromJust design

    returnJson report


-- | Unit report of given unit from point of view of faction and particular person
-- | If unit is owned by the person, more data will be directly available.
-- | Otherwise details of the report will be based on the observations that have been
-- | done previously.
unitReport :: (PersistQueryRead backend, PersistUniqueRead backend, MonadIO m
    , BaseBackend backend ~ SqlBackend, BackendCompatible SqlBackend backend) =>
    FactionId
    -> Entity Person
    -> (UnitId, Unit')
    -> Design
    -> ReaderT backend m UnitReport
unitReport fId avatar (unitId, unit) design =
    case unitOwnershipType unit avatar of
        OwnUnit -> do
            stats <- getUnitStats fId unitId $ unitDesignId unit
            crew <- getUnitCrew unitId
            return $ ownerReport unit stats design crew

        OtherUnit -> do
            stats <- getUnitStats fId unitId $ unitDesignId unit
            observations <- getObservationDetails fId unitId

            return $ otherReport unit stats observations design


-- | Load known stats for unit / design using knowledge available to given faction
getUnitStats :: (PersistQueryRead backend, MonadIO m, BaseBackend backend ~ SqlBackend) =>
   FactionId -> UnitId -> DesignId -> ReaderT backend m [StatsReportDetails]
getUnitStats fId uId dId = do
    unitStats <- selectList [ UnitStatsReportUnitId ==. uId
                            , UnitStatsReportOwnerId ==. fId
                            ] [ Desc UnitStatsReportDate ]
    classStats <- selectList [ DesignStatsReportDesignId ==. dId
                             , DesignStatsReportOwnerId ==. fId
                             ] [ Desc DesignStatsReportDate ]

    -- unit stats are given precedence. Design stats are used only when no unit stats are
    -- available for particular aspect
    return $ (unitStatsReportContent . entityVal <$> unitStats) ++ (designStatsReportContent . entityVal <$> classStats)


-- | Load observations for unit using knowledge available to given faction
getObservationDetails :: (PersistQueryRead backend, MonadIO m, BaseBackend backend ~ SqlBackend) =>
    FactionId -> UnitId -> ReaderT backend m [UnitObservationDetails]
getObservationDetails fId uId = do
    os <- selectList [ UnitObservationUnitId ==. uId
                     , UnitObservationOwnerId ==. fId
                     ] [ Desc UnitObservationDate ]
    return $ deserializeObservations $ (unitObservationContent . entityVal) <$> os


-- create a new unit
postApiUnitR :: UnitId -> Handler Value
postApiUnitR = undefined


-- update details of an unit
putApiUnitR :: UnitId -> Handler Value
putApiUnitR = undefined


-- | Ownership relation between unit and person
unitOwnershipType :: Unit' -> Entity Person -> UnitOwnershipType
unitOwnershipType unit person =
        if unitOwnerId unit == entityKey person
            then OwnUnit
            else OtherUnit


data UnitOwnershipType
    = OwnUnit
    -- | VassalUnit // added when vassals are in game
    | OtherUnit
    deriving (Show, Read, Eq)
