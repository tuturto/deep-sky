{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}


module Handler.AdminPanel
    ( getAdminPanelR, getAdminApiSimulationR, putAdminApiSimulationR
    , getAdminApiPeopleR )
    where

import Control.Lens ( (#) )
import Database.Persist.Sql (toSqlKey)
import Data.Either.Validation ( Validation(..), _Failure, _Success )
import Data.Maybe ( isNothing )
import Import

import Common ( apiRequireAdmin )
import Errors ( ErrorCode(..), raiseIfErrors, raiseIfFailure )
import Handler.Home ( getNewHomeR )
import Simulation.Main ( processTurn )


-- | Get admin panel
getAdminPanelR :: Handler Html
getAdminPanelR = getNewHomeR


-- | Get current state of simulation
getAdminApiSimulationR :: Handler Value
getAdminApiSimulationR = do
    _ <- apiRequireAdmin
    status <- runDB $ get $ toSqlKey 1
    let t = simulationCurrentTime <$> status
    when (isNothing status) $ do
        raiseIfErrors [ SimulationStatusNotFound ]
    return $ toJSON status


-- | Update current state of simulation
putAdminApiSimulationR :: Handler Value
putAdminApiSimulationR = do
    _ <- apiRequireAdmin
    msg <- requireJsonBody
    status <- runDB $ get $ toSqlKey 1
    _ <- raiseIfFailure $ validateSimulationPut status msg

    -- update simulation status
    -- at later point we might want to do this in a separate process
    -- and return from server immediately
    when (fmap simulationCurrentTime status /= (Just $ simulationCurrentTime msg)) $ do
        -- TODO: set simulation status to 'ProcessingTurn'
        _ <- runDB $ processTurn
        return ()

    -- load and return simulation status
    finalStatus <- runDB $ get $ toSqlKey 1
    let _ = simulationCurrentTime <$> finalStatus
    return $ toJSON finalStatus


-- | Given simulation status loaded from database, validate new simulation status
validateSimulationPut :: Maybe Simulation -> Simulation -> Validation [ErrorCode] Simulation
validateSimulationPut old new =
    case old of
        Nothing ->
            _Failure # [ SimulationStatusNotFound ]

        Just oldStatus ->
            pure oldStatus
                <* timeDifferenceIsNotTooBig oldStatus new


-- | Timme difference between two steps in simulation should be exactly one
timeDifferenceIsNotTooBig :: Simulation -> Simulation -> Validation [ErrorCode] Simulation
timeDifferenceIsNotTooBig old new =
    if dt == 0 || dt == 1
        then
            _Success # old
        else
            _Failure # [ DeltaTIsTooBig ]
    where
        dt = simulationCurrentTime new - simulationCurrentTime old


-- | All people in the simulation
getAdminApiPeopleR :: Handler Value
getAdminApiPeopleR = undefined
