{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}


module Handler.AdminPanel
    ( getAdminPanelR, getAdminApiSimulationR, putAdminApiSimulationR
    , getAdminApiPeopleR, getAdminPeopleR, getAdminPersonR, getAdminApiPersonR
    , putAdminApiPersonR, getAdminAddPersonR, postAdminApiAddPersonR
    )
    where

import Control.Lens ( (#), (^?) )
import Control.Monad.Random ( evalRand )
import Database.Persist.Sql (toSqlKey)
import Data.Either.Validation ( Validation(..), _Failure, _Success )
import Data.Maybe ( isNothing, fromJust )
import Import
import qualified Prelude as P
import System.Random ( newStdGen )

import Common ( apiRequireAdmin, pagedResult, apiNotFound )
import Creators.Person ( PersonOptions(..), AgeOptions(..), generatePersonM )
import CustomTypes ( SystemStatus(..) )
import Errors ( ErrorCode(..), raiseIfErrors, raiseIfFailure )
import Handler.Home ( getNewHomeR )
import MenuHelpers ( starDate )
import People.Data ( StatScore(..), firstName, cognomen, familyName
                   , regnalNumber, unFirstName, unCognomen, unFamilyName )
import Simulation.Main ( processTurn )


-- | Get admin panel
getAdminPanelR :: Handler Html
getAdminPanelR = getNewHomeR


-- | Get people listing for admins
getAdminPeopleR :: Handler Html
getAdminPeopleR = getNewHomeR

-- | Get single person for admins
getAdminPersonR :: PersonId -> Handler Html
getAdminPersonR _ = getNewHomeR


-- | Add new person
getAdminAddPersonR :: Handler Html
getAdminAddPersonR = getNewHomeR


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
        runDB $ update (toSqlKey 1) [ SimulationStatus =. ProcessingTurn ]
        _ <- runDB $ processTurn
        runDB $ update (toSqlKey 1) [ SimulationStatus =. Online ]
        return ()

    when (fmap simulationStatus status /= (Just $ simulationStatus msg)) $ do
        _ <- runDB $ update (toSqlKey 1) [ SimulationStatus =. (simulationStatus msg) ]
        return ()

    -- load and return simulation status
    finalStatus <- runDB $ get $ toSqlKey 1
    let _ = simulationCurrentTime <$> finalStatus
    return $ toJSON finalStatus


-- | All people in the simulation
-- supports query parameters skip and take
getAdminApiPeopleR :: Handler Value
getAdminApiPeopleR = do
    _ <- apiRequireAdmin
    skipParamM <- lookupGetParam "skip"
    takeParamM <- lookupGetParam "take"
    let skipParam = maybe 0 id (join $ fmap readMay skipParamM)
    let takeParam = maybe 60000 id (join $ fmap readMay takeParamM)

    people <- runDB $ selectList [] [ Asc PersonId
                                    , OffsetBy skipParam
                                    , LimitTo takeParam
                                    ]

    return $ toJSON $ pagedResult skipParam takeParam people


-- | Details of single person
getAdminApiPersonR :: PersonId -> Handler Value
getAdminApiPersonR pId = do
    _ <- apiRequireAdmin
    person <- runDB $ get pId
    when (isNothing person) apiNotFound

    return $ toJSON (Entity pId $ fromJust person)


-- | Update details of single person
putAdminApiPersonR :: PersonId -> Handler Value
putAdminApiPersonR pId = do
    _ <- apiRequireAdmin
    msg <- requireJsonBody
    errors <- runDB $ updatePerson pId msg
    _ <- raiseIfErrors errors
    returnJson (Entity pId msg)


-- | Create new person
postAdminApiAddPersonR :: Handler Value
postAdminApiAddPersonR = do
    _ <- apiRequireAdmin
    msg <- requireJsonBody
    date <- runDB $ starDate
    _ <- raiseIfFailure $ validateAddPerson msg
    g <- liftIO newStdGen
    let person = evalRand (generatePersonM date msg) g
    pId <- runDB $ insert person
    returnJson (Entity pId person)


updatePerson :: (MonadIO m,
    PersistUniqueRead backend, PersistStoreWrite backend,
    BaseBackend backend ~ SqlBackend)
    => PersonId -> Person -> ReaderT backend m [ErrorCode]
updatePerson pId msg = do
    person <- get pId
    simulation <- get $ toSqlKey 1
    let errors = validatePersonPut simulation person msg
    when (P.null $ concat $ errors ^? _Failure) $ do
        replace pId msg
    return $ concat $ errors ^? _Failure


-- | Given simulation status loaded from database, validate new simulation status
validateSimulationPut :: Maybe Simulation -> Simulation -> Validation [ErrorCode] Simulation
validateSimulationPut old new =
    case old of
        Nothing ->
            _Failure # [ SimulationStatusNotFound ]

        Just oldStatus ->
            pure new
                <* timeDifferenceIsNotTooBig oldStatus new
                <* onlyTimeOrStatusChanges oldStatus new


-- | Time difference between two steps in simulation should be exactly one
timeDifferenceIsNotTooBig :: Simulation -> Simulation -> Validation [ErrorCode] Simulation
timeDifferenceIsNotTooBig old new =
    if dt == 0 || dt == 1
        then
            _Success # new
        else
            _Failure # [ DeltaTIsTooBig ]
    where
        dt = simulationCurrentTime new - simulationCurrentTime old


-- | I's not allowed to process turn and change system status in one go
onlyTimeOrStatusChanges :: Simulation -> Simulation -> Validation [ErrorCode] Simulation
onlyTimeOrStatusChanges old new =
    if dt /= 0 && (simulationStatus old /= simulationStatus new)
        then
            _Failure # [ TurnProcessingAndStateChangeDisallowed ]
        else
            _Success # new
    where
        dt = simulationCurrentTime new - simulationCurrentTime old


-- | Validate updating person
validatePersonPut :: Maybe Simulation -> Maybe Person -> Person -> Validation [ErrorCode] Person
validatePersonPut simulation old new =
    case old of
        Nothing ->
            _Failure # [ ResourceNotFound ]

        Just _ ->
            pure new
                <* statsAreValid new
                <* dateOfBirthIsNotInFuture simulation new
                <* nameIsValid new


-- | Validate that all stats are ok
statsAreValid :: Person -> Validation [ErrorCode] Person
statsAreValid person =
    pure person
        <* statIsZeroOrGreater personDiplomacy "diplomacy" person
        <* statIsZeroOrGreater personMartial "martial" person
        <* statIsZeroOrGreater personStewardship "stewardship" person
        <* statIsZeroOrGreater personLearning "learning" person
        <* statIsZeroOrGreater personIntrique "intrique" person


-- | Stat should be zero or greater
statIsZeroOrGreater :: (Person -> StatScore a) -> Text -> Person -> Validation [ErrorCode] Person
statIsZeroOrGreater stat name new =
    if stat new >= 0
        then
            _Success # new
        else
            _Failure # [ StatIsTooLow name ]


-- | Person's date of birth should not be in future
dateOfBirthIsNotInFuture :: Maybe Simulation -> Person -> Validation [ErrorCode] Person
dateOfBirthIsNotInFuture Nothing _ =
    _Failure # [ CouldNotConfirmDateOfBirth ]

dateOfBirthIsNotInFuture (Just simulation) person =
    if personDateOfBirth person <= simulationCurrentTime simulation
        then
            _Success # person
        else
            _Failure # [ DateOfBirthIsInFuture ]


-- | Validate various aspects of person's name
nameIsValid :: Person -> Validation [ErrorCode] Person
nameIsValid person =
    pure person
        <* firstNameIsNotEmpty person
        <* cognomenIsNotEmpty person
        <* familyNameIsNotEmpty person
        <* regnalNumberIsNotNegative person


-- | First name can't be empty text
firstNameIsNotEmpty :: Person -> Validation [ErrorCode] Person
firstNameIsNotEmpty person =
    if (not . null . unFirstName . firstName . personName) person
        then
            _Success # person
        else
            _Failure # [ FirstNameIsEmpty ]


-- | Cognomen can't be empty text
cognomenIsNotEmpty :: Person -> Validation [ErrorCode] Person
cognomenIsNotEmpty person =
    case (cognomen . personName) person of
        Nothing ->
            _Success # person

        Just s ->
            if (not . null . unCognomen) s
                then
                    _Success # person
                else
                    _Failure # [ CognomenIsEmpty ]


-- | Family name can't be empty text
familyNameIsNotEmpty :: Person -> Validation [ErrorCode] Person
familyNameIsNotEmpty person =
    case (familyName . personName) person of
        Nothing ->
            _Success # person

        Just s ->
            if (not . null . unFamilyName) s
                then
                    _Success # person
                else
                    _Failure # [ FamilyNameIsEmpty ]


-- | Regnal number should be zero or greater
regnalNumberIsNotNegative :: Person -> Validation [ErrorCode] Person
regnalNumberIsNotNegative person =
    case (regnalNumber . personName) person of
        Nothing ->
            _Success # person

        Just n ->
            if n >= 0
                then
                    _Success # person
                else
                    _Failure # [ RegnalNumberIsLessThanZero ]


-- | Validate person add message
validateAddPerson :: PersonOptions -> Validation [ErrorCode] PersonOptions
validateAddPerson opt =
        pure opt
            <* validateAgeOptions opt


validateAgeOptions :: PersonOptions -> Validation [ErrorCode] PersonOptions
validateAgeOptions opt =
    case personOptionsAge opt of
        Nothing ->
            _Success # opt

        Just (AgeBracket a b) ->
            if a <= b
                then _Success # opt
                else _Failure # [ AgeBracketStartIsGreaterThanEnd ]

        Just (ExactAge _) ->
            _Success # opt
