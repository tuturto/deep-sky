{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FunctionalDependencies #-}

module Common ( maybeGet, chooseOne, requireFaction, apiRequireFaction, apiRequireAuthPair
              , FromDto(..), ToDto(..), apiNotFound, apiInvalidArgs, apiInternalError, apiOk
              , safeHead, apiForbidden, mkUniq )
    where

import Import
import qualified Prelude as P ( (!!), length )
import Data.Set
import System.Random


-- | Get item from list with given index
--   If item is within bounds, return Just it, otherwise Nothing
maybeGet :: Int -> [a] -> Maybe a
maybeGet i col
    | i < 0             = Nothing
    | i >= P.length col = Nothing
    | otherwise         = Just (col P.!! i)


-- | Get head of a list, if list is empty, return Nothing
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing


chooseOne :: a -> a -> IO a
chooseOne item1 item2 = do
    n <- randomRIO (0, 1) :: IO Integer
    return $ case n of
                0 -> item1
                _ -> item2


-- | Check that user has logged in and is member of a faction
--   In case user is not member of a faction, http 500 will be returned as an error page
requireFaction :: HandlerFor App (AuthId (HandlerSite (HandlerFor App)), User, Key Faction)
requireFaction = do
    (authId, user) <- requireAuthPair
    fId <- case userFactionId user of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of a faction" :: Text)
    return (authId, user, fId)


-- | Check that user has logged in
--   In case user is not logged in, http 401 with json body will be returned
apiRequireAuthPair :: HandlerFor App (AuthId (HandlerSite (HandlerFor App)), AuthEntity App)
apiRequireAuthPair = do
    authData <- maybeAuthPair
    case authData of
            Just x -> return x
            Nothing -> sendStatusJSON status401 $ toJSON $ ErrorJson "Not logged in"


-- | Check that user has logged in and is member of a faction
--   In case user is not member of a faction, http 500 with json body will be returned
apiRequireFaction :: HandlerFor App (AuthId (HandlerSite (HandlerFor App)), User, Key Faction)
apiRequireFaction = do
    (authId, user) <- apiRequireAuthPair
    fId <- case userFactionId user of
                        Just x -> return x
                        Nothing -> sendStatusJSON status500 $ toJSON $ ErrorJson "Not a member of a faction"
    return (authId, user, fId)


-- | Send 404 error with json body
apiNotFound :: HandlerFor App a
apiNotFound =
    sendStatusJSON status404 $ toJSON $ ErrorJson "Resource not found"


-- | Send 400 error with json body containing list of names of all invalid arguments
apiInvalidArgs :: [Text] -> HandlerFor App a
apiInvalidArgs params =
    sendStatusJSON status400 $ toJSON $ ErrorsJson params


-- | Send 403 error with json body containing error message
apiForbidden :: Text -> HandlerFor App a
apiForbidden explanation =
    sendStatusJSON status403 $ toJSON $ ErrorJson explanation


-- | Send 500 error with json body
apiInternalError :: HandlerFor App a
apiInternalError =
    sendStatusJSON status500 $ toJSON $ ErrorJson "Internal error occurred"


-- | Send 200 with json body
apiOk :: (MonadHandler m, ToJSON a) => a -> m a
apiOk content =
    sendStatusJSON status200 $ toJSON content


-- | Class to transform dto to respective entity
class FromDto c d | c -> d where
    fromDto :: d -> c


-- | Class to transfrom entity to dto
class (ToJSON d) => ToDto c d | c -> d where
    toDto :: c -> d


data ErrorJson = ErrorJson { unerror :: Text }
    | ErrorsJson { unerrors :: [Text] }


instance ToJSON ErrorJson where
    toJSON ErrorJson { unerror = err } =
        object [ "errors" .= [err] ]

    toJSON ErrorsJson { unerrors = errs } =
        object [ "errors" .= errs ]


mkUniq :: Ord a => [a] -> [a]
mkUniq = Data.Set.toList . Data.Set.fromList
