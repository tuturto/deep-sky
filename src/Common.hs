{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}

module Common ( maybeGet, chooseOne, requireFaction, apiRequireFaction, apiRequireAuthPair
              , FromDto(..), ToDto(..), apiNotFound, apiInvalidArgs, apiInternalError, apiOk
              , safeHead, apiForbidden, mkUniq, choose, getR, apiError, entityValL, entityKeyL
              , Frequency(..), clamp )
    where

import Import
import qualified Prelude as P ( (!!), length )
import Control.Lens ( Lens', lens )
import Data.Set
import qualified Data.List as List
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
requireFaction :: HandlerFor App (AuthId (HandlerSite (HandlerFor App)), User, Entity Person, Key Faction)
requireFaction = do
    (authId, user) <- requireAuthPair
    pId <- case userAvatar user of
                Nothing ->
                    sendResponseStatus status500 ("No avatar selected" :: Text)

                Just x ->
                    return x
    loaded <- runDB $ get pId
    avatar <- case loaded of
                Nothing ->
                    sendResponseStatus status500 ("No avatar found" :: Text)

                Just x ->
                    return x

    fId <- case personFactionId avatar of
                Nothing ->
                    sendResponseStatus status500 ("Not a memeber of faction" :: Text)

                Just x ->
                    return x


    return (authId, user, Entity pId avatar, fId)


-- | Check that user has logged in
--   In case user is not logged in, http 401 with json body will be returned
apiRequireAuthPair :: HandlerFor App (AuthId (HandlerSite (HandlerFor App)), AuthEntity App)
apiRequireAuthPair = do
    authData <- maybeAuthPair
    case authData of
            Just x ->
                return x

            Nothing ->
                sendStatusJSON status401 $ toJSON $ ErrorJson "Not logged in"


-- | Check that user has logged in and is member of a faction
--   In case user is not member of a faction, http 500 with json body will be returned
apiRequireFaction :: HandlerFor App (AuthId (HandlerSite (HandlerFor App)), User, Entity Person, Key Faction)
apiRequireFaction = do
    (authId, user) <- apiRequireAuthPair
    pId <- case userAvatar user of
                Nothing ->
                    sendStatusJSON status500 ("No avatar selected" :: Text)

                Just x ->
                    return x
    loaded <- runDB $ get pId
    avi <- case loaded of
                Nothing ->
                    sendStatusJSON status500 ("No avatar found" :: Text)

                Just foo ->
                    return foo

    fId <- case personFactionId avi of
                Nothing ->
                    sendStatusJSON status500 ("Not a memeber of faction" :: Text)

                Just x ->
                    return x


    return (authId, user, Entity pId avi, fId)


-- | Send 404 error with json body
apiNotFound :: HandlerFor App a
apiNotFound =
    sendStatusJSON status404 $ toJSON $ ErrorJson "Resource not found"


-- | Send 400 (Bad request) error with json body containing list of names of all invalid arguments
-- The server cannot or will not process the request due to an apparent client error
-- (e.g., malformed request syntax, size too large, invalid request message framing,
-- or deceptive request routing).
apiInvalidArgs :: [Text] -> HandlerFor App a
apiInvalidArgs params =
    sendStatusJSON status400 $ toJSON $ ErrorsJson params


-- | Send 403 (Forbidden) error with json body containing error message
-- The request was valid, but the server is refusing action. The user might not
-- have the necessary permissions for a resource, or may need an account of some sort.
apiForbidden :: Text -> HandlerFor App a
apiForbidden explanation =
    sendStatusJSON status403 $ toJSON $ ErrorJson explanation


-- | Send 500 (Internal server error) error with json body
apiInternalError :: HandlerFor App a
apiInternalError =
    sendStatusJSON status500 $ toJSON $ ErrorJson "Internal error occurred"


-- | Send 200 (ok) with json body
apiOk :: (MonadHandler m, ToJSON a) => a -> m a
apiOk content =
    sendStatusJSON status200 $ toJSON content


-- | Return error message as JSON
-- when unknown status is given, 500 is used instead
apiError :: (Status, Text) -> HandlerFor App a2
apiError (status, msg) =
    case statusCode status of
        400 ->
            apiInvalidArgs [msg]
        404 ->
            apiNotFound
        403 ->
            apiForbidden msg
        500 ->
            apiInternalError
        _ ->
            apiInternalError


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


-- | Frequency or weight of a
data Frequency a = Frequency Int a
    deriving (Show, Read, Eq)


 -- | Randomly choose item from weighted list
 -- In case of empty list, Nothing is returned
choose :: [Frequency a] -> IO (Maybe a)
choose [] =
    return Nothing

choose items =
    pick items <$> randomRIO (1, total)
    where
        total = sum $ fmap (\(Frequency x _) -> x) items
        pick [] _ = Nothing
        pick (Frequency x item:xs) n
            | n <= x = Just item
            | otherwise = pick xs (n - x)


-- | get n unique entries from given list in random order
-- | if n > length of list, all items of the list will be returned
getR :: RandomGen g => g -> Int -> [a] -> [a]
getR _ 0 _ =
    []

getR _ _ [] =
    []

getR g n xs =
    fmap (xs P.!!) ids
    where
        ids = List.take (min n $ length xs) $ List.nub $ randomRs (0, length xs - 1) g


-- | Lens for accessing entity value in Entity
entityValL :: Lens' (Entity a) a
entityValL = lens entityVal (\(Entity key _) value -> Entity key value)


-- | Lens for accessing entity key in Entity
entityKeyL :: Lens' (Entity a) (Key a)
entityKeyL = lens entityKey (\(Entity _ value) key -> Entity key value)


-- | Clamp value within a given parameters
-- note that if start > end, this function will have odd value
clamp :: Ord a => a -> a -> a -> a
clamp start end val =
        max start $ min end val
