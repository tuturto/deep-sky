{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Common ( maybeGet, chooseOne, requireFaction, apiRequireFaction, apiRequireAuthPair
              , DtoTransform(..), apiNotFound )
    where

import Import
import qualified Prelude as P ( (!!), length )
import System.Random
import Data.Maybe (fromJust, isJust)
import Data.Aeson.TH

-- | Get item from list with given index
--   If item is within bounds, return Just it, otherwise Nothing
maybeGet :: [a] -> Int -> Maybe a
maybeGet col index
    | index < 0               = Nothing
    | index >= (P.length col) = Nothing
    | otherwise               = Just (col P.!! index)

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
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of a faction" :: Text)
    return (authId, user, fId)

-- | Check that user has logged in 
--   In case user is not logged in, http 401 with json body will be returned
apiRequireAuthPair :: HandlerFor App (AuthId (HandlerSite (HandlerFor App)), AuthEntity App)
apiRequireAuthPair = do
    authData <- maybeAuthPair
    res <- case authData of
                    Just x -> return x
                    Nothing -> sendStatusJSON status401 $ toJSON $ ErrorJson "Not logged in"
    return res 

-- | Check that user has logged in and is member of a faction
--   In case user is not member of a faction, http 500 with json body will be returned
apiRequireFaction :: HandlerFor App (AuthId (HandlerSite (HandlerFor App)), User, Key Faction)
apiRequireFaction = do
    (authId, user) <- apiRequireAuthPair
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendStatusJSON status500 $ toJSON $ ErrorJson "Not a member of a faction"
    return (authId, user, fId)

apiNotFound :: HandlerFor App a
apiNotFound = 
    sendStatusJSON status404 $ toJSON $ ErrorJson ("Resource not found" :: Text)

-- | Class to transform dto to respective entity
class DtoTransform d c where
    fromDto :: d -> c

data ErrorJson = ErrorJson { unerror :: Text }

$(deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''ErrorJson)
