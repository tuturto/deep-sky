{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Common ( maybeGet, chooseOne, requireFaction )
    where

import Import
import qualified Prelude as P ( (!!), length )
import System.Random
import Data.Maybe (fromJust, isJust)

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
--   In case user is not member of a faction, http 500 will be returned
requireFaction :: HandlerFor App (AuthId (HandlerSite (HandlerFor App)), User, Key Faction)
requireFaction = do
    (authId, user) <- requireAuthPair   
    fId <- case (userFactionId user) of
                        Just x -> return x
                        Nothing -> sendResponseStatus status500 ("Not a member of faction" :: Text)
    return (authId, user, fId)
