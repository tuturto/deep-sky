module Common (maybeGet, chooseOne, filterMap)
    where

import System.Random
import Data.Maybe (fromJust, isJust)

-- | Get item from list with given index
--   If item is within bounds, return Just it, otherwise Nothing
maybeGet :: [a] -> Int -> Maybe a
maybeGet col index
    | index < 0             = Nothing
    | index >= (length col) = Nothing
    | otherwise             = Just (col !! index)

chooseOne :: a -> a -> IO a
chooseOne item1 item2 = do
    n <- randomRIO (0, 1) :: IO Integer
    return $ case n of
                0 -> item1
                _ -> item2

-- | do map and filter out Nothings
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f a =
    map fromJust $ filter isJust $ map f a