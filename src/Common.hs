module Common (maybeGet, chooseOne)
    where

import System.Random

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
