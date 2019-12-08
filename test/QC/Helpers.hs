module QC.Helpers (isMaybeTrue, deserializationMirrosSerialization)
    where

import Data.Aeson ( decode, encode, FromJSON, ToJSON )

-- | Helper to check if Maybe Bool is True
isMaybeTrue :: Maybe Bool -> Bool
isMaybeTrue (Just x) = x == True
isMaybeTrue Nothing = False


-- | Check if deserializing a serialized object will yield original data
deserializationMirrosSerialization :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
deserializationMirrosSerialization dto =
    isMaybeTrue $ fmap (== dto) $ (decode . encode) dto