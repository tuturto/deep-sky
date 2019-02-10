{-# LANGUAGE NoImplicitPrelude     #-}

module Dto.Icons ( IconMapper(..) )
    where

import ClassyPrelude.Yesod   as Import


-- | mapper that can be used to retrieve link to icon as Text.
-- This is used for example returning links to these resources as a part of
-- JSON message. Client can then use the link to retrieve actual image file
-- and take advantage of caching.
newtype IconMapper a =
    IconMapper { runIconMapper :: a -> Text }
