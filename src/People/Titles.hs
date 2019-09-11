{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module People.Titles ( shortTitle, longTitle )
    where

import Import
import People.Data ( ShortTitle(..), LongTitle(..) )

shortTitle :: Person -> Maybe ShortTitle
shortTitle Person { personPlanetTitle = Just _ } =
    Just "Praetor"

shortTitle Person { personStarSystemTitle = Just _ } =
    Just "Procurator"

shortTitle _ =
    Nothing


longTitle :: Person -> Maybe LongTitle
longTitle _ =
    Just "unimplemented"