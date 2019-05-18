{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Dto.Common
    ( StarDateResponse(..) )
    where

import Import
import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import CustomTypes ( StarDate )


data StarDateResponse = StarDateResponse
    { starDateResponseCurrentTime :: StarDate }
    deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = drop 16 } ''StarDateResponse)
