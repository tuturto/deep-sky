{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Widgets where

import Import
import Report

planetReportList planetReports = $(widgetFile "planetlistW")

systemDetails systemReport = $(widgetFile "systemdetailsW")
