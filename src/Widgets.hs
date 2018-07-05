{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Widgets where

import Import
import Report

systemDetails :: CollatedStarSystemReport -> WidgetFor site ()
systemDetails systemReport = $(widgetFile "systemdetailsW")

planetReportList :: Foldable t => t CollatedPlanetReport -> WidgetFor App ()
planetReportList planetReports = $(widgetFile "planetlistW")

planetDetails :: CollatedPlanetReport -> WidgetFor App ()
planetDetails planetReport = $(widgetFile "planetdetailsW")

starReportList :: Foldable t => t CollatedStarReport -> WidgetFor site ()
starReportList starReports = $(widgetFile "starListW")
