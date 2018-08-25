{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Widgets ( systemDetails, planetReportList, planetDetails, starReportList
               , starLaneList, buildingList, populationList, shipList, newsArticleWidget )
    where

import Import
import Report
import MenuHelpers (toDisplayDate)
import News

systemDetails :: CollatedStarSystemReport -> WidgetFor site ()
systemDetails systemReport = $(widgetFile "widgets/systemDetailsW")

planetReportList :: Foldable t => t CollatedPlanetReport -> WidgetFor App ()
planetReportList planetReports = $(widgetFile "widgets/planetListW")

planetDetails :: CollatedPlanetReport -> WidgetFor App ()
planetDetails planetReport = $(widgetFile "widgets/planetDetailsW")

starReportList :: Foldable t => t CollatedStarReport -> WidgetFor site ()
starReportList starReports = $(widgetFile "widgets/starListW")

starLaneList :: Foldable t => t CollatedStarLaneReport -> WidgetFor App ()
starLaneList starLaneReports = $(widgetFile "widgets/starlanesW")

buildingList :: Foldable t => t CollatedBuildingReport -> WidgetFor site ()
buildingList buildingReports = $(widgetFile "widgets/buildingListW")

populationList :: Foldable t => t CollatedPopulationReport -> WidgetFor site ()
populationList populationReports = $(widgetFile "widgets/populationListW")

shipList :: Foldable t => t (Ship, Faction) -> WidgetFor site ()
shipList ships = $(widgetFile "widgets/shipListW")

newsArticleWidget :: NewsArticle -> WidgetFor App ()
newsArticleWidget article = $(widgetFile "widgets/news/articleW")
 
newsContentWidget :: NewsArticle -> WidgetFor App ()
newsContentWidget (StarFoundNews starName systemName systemId _) = $(widgetFile "widgets/news/starFoundW")
newsContentWidget (PlanetFoundNews planetName systemName systemId planetId _) = $(widgetFile "widgets/news/planetFoundW")
