{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Widgets ( systemDetails, planetReportList, planetDetails, starReportList
               , starLaneList, buildingList, populationList, shipList, newsArticleWidget )
    where

import Import
import Report ( spectralInfo, CollatedPopulationReport(..), CollatedBuildingReport(..)
              , CollatedStarLaneReport(..), CollatedStarReport(..), CollatedPlanetReport(..)
              , CollatedStarSystemReport(..) )
import CustomTypes ( SpectralType(..) )
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

newsArticleWidget :: (Key News, NewsArticle) -> WidgetFor App ()
newsArticleWidget article = $(widgetFile "widgets/news/articleW")
 
newsContentWidget :: NewsArticle -> WidgetFor App ()
newsContentWidget (StarFoundNews starName systemName systemId _) = $(widgetFile "widgets/news/starFoundW")
newsContentWidget (PlanetFoundNews planetName systemName systemId planetId _) = $(widgetFile "widgets/news/planetFoundW")
newsContentWidget (UserWrittenNews content _ _ user) = $(widgetFile "widgets/news/userNewsW")
newsContentWidget (DesignCreatedNews _ name _) = $(widgetFile "widgets/news/blueprintW")
 
newsImage :: NewsArticle -> Route App
newsImage (StarFoundNews {}) = StaticR images_news_sun_png
newsImage (PlanetFoundNews {}) = StaticR images_news_planet_png
newsImage (DesignCreatedNews {}) = StaticR images_news_blueprint_png
newsImage (UserWrittenNews { userWrittenNewsIcon = icon }) = 
    case icon of
        GenericUserNews ->
            StaticR images_news_question_png
        JubilationUserNews ->
            StaticR images_news_jubileum_png
        CatUserNews ->
            StaticR images_news_cat_png


