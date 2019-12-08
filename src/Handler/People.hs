{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.People
    ( getApiPersonR, getPersonR, getPeopleR, getApiDemesneR )
    where

import Import
import Data.Maybe ( fromJust )
import Common ( apiRequireFaction, apiNotFound, apiRequireViewSimulation )
import MenuHelpers ( starDate )
import People.Import ( personReport, demesneReport )
import Handler.Home ( getNewHomeR )


-- | serve client program and have it started showing person details
getPersonR :: PersonId -> Handler Html
getPersonR _ = getNewHomeR


-- | serve client program and have it started showing database
getPeopleR :: Handler Html
getPeopleR = getNewHomeR


-- | Information of single person, taking intel level into account
getApiPersonR :: PersonId -> HandlerFor App Value
getApiPersonR pId = do
    (uId, _, avatar, fId) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    target <- runDB $ get pId
    when (isNothing target) apiNotFound

    report <- runDB $ personReport avatar fId (Entity pId (fromJust target))

    returnJson report


-- | Demesne of given character, according to intelligence level
getApiDemesneR :: PersonId -> HandlerFor App Value
getApiDemesneR pId = do
    (uId, _, avatar, _) <- apiRequireFaction
    _ <- apiRequireViewSimulation uId
    today <- runDB $ starDate
    person <- runDB $ get pId
    when (isNothing person) apiNotFound
    intel <- runDB $ selectList [ HumanIntelligencePersonId ==. pId
                                , HumanIntelligenceOwnerId ==. entityKey avatar
                                ] []
    planets <- runDB $ selectList [ PlanetRulerId ==. Just pId ] []
    systems <- runDB $ selectList [ StarSystemRulerId ==. Just pId ] []
    let report = demesneReport today (fromJust person) systems planets (entityVal <$> intel)
    return $ toJSON report
