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
import Common ( apiRequireFaction, apiNotFound )
import MenuHelpers ( starDate )
import People.Import ( personReport, demesneReport )
import Handler.Home ( getNewHomeR )


-- | serve client program and have it started showing person details
getPersonR :: Key Person -> Handler Html
getPersonR _ = getNewHomeR


-- | serve client program and have it started showing database
getPeopleR :: Handler Html
getPeopleR = getNewHomeR


-- | Information of single person, taking intel level into account
getApiPersonR :: Key Person -> HandlerFor App Value
getApiPersonR pId = do
    (_, _, avatar, _) <- apiRequireFaction
    today <- runDB $ starDate
    person <- runDB $ selectFirst [ PersonId ==. pId ] []
    when (isNothing person) (apiNotFound)
    intel <- runDB $ selectList [ HumanIntelligencePersonId ==. pId
                                , HumanIntelligenceOwnerId ==. entityKey avatar
                                ] []
    let report = personReport <$> Just today
                              <*> person
                              <*> Just ((humanIntelligenceLevel . entityVal) <$> intel)
    return $ toJSON report


-- | Demesne of given character, according to intelligence level
getApiDemesneR :: Key Person -> HandlerFor App Value
getApiDemesneR pId = do
    (_, _, avatar, _) <- apiRequireFaction
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
