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
import Common ( apiRequireFaction, apiNotFound, mkUniq )
import MenuHelpers ( starDate )
import People.Import ( personReport, demesneReport )
import People.Queries ( getPersonLocation, PersonLocationSum(..))
import Handler.Home ( getNewHomeR )
import Queries ( personRelations, PersonRelationData(..) )


-- | serve client program and have it started showing person details
getPersonR :: Key Person -> Handler Html
getPersonR _ = getNewHomeR


-- | serve client program and have it started showing database
getPeopleR :: Handler Html
getPeopleR = getNewHomeR


-- | Information of single person, taking intel level into account
getApiPersonR :: Key Person -> HandlerFor App Value
getApiPersonR pId = do
    (_, _, avatar, fId) <- apiRequireFaction
    let avatarId = entityKey avatar
    today <- runDB $ starDate
    info <- runDB $ personRelations pId (entityKey avatar)
    when (isNothing info) apiNotFound
    let dId = join $ (personDynastyId . entityVal . personRelationDataPerson) <$> info
    dynasty <- runDB $ mapM getEntity dId
    intel <- runDB $ selectList [ HumanIntelligencePersonId ==. pId
                                , HumanIntelligenceOwnerId ==. avatarId ] []
    let intelTypes = mkUniq $ (humanIntelligenceLevel . entityVal) <$> intel
    targetRelations <- runDB $ selectList [ RelationOriginatorId ==. pId ] []
    let pIds = pId : avatarId : (mkUniq $ fmap (relationTargetId . entityVal) targetRelations)
    allTraits <- runDB $ selectList ( [PersonTraitPersonId <-. pIds] ++
                                      ( [PersonTraitValidUntil <=. (Just today)]
                                        ||. [PersonTraitValidUntil ==. Nothing])
                                    ) []
    locationDb <- runDB $ getPersonLocation fId pId
    let location = case locationDb of
                    Nothing ->
                        UnknownLocation

                    Just l ->
                        l

    let report = personReport today
                              (fromJust info)
                              (join dynasty)
                              (entityVal <$> allTraits)
                              intelTypes
                              (entityVal <$> targetRelations)
                              location
                              avatarId
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
