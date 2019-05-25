{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.People
    ( getApiPersonR, getPersonR, getPeopleR )
    where

import Import
import Common ( apiRequireFaction )
import MenuHelpers ( starDate )
import People.Import ( personReport )
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
    loaded <- runDB $ get pId
    intel <- runDB $ selectList [ HumanIntelligencePersonId ==. pId
                                , HumanIntelligenceOwnerId ==. entityKey avatar
                                ] []
    let report = personReport <$> Just today
                              <*> loaded
                              <*> Just ((humanIntelligenceLevel . entityVal) <$> intel)
    return $ toJSON report
