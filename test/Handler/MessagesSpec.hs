{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.MessagesSpec (spec)
    where

import TestImport
import Database.Persist.Sql (toSqlKey)

import Handler.Helpers ( delete_, setupPerson )
import News.Import ( researchCompleted )
import Research.Data ( Technology(..) )


spec :: Spec
spec = withApp $ do
    describe "Message handling" $ do
        it "unauthenticated user can't access messages" $ do
            _ <- get ApiMessageR
            statusIs 401

        it "pending messages are loaded" $ do
            (pId, fId) <- setupPerson
            _ <- runDB $ insert $ researchCompleted 25250 fId HighSensitivitySensors
            user <- createUser "Pete" (Just pId)
            authenticateAs user
            _ <- get ApiMessageR
            bodyContains "ResearchCompleted"
            statusIs 200

        it "message marked deleted isn't loaded" $ do
            (pId, fId) <- setupPerson
            _ <- runDB $ insert $ researchCompleted 25250 fId HighSensitivitySensors
            user <- createUser "Pete" (Just pId)
            authenticateAs user
            _ <- delete_ $ ApiMessageIdR (toSqlKey 1)
            _ <- get ApiMessageR
            bodyNotContains "ResearchCompleted"
            statusIs 200
