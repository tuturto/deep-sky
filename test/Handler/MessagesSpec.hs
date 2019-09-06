{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.MessagesSpec (spec)
    where

import TestImport
import Control.Lens ( (^?), _Just, _head )
import Data.Aeson ( Value(..), decode )
import Data.Aeson.Lens ( _Array, _String, _Integer, key )
import Database.Persist.Sql (toSqlKey)
import Network.Wai.Test ( SResponse(..) )

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
            resp <- getResponse
            let jsonM = join (decode <$> simpleBody <$> resp) :: Maybe Value

            assertEq "message tag"
                     (jsonM ^? (_Just . _Array . _head . key "tag" . _String))
                     (Just "ResearchCompleted")
            assertEq "star date"
                     (jsonM ^? (_Just . _Array . _head . key "starDate" . _Integer))
                     (Just 25250)
            assertEq "technology"
                     (jsonM ^? (_Just . _Array . _head . key "contents" . key "Technology" . _String))
                     (Just "HighSensitivitySensors")

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
