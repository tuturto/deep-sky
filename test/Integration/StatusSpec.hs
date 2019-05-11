{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Integration.StatusSpec ( spec )
    where

import TestImport
import CustomTypes ( PlanetaryStatus(..) )
import Simulation.Status ( removeExpiredStatuses )


spec :: Spec
spec = withApp $ do

    describe "Status handling"  $ do
        describe "Planet statuses"  $ do
            it "Expired planet statuses are removed and news created" $ do
                sId <- runDB $ insert $ StarSystem
                        { starSystemName = "Aldebaraan"
                        , starSystemCoordX = 10
                        , starSystemCoordY = 20
                        }

                fId <- runDB $ insert $ Faction
                        { factionName = "Star lords"
                        , factionHomeSystem = sId
                        , factionBiologicals = 10
                        , factionMechanicals = 10
                        , factionChemicals = 10
                        }

                pId1 <- runDB $ insert $ Planet
                        { planetName = "New Earth"
                        , planetPosition = 3
                        , planetStarSystemId = sId
                        , planetOwnerId = Just fId
                        , planetGravity = 1.0
                        }

                _ <- runDB $ insert $ PlanetStatus
                        { planetStatusPlanetId = pId1
                        , planetStatusStatus = GoodHarvest
                        , planetStatusExpiration = Just 20201
                        }

                let time = Time 20201
                _ <- runDB $ insert time

                news <- runDB $ removeExpiredStatuses time

                statuses <- runDB $ selectList [ PlanetStatusPlanetId ==. pId1 ] []
                loadedNews <- runDB $ selectList [] [ Asc NewsDate ]

                liftIO $ statuses `shouldSatisfy` (\x -> length x == 0)
                liftIO $ news `shouldSatisfy` (\x -> length x == 1)
                liftIO $ loadedNews `shouldSatisfy` (\x -> length x == 1)