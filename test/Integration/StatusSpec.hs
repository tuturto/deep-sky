{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Integration.StatusSpec ( spec )
    where

import TestImport
import CustomTypes ( SystemStatus(..) )
import Simulation.Status ( removeExpiredStatuses )
import Space.Data ( PlanetaryStatus(..) )


spec :: Spec
spec = withApp $ do

    describe "Status handling"  $ do
        describe "Planet statuses"  $ do
            it "Expired planet statuses are removed and news created" $ do


                sId <- runDB $ insert $ StarSystem
                        { starSystemName = "Aldebaraan"
                        , starSystemCoordX = 10
                        , starSystemCoordY = 20
                        , starSystemRulerId = Nothing
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
                        , planetRulerId = Nothing
                        }

                _ <- runDB $ insert $ PlanetStatus
                        { planetStatusPlanetId = pId1
                        , planetStatusStatus = GoodHarvest
                        , planetStatusExpiration = Just 20201
                        }

                let status = Simulation 20201 Online
                _ <- runDB $ insert status

                news <- runDB $ removeExpiredStatuses (simulationCurrentTime status)

                statuses <- runDB $ selectList [ PlanetStatusPlanetId ==. pId1 ] []
                loadedNews <- runDB $ selectList [] [ Asc NewsDate ]

                liftIO $ statuses `shouldSatisfy` (\x -> length x == 0)
                liftIO $ news `shouldSatisfy` (\x -> length x == 1)
                liftIO $ loadedNews `shouldSatisfy` (\x -> length x == 1)
