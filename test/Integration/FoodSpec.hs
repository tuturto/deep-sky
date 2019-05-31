{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Integration.FoodSpec ( spec )
    where

import TestImport
import CustomTypes ( PlanetaryStatus(..), BuildingType(..) )
import Simulation.Food ( getFoodProduction )


spec :: Spec
spec = withApp $ do

    describe "Food production"  $ do
        it "Good harvest boosts production" $ do
            sId <- runDB $ insert $ StarSystem
                    { starSystemName = "Aldebaraan"
                    , starSystemCoordX = 10
                    , starSystemCoordY = 20
                    , starSystemRulerId = Nothing
                    }

            pId1 <- runDB $ insert $ Planet
                    { planetName = "New Earth"
                    , planetPosition = 3
                    , planetStarSystemId = sId
                    , planetOwnerId = Nothing
                    , planetGravity = 1.0
                    , planetRulerId = Nothing
                     }

            pId2 <- runDB $ insert $ Planet
                    { planetName = "New Mars"
                    , planetPosition = 4
                    , planetStarSystemId = sId
                    , planetOwnerId = Nothing
                    , planetGravity = 1.0
                    , planetRulerId = Nothing
                    }

            _ <- runDB $ insert $ Building
                    { buildingPlanetId = pId1
                    , buildingType = Farm
                    , buildingLevel = 1
                    , buildingDamage = 0.0
                    }

            _ <- runDB $ insert $ Building
                    { buildingPlanetId = pId2
                    , buildingType = Farm
                    , buildingLevel = 1
                    , buildingDamage = 0.0
                    }

            _ <- runDB $ insert $ PlanetStatus
                    { planetStatusPlanetId = pId1
                    , planetStatusStatus = GoodHarvest
                    , planetStatusExpiration = Nothing
                    }

            production1 <- runDB $ getFoodProduction pId1
            production2 <- runDB $ getFoodProduction pId2

            liftIO $ production1 `shouldSatisfy` (> production2)
