{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Helpers ( delete_, setupPerson )
    where

import TestImport

import People.Data ( Sex(..), Gender(..), PersonName(..), FirstName(..) )

-- | Perform delete request
delete_ :: Route App -> YesodExample App ()
delete_ = performMethod "DELETE"


-- | Insert faction and person into database and return their primary keys as tuple
setupPerson :: SIO (YesodExampleData App) (Key Person, Key Faction)
setupPerson = do
    sId <- runDB $ insert $ StarSystem { starSystemName = "Star system"
                                       , starSystemCoordX = 0.0
                                       , starSystemCoordY = 0.0
                                       , starSystemRulerId = Nothing
                                       }
    fId <- runDB $ insert $ Faction { factionName = "faction"
                                    , factionHomeSystem = sId
                                    , factionBiologicals = 100
                                    , factionMechanicals = 100
                                    , factionChemicals = 100
                                    }
    pId <- runDB $ insert $ Person { personName = SimpleName (FirstName "Avatar") Nothing
                                   , personSex = Female
                                   , personGender = Woman
                                   , personDateOfBirth = 20120
                                   , personDiplomacy = 10
                                   , personMartial = 10
                                   , personStewardship = 10
                                   , personIntrique = 10
                                   , personLearning = 10
                                   , personFactionId = Just fId
                                   , personPlanetTitle = Nothing
                                   , personStarSystemTitle = Nothing
                                   , personDynastyId = Nothing
                                   }
    return (pId, fId)
