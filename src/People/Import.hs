{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module People.Import
    ( personReport, PersonReport(..), StatReport(..) )
    where

import Import
import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import CustomTypes ( StarDate, Age, age )
import People.Data ( PersonIntel(..), Diplomacy, Martial, Stewardship
                   , Intrique, Learning, StatScore, PersonName, Sex
                   , Gender )


data PersonReport = PersonReport
    { personReportId :: Key Person
    , personReportName :: PersonName
    , personReportSex :: Sex
    , personReportGender :: Gender
    , personReportAge :: Age
    , personReportStats :: Maybe StatReport
    }
    deriving (Show, Read, Eq)


data StatReport = StatReport
    { statReportDiplomacy :: StatScore Diplomacy
    , statReportMartial :: StatScore Martial
    , statReportStewardship :: StatScore Stewardship
    , statReportIntrique :: StatScore Intrique
    , statReportLearning :: StatScore Learning
    }
    deriving (Show, Read, Eq)


-- | Person report of given person and taking HUMINT level into account
personReport :: StarDate -> Entity Person -> [PersonIntel] -> PersonReport
personReport today personE intel =
    PersonReport { personReportId = entityKey personE
                 , personReportName = personName person
                 , personReportSex = personSex person
                 , personReportGender = personGender person
                 , personReportAge = age (personDateOfBirth person) today
                 , personReportStats = statReport person intel
                 }
    where
        person = entityVal personE


-- | Stat report of given person and taking HUMINT level into account
-- if Stats level isn't available, no report is given
-- if Stats level is available, full report is given without errors
statReport :: Person -> [PersonIntel] -> Maybe StatReport
statReport person intel =
    if available
        then Just StatReport { statReportDiplomacy = personDiplomacy person
                             , statReportMartial = personMartial person
                             , statReportStewardship = personStewardship person
                             , statReportIntrique = personIntrique person
                             , statReportLearning = personLearning person
                             }
        else Nothing
    where
        available = any (\x -> x == Stats) intel

$(deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''PersonReport)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''StatReport)
