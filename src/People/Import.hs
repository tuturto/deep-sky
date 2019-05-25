{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module People.Import
    ( personReport )
    where

import Import
import CustomTypes ( StarDate, age )
import People.Data ( PersonReport(..), StatReport(..), PersonIntel(..) )


-- | Person report of given person and taking HUMINT level into account
personReport :: StarDate -> Person -> [PersonIntel] -> PersonReport
personReport today person intel =
    PersonReport { personReportName = personName person
                 , personReportSex = personSex person
                 , personReportGender = personGender person
                 , personReportAge = age (personDateOfBirth person) today
                 , personReportStats = statReport person intel
                 }


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
