module QC.Generators.Vehicles ( positiveCrewRequirements, positiveCrewRequirement )
    where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances()

import Vehicles.Data ( CrewPosition(..), CrewRank(..), CrewAmount(..) )
import Vehicles.Stats ( CrewRequirement(..) )


positiveCrewRequirement :: CrewPosition -> Gen CrewRequirement
positiveCrewRequirement position = do
    amount <- arbitrary `suchThat` \x -> x > 0
    return $ CrewRequirement position SecondClass (CrewAmount amount)


positiveCrewRequirements :: Gen [CrewRequirement]
positiveCrewRequirements = do
    let positions = [minBound..]
    mapM positiveCrewRequirement positions
