{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Research.Import ( availableForResearch, isAvailable, researchOutput, researchReady
                       , antecedentsAvailable, currentResearchProgressL, currentResearchTypeL )
    where


import Import
import Control.Lens ( Lens', lens )
import CustomTypes ( BuildingType(..) )
import Research.Data ( TechTree(..), Research(..), TotalResearchScore(..)
                     , ResearchProduction(..), ResearchScore(..), ResearchCategory(..)
                     , Technology(..) )
import Research.Tree ( techMap )


-- | All research that has not yet been done, but is available based
-- on the given tech tree
availableForResearch :: TechTree -> [CompletedResearch] -> [Research]
availableForResearch tree completed =
    filter antecedentsDone (unTechTree tree)
    where
        completedTech = fmap completedResearchType completed
        antecedentsDone tech = isAvailable completed tech
                            && notElem (researchType tech) completedTech


-- | is given research available based on completed research
isAvailable :: [CompletedResearch] -> Research -> Bool
isAvailable completed research =
    all inCompleted $ researchAntecedents research
    where
        completedTech = fmap completedResearchType completed
        inCompleted x = x `elem` completedTech


-- | Research output of a building, takes account type and level
-- of the building.
researchOutput :: Building -> TotalResearchScore ResearchProduction
researchOutput Building { buildingType = ResearchComplex } =
    TotalResearchScore
    { totalResearchScoreEngineering = ResearchScore 10
    , totalResearchScoreNatural = ResearchScore 10
    , totalResearchScoreSocial = ResearchScore 10
    }

researchOutput Building { buildingType = ParticleAccelerator } =
    TotalResearchScore
    { totalResearchScoreEngineering = ResearchScore 15
    , totalResearchScoreNatural = ResearchScore 15
    , totalResearchScoreSocial = ResearchScore 0
    }

researchOutput _ = mempty


-- | Is current research completed
researchReady :: CurrentResearch -> Bool
researchReady r =
    case researchCategory research of
        Engineering _ ->
            completed (totalResearchScoreEngineering cost) (currentResearchProgress r)

        NaturalScience _ ->
            completed (totalResearchScoreNatural cost) (currentResearchProgress r)

        SocialScience _ ->
            completed (totalResearchScoreSocial cost) (currentResearchProgress r)
    where
        research = techMap $ currentResearchType r
        cost = researchCost research
        completed c done = unResearchScore c <= done


-- | Is given list of known technology enough to satisfy antecedents of research
antecedentsAvailable :: [Technology] -> Research -> Bool
antecedentsAvailable known research =
    all (`elem` known) $ researchAntecedents research


-- Lens for accessing research progress of current research
currentResearchProgressL :: Lens' CurrentResearch Int
currentResearchProgressL = lens currentResearchProgress (\r v -> r { currentResearchProgress = v})


-- | Lens for accessing type of current research
currentResearchTypeL :: Lens' CurrentResearch Technology
currentResearchTypeL = lens currentResearchType (\r v -> r { currentResearchType = v})
