{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Research.Import ( availableForResearch, isAvailable, researchOutput, researchReady
                       , antecedentsAvailable )
    where


import Import
import qualified Data.Map.Strict as Map
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

researchOutput _ =
    TotalResearchScore
    { totalResearchScoreEngineering = ResearchScore 0
    , totalResearchScoreNatural = ResearchScore 0
    , totalResearchScoreSocial = ResearchScore 0
    }


-- | Is current research completed
researchReady :: CurrentResearch -> Bool
researchReady r =
    case researchCategory <$> research of
        Just (Engineering _) ->
            completed (fmap totalResearchScoreEngineering cost) (currentResearchProgress r)

        Just (NaturalScience _) ->
            completed (fmap totalResearchScoreNatural cost) (currentResearchProgress r)

        Just (SocialScience _) ->
            completed (fmap totalResearchScoreSocial cost) (currentResearchProgress r)

        Nothing ->
            False

    where
        research = Map.lookup (currentResearchType r) techMap
        cost = researchCost <$> research
        completed c done =
            case c of
                Nothing ->
                    False

                Just techCost ->
                    unResearchScore techCost <= done


-- | Is given list of known technology enough to satisfy antecedents of research
antecedentsAvailable :: [Technology] -> Research -> Bool
antecedentsAvailable known research =
    all (`elem` known) $ researchAntecedents research
