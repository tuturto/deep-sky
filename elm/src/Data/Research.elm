module Data.Research exposing
    ( CurrentResearch
    , EngineeringSubField(..)
    , NaturalScienceSubField(..)
    , Research
    , ResearchCategory(..)
    , ResearchScore(..)
    , ResearchTier(..)
    , SocialScienceSubField(..)
    , Technology(..)
    , TopResearchCategory(..)
    , TotalResearchScore
    , researchToCurrent
    , sameTopCategory
    , topResearchCategory
    , topResearchCategoryToString
    , topResearchCategoryToTag
    , unResearchScore
    , unResearchTier
    , unTechnology
    )


type ResearchCategory
    = Engineering EngineeringSubField
    | NaturalScience NaturalScienceSubField
    | SocialScience SocialScienceSubField


type TopResearchCategory
    = Eng
    | NatSci
    | SocSci


topResearchCategoryToString : TopResearchCategory -> String
topResearchCategoryToString cat =
    case cat of
        Eng ->
            "engineering"

        NatSci ->
            "natural science"

        SocSci ->
            "social science"


topResearchCategoryToTag : TopResearchCategory -> String
topResearchCategoryToTag cat =
    case cat of
        Eng ->
            "Eng"

        NatSci ->
            "NatSci"

        SocSci ->
            "SocSci"


topResearchCategory : ResearchCategory -> TopResearchCategory
topResearchCategory cat =
    case cat of
        Engineering _ ->
            Eng

        NaturalScience _ ->
            NatSci

        SocialScience _ ->
            SocSci


sameTopCategory : ResearchCategory -> ResearchCategory -> Bool
sameTopCategory a b =
    topResearchCategory a == topResearchCategory b


type EngineeringSubField
    = Industry
    | Materials
    | Propulsion
    | FieldManipulation


type NaturalScienceSubField
    = Computing
    | EnergyManipulation
    | Particles
    | Biology


type SocialScienceSubField
    = MilitaryTheory
    | Statecraft
    | ShadowOps


type Technology
    = Technology String


unTechnology : Technology -> String
unTechnology (Technology s) =
    s


type ResearchTier
    = ResearchTier Int


unResearchTier : ResearchTier -> Int
unResearchTier (ResearchTier n) =
    n


type ResearchScore
    = ResearchScore Int


unResearchScore : ResearchScore -> Int
unResearchScore (ResearchScore n) =
    n


type alias TotalResearchScore =
    { engineering : ResearchScore
    , natural : ResearchScore
    , social : ResearchScore
    }


type alias Research =
    { name : String
    , researchType : Technology
    , category : ResearchCategory
    , antecedents : List Technology
    , cost : TotalResearchScore
    , tier : ResearchTier
    }


type alias CurrentResearch =
    { research : Research
    , progress : ResearchScore
    }


researchToCurrent : Research -> CurrentResearch
researchToCurrent res =
    { research = res
    , progress = ResearchScore 0
    }
