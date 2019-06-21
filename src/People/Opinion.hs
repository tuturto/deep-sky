{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module People.Opinion
    ( OpinionReport(..), OpinionFeeling(..), OpinionReason(..)
    , OpinionScore(..), ReportResult(..), opinionReport, scoreFeeling
    , traitScore, traitPairOpinion, relationTypeOpinion
    , reportResultToOpinionResult )
where

import Import
import Data.Aeson ( Object, withScientific, withText, withObject )
import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import Data.Aeson.Types ( Parser )
import Data.Scientific ( toBoundedInteger )
import Common ( mkUniq, safeHead, clamp )
import People.Data ( PersonIntel(..), TraitType(..), RelationType(..)
                   , RelationVisibility(..), OpinionIntel(..)
                   , opinionIntelVisibility )


-- | Originator's opinion of the target
-- Opinion depends on the traits of both persons
-- Level of detail of given report depends on various things, like type of intel
-- available that is available about both persons
opinionReport :: [TraitType]
    -> [PersonIntel]
    -> [TraitType]
    -> [PersonIntel]
    -> [Relation]
    -> OpinionReport
opinionReport originatorTraits originatorIntel targetTraits targetIntel targetRelations =
    reportResultToOpinionResult $ traitsRep <> relationsRep
    where
        traitsRep = traitScore originatorTraits originatorIntel targetTraits targetIntel
        relationsRep = relationScore originatorIntel targetRelations


-- | Mapping between opinion score and opinion feeling
scoreFeeling :: OpinionScore -> OpinionFeeling
scoreFeeling score
    | score < -15 = NegativeFeeling
    | score <= 15 = NeutralFeeling
    | otherwise = PositiveFeeling


-- | Opinion score based on two trait type lists
-- Reasons for opinion are given only if both intels include Traits
traitScore :: [TraitType] -> [PersonIntel] -> [TraitType] -> [PersonIntel] -> ReportResult
traitScore originatorTraits originatorIntel targetTraits targetIntel =
    if (Traits `elem` originatorIntel) && (Traits `elem` targetIntel)
        then DetailedLevel score reasons
        else FeelingLevel score
    where
        (score, reasons) = mconcat $ traitPairScore <$> originatorTraits <*> targetTraits


-- | Helper function that turns Maybe (OpinionScore, OpinionReason) into (OpinionScore, [OpinionReason])
-- the latter form has advantage of being a monoid and as a such it can be used to combine multiple
-- values into a single one
traitPairScore :: TraitType -> TraitType -> (OpinionScore, [OpinionReason])
traitPairScore a b =
    case traitPairOpinion a b of
            Nothing ->
                mempty

            Just (s, r) ->
                (s, [r])


-- | Internal type used to keep track of accumulated opinion score and results
-- since there's semigroup and monoid instances defined for this, multiple ones can
-- be combined together. While combining, opinion scores are added and reason lists
-- concatenated.
-- There exists hiearchy in results, feeling level is the lowest, while detailed level
-- is the highest. When combining results from different levels, lowest level defines
-- end result level. In other words, combining detailed level with feeling level will
-- result to feeling level.
data ReportResult =
    FeelingLevel OpinionScore
    | ReasonsLevel OpinionScore [OpinionReason]
    | DetailedLevel OpinionScore [OpinionReason]
    deriving (Show, Read, Eq)


instance Semigroup ReportResult where
    (FeelingLevel s1) <> (FeelingLevel s2) = FeelingLevel (s1 <> s2)
    (FeelingLevel s1) <> (ReasonsLevel s2 _) = FeelingLevel (s1 <> s2)
    (FeelingLevel s1) <> (DetailedLevel s2 _) = FeelingLevel (s1 <> s2)
    (ReasonsLevel s1 _) <> (FeelingLevel s2) = FeelingLevel (s1 <> s2)
    (ReasonsLevel s1 r1) <> (ReasonsLevel s2 r2) = ReasonsLevel (s1 <> s2) (r1 <> r2)
    (ReasonsLevel s1 r1) <> (DetailedLevel s2 r2) = ReasonsLevel (s1 <> s2) (r1 <> r2)
    (DetailedLevel s1 _) <> (FeelingLevel s2) = FeelingLevel (s1 <> s2)
    (DetailedLevel s1 r1) <> (ReasonsLevel s2 r2) = ReasonsLevel (s1 <> s2) (r1 <> r2)
    (DetailedLevel s1 r1) <> (DetailedLevel s2 r2) = DetailedLevel (s1 <> s2) (r1 <> r2)


instance Monoid ReportResult where
    mempty = DetailedLevel mempty mempty


-- | Mapping between report result and opinion report
reportResultToOpinionResult :: ReportResult -> OpinionReport
reportResultToOpinionResult (FeelingLevel score) =
    BaseOpinionReport $ scoreFeeling score

reportResultToOpinionResult (ReasonsLevel score reasons) =
    OpinionReasonReport (scoreFeeling score) reasons

reportResultToOpinionResult (DetailedLevel score reasons) =
    DetailedOpinionReport (clamp (-100) 100 score) reasons


-- | Opinion score and reason for a pair of traits
-- In case there's no particular opinion, Nothing is returned
traitPairOpinion :: TraitType -> TraitType -> Maybe (OpinionScore, OpinionReason)
traitPairOpinion Brave Brave = Just (10, "Both brave")
traitPairOpinion Brave Coward = Just (-10, "Brave vs. coward")
traitPairOpinion Brave _ = Nothing
traitPairOpinion Coward Coward = Just (5, "Both coward")
traitPairOpinion Coward Brave = Just (-10, "Coward vs. brave")
traitPairOpinion Coward _ = Nothing
traitPairOpinion Chaste Chaste = Just (10, "Both chaste")
traitPairOpinion Chaste Lustful = Just (-10, "Chaste vs. lustful")
traitPairOpinion Chaste _ = Nothing
traitPairOpinion Temperate Temperate = Just (10, "Both temperate")
traitPairOpinion Temperate Gluttonous = Just (-10, "Temperate vs. gluttonous")
traitPairOpinion Temperate _ = Nothing
traitPairOpinion Charitable Charitable = Just (10, "Both charitable")
traitPairOpinion Charitable Greedy = Just (-10, "Charitable vs. greedy")
traitPairOpinion Charitable _ = Nothing
traitPairOpinion Diligent Diligent = Just (10, "Both diligent")
traitPairOpinion Diligent Slothful = Just (-10, "Diligent vs. slothful")
traitPairOpinion Diligent _ = Nothing
traitPairOpinion Patient Patient = Just (10, "Both patient")
traitPairOpinion Patient Wroth = Just (-10, "Patient vs. wroth")
traitPairOpinion Patient _ = Nothing
traitPairOpinion Kind Kind = Just (10, "Both kind")
traitPairOpinion Kind Cruel = Just (-10, "Kind vs. cruel")
traitPairOpinion Kind _ = Nothing
traitPairOpinion Humble Humble = Just (10, "Both humble")
traitPairOpinion Humble Proud = Just (-10, "Humble vs. proud")
traitPairOpinion Humble _ = Nothing
traitPairOpinion Lustful Lustful = Just (15, "Both lustful")
traitPairOpinion Lustful Chaste = Just (-10, "Lustful vs. chaste")
traitPairOpinion Lustful _ = Nothing
traitPairOpinion Gluttonous Gluttonous = Just (10, "Both gluttonous")
traitPairOpinion Gluttonous Temperate = Just (-10, "Gluttonous vs. temperate")
traitPairOpinion Gluttonous _ = Nothing
traitPairOpinion Greedy Greedy = Just (5, "Both greedy")
traitPairOpinion Greedy Charitable = Just (-10, "Greedy vs. charitable")
traitPairOpinion Greedy _ = Nothing
traitPairOpinion Slothful Slothful = Just (10, "Both slothful")
traitPairOpinion Slothful Diligent = Just (-10, "Slothful vs. diligent")
traitPairOpinion Slothful _ = Nothing
traitPairOpinion Wroth Wroth = Just (10, "Both wroth")
traitPairOpinion Wroth Patient = Just (-10, "Wroth vs. patient")
traitPairOpinion Wroth _ = Nothing
traitPairOpinion Envious Envious = Just (5, "Both envious")
traitPairOpinion Envious _ = Nothing
traitPairOpinion Proud Proud = Just (10, "Both proud")
traitPairOpinion Proud Humble = Just (-10, "Proud vs. humble")
traitPairOpinion Proud _ = Nothing
traitPairOpinion Ambitious Ambitious = Just (-25, "Both ambitious")
traitPairOpinion Ambitious Content = Just (-5, "Ambitious vs. content")
traitPairOpinion Ambitious _ = Nothing
traitPairOpinion Content Content = Just (15, "Both content")
traitPairOpinion Content _ = Nothing
traitPairOpinion Cruel Cruel = Just (10, "Both cruel")
traitPairOpinion Cruel Kind = Just (-10, "Cruel vs. kind")
traitPairOpinion Cruel _ = Nothing
traitPairOpinion Cynical Cynical = Just (5, "Both cynical")
traitPairOpinion Cynical _ = Nothing
traitPairOpinion Deceitful Deceitful = Just (5, "Both deceitful")
traitPairOpinion Deceitful Honest = Just (-10, "Deceitful vs. honest")
traitPairOpinion Deceitful _ = Nothing
traitPairOpinion Honest Honest = Just (10, "Both honest")
traitPairOpinion Honest Deceitful = Just (-10, "Honest vs. deceitful")
traitPairOpinion Honest _ = Nothing
traitPairOpinion Shy Shy = Just (10, "Both shy")
traitPairOpinion Shy _ = Nothing


relationScore :: [PersonIntel] -> [Relation] -> ReportResult
relationScore intel relations =
    mconcat $ (relReport oIntel score) <$> visibilities
    where
        score = mconcat $ (relationTypeScore . relationType) <$> relations
        visibilities = mkUniq $ relationVisibility <$> relations
        oIntel = mkUniq $ mapMaybe (\case
                                        Opinions x ->
                                            Just x

                                        _ ->
                                            Nothing)
                                   intel


-- | turn opinion score and reasons into report result based on available
-- opinion intel and current relation visibility.
-- This is meant to be used for post processing opinion result based on
-- relationship types between people
relReport :: [OpinionIntel]
    -> (OpinionScore, [OpinionReason])
    -> RelationVisibility
    -> ReportResult
relReport intel (score, reasons) visibility =
    case matching of
        Nothing ->
            FeelingLevel score

        Just (BaseOpinionIntel _) ->
            FeelingLevel score

        Just (ReasonsForOpinions _) ->
            ReasonsLevel score reasons

        Just (DetailedOpinions _) ->
            DetailedLevel score reasons
    where
        -- TODO: test that sort sorts in the way we expect it to sort
        matching = safeHead $ reverse $ sort $ filter (\x -> opinionIntelVisibility x == visibility) intel


-- | Opinion score and reason in case where target is of given relation to a person
relationTypeOpinion :: RelationType -> Maybe (OpinionScore, OpinionReason)
relationTypeOpinion Parent = Just (10, "Parent")
relationTypeOpinion Child = Just (10, "Child")
relationTypeOpinion Sibling = Just (5, "Sibling")
relationTypeOpinion StepParent = Just (10, "Step parent")
relationTypeOpinion StepChild = Just (10, "Step child")
relationTypeOpinion StepSibling = Just (5, "Step sibling")
relationTypeOpinion Betrothed = Nothing
relationTypeOpinion Spouse = Just (15, "Spouse")
relationTypeOpinion ExSpouse = Just (-15, "Ex-spouse")
relationTypeOpinion Lover = Just (50, "Lover")
relationTypeOpinion ExLover = Just (-50, "Ex-lover")
relationTypeOpinion Friend = Just (75, "Friend")
relationTypeOpinion Rival = Just (-100, "Rival")


-- deduplicate
relationTypeScore :: RelationType -> (OpinionScore, [OpinionReason])
relationTypeScore rType =
    case relationTypeOpinion rType of
            Nothing ->
                mempty

            Just (s, r) ->
                (s, [r])


data OpinionReport =
    BaseOpinionReport OpinionFeeling
    | OpinionReasonReport OpinionFeeling [OpinionReason]
    | DetailedOpinionReport OpinionScore [OpinionReason]
    deriving (Show, Read, Eq)


instance ToJSON OpinionReport where
    toJSON (BaseOpinionReport feeling) =
        object [ "Tag" .= ("BaseOpinionReport" :: Text)
               , "Feeling" .= toJSON feeling
               ]

    toJSON (OpinionReasonReport feeling reasons) =
        object [ "Tag" .= ("OpinionReasonReport" :: Text)
               , "Feeling" .= toJSON feeling
               , "Reasons" .= toJSON reasons
               ]

    toJSON (DetailedOpinionReport score reasons) =
        object [ "Tag" .= ("DetailedOpinionReport" :: Text)
               , "Score" .= toJSON score
               , "Reasons" .= toJSON reasons
               ]


instance FromJSON OpinionReport where
    parseJSON = withObject "opinion report" $ \o -> do
        tag <- o .: "Tag"
        item <- parseOpinionReport tag o
        return item


parseOpinionReport :: Text -> Object -> Parser OpinionReport
parseOpinionReport "BaseOpinionReport" o = do
    feeling <- o .: "Feeling"
    return $ BaseOpinionReport feeling

parseOpinionReport "OpinionReasonReport" o = do
    feeling <- o .: "Feeling"
    reasons <- o .: "Reasons"
    return $ OpinionReasonReport feeling reasons

parseOpinionReport "DetailedOpinionReport" o = do
    score <- o .: "Score"
    reasons <- o .: "Reasons"
    return $ DetailedOpinionReport score reasons

parseOpinionReport _ _ =
    mempty


data OpinionFeeling =
    PositiveFeeling
    | NeutralFeeling
    | NegativeFeeling
    deriving (Show, Read, Eq)


newtype OpinionReason = OpinionReason { unOpinionReason :: Text }
    deriving (Show, Read, Eq)


instance IsString OpinionReason where
    fromString = OpinionReason . fromString


instance ToJSON OpinionReason where
    toJSON = toJSON . unOpinionReason


instance FromJSON OpinionReason where
    parseJSON =
        withText "opinion reason"
            (\x -> return $ OpinionReason x)


newtype OpinionScore = OpinionScore { unOpinionScore :: Int }
    deriving (Show, Read, Eq, Ord, Num)


instance Semigroup OpinionScore where
    (<>) a b = a + b


instance Monoid OpinionScore where
    mempty = 0


instance ToJSON OpinionScore where
    toJSON = toJSON . unOpinionScore


instance FromJSON OpinionScore where
    parseJSON =
        withScientific "opinion score"
            (\x -> case toBoundedInteger x of
                Nothing ->
                    return $ OpinionScore 0

                Just n ->
                    return $ OpinionScore n)


$(deriveJSON defaultOptions { fieldLabelModifier = drop 14 } ''OpinionFeeling)
