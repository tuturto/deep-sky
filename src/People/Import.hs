{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module People.Import
    ( PersonReport(..), StatReport(..), RelationLink(..), TraitReport(..)
    , TraitDescription(..), TraitName(..), personReport, demesneReport
    , shortTitle, longTitle, relationsReport, knownLink, traitName
    , traitDescription, relationOriginatorIdL, relationTargetIdL, relationTypeL
    , flipRelation, flipRelationType, humanIntelligenceLevelL
    )
    where

import Import
import qualified Prelude as P
import Control.Lens ( Lens', lens, (&), (.~), (%~), traverse, _Just, (^..) )
import Data.Aeson ( withText )
import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import Data.List ( nub )
import Common ( mkUniq )
import CustomTypes ( StarDate, Age, age )
import People.Data ( PersonIntel(..), Diplomacy, Martial, Stewardship
                   , Intrique, Learning, StatScore, PersonName, Sex
                   , Gender, DemesneName(..), ShortTitle(..), LongTitle(..)
                   , RelationVisibility(..), RelationType(..), DynastyName(..)
                   , TraitType(..)
                   )
import People.Opinion ( OpinionReport, opinionReport )
import Queries ( PersonRelationData(..), PersonDataLink(..)
               , personDataLinkOriginatorIntelligenceL
               , personDataLinkTargetIntelligenceL )


data PersonReport = PersonReport
    { personReportId :: !(Key Person)
    , personReportName :: !PersonName
    , personReportShortTitle :: !(Maybe ShortTitle)
    , personReportLongTitle :: !(Maybe LongTitle)
    , personReportSex :: !Sex
    , personReportGender :: !Gender
    , personReportAge :: !Age
    , personReportStats :: !(Maybe StatReport)
    , personReportRelations :: ![RelationLink]
    , personReportIntelTypes :: ![PersonIntel]
    , personReportDynasty :: !(Maybe DynastyReport)
    , personReportAvatarOpinion :: !OpinionReport
    , personReportOpinionOfAvatar :: !OpinionReport
    , personReportTraits :: !(Maybe [TraitReport])
    , personReportAvatar :: Bool
    } deriving (Show, Read, Eq)


data StatReport = StatReport
    { statReportDiplomacy :: !(StatScore Diplomacy)
    , statReportMartial :: !(StatScore Martial)
    , statReportStewardship :: !(StatScore Stewardship)
    , statReportIntrique :: !(StatScore Intrique)
    , statReportLearning :: !(StatScore Learning)
    } deriving (Show, Read, Eq)


data DemesneReport =
    PlanetDemesne PlanetDemesneReport
    | StarSystemDemesne StarSystemDemesneReport
    deriving (Show, Read, Eq)


data PlanetDemesneReport = PlanetDemesneReport
    { planetDemesneReportPlanetId :: !(Key Planet)
    , planetDemesneReportStarSystemId :: !(Key StarSystem)
    , planetDemesneReportName :: !Text
    , planetDemesneReportFormalName :: !DemesneName
    , planetDemesneReport :: !StarDate
    } deriving (Show, Read, Eq)


data StarSystemDemesneReport = StarSystemDemesneReport
    { starSystemDemesneReportStarSystemId :: !(Key StarSystem)
    , starSystemDemesneReportName :: !Text
    , starSystemDemesneReportFormalName :: !DemesneName
    , starSystemDemesneReportDate :: !StarDate
    } deriving (Show, Read, Eq)


instance ToJSON DemesneReport where
    toJSON (PlanetDemesne report) =
        object [ "Tag" .= ("Planet" :: Text)
               , "PlanetId" .= planetDemesneReportPlanetId report
               , "StarSystemId" .= planetDemesneReportStarSystemId report
               , "Name" .= planetDemesneReportName report
               , "FormalName" .= planetDemesneReportFormalName report
               , "Date" .= planetDemesneReport report
               ]

    toJSON (StarSystemDemesne report) =
        object [ "Tag" .= ("StarSystem" :: Text)
               , "StarSystemId" .= starSystemDemesneReportStarSystemId report
               , "Name" .= starSystemDemesneReportName report
               , "FormalName" .= starSystemDemesneReportFormalName report
               , "Date" .= starSystemDemesneReportDate report
               ]


-- | Build demesne report listing all holdings of a person, according to
-- given intelligence level
demesneReport :: StarDate
    -> Person
    -> [Entity StarSystem]
    -> [Entity Planet]
    -> [HumanIntelligence]
    -> [DemesneReport]
demesneReport today person systems planets intel =
    ((planetReport today) <$> incPlanets) ++ ((systemReport today) <$> incSystems)
    where
        incPlanets = filter (planetIncluded intel person) planets
        incSystems = filter (starSystemIncluded intel person) systems


-- | Shoudl planet be included according to HUMINT and primary title
-- if Demesne is included in intelligence, planet is included
-- otherwise it's included if it's either primary title or star system where
-- the planet resides is primary title
planetIncluded :: [HumanIntelligence] -> Person -> Entity Planet -> Bool
planetIncluded intel person planet =
        fullIntel
        || isPrimary
        || isRelatedTitle
    where
        fullIntel = any (\x -> humanIntelligenceLevel x == Demesne) intel
        isPrimary = personPlanetTitle person == (Just $ entityKey planet)
        isRelatedTitle = personStarSystemTitle person == (Just . planetStarSystemId . entityVal) planet


-- | Should a star system be included according to HUMINT and primary title
-- if Demesne is included in intelligence, star system is included
-- otherwise it's included if star system is set a primary title
starSystemIncluded :: [HumanIntelligence] -> Person -> Entity StarSystem -> Bool
starSystemIncluded intel person starSystem =
        fullIntel
        || isPrimary
    where
        fullIntel = any (\x -> humanIntelligenceLevel x == Demesne) intel
        isPrimary = personStarSystemTitle person == (Just $ entityKey starSystem)


-- | Demesne report of given planet with timestamp
planetReport :: StarDate -> Entity Planet -> DemesneReport
planetReport date planet =
    PlanetDemesne $ PlanetDemesneReport
                        { planetDemesneReportPlanetId = entityKey planet
                        , planetDemesneReportStarSystemId = (planetStarSystemId . entityVal) planet
                        , planetDemesneReportName = name
                        , planetDemesneReportFormalName = DemesneName ("Colony of " ++ name)
                        , planetDemesneReport = date
                        }
    where
        name = (planetName . entityVal) planet


-- | Demesne report of given star system with timestamp
systemReport :: StarDate -> Entity StarSystem -> DemesneReport
systemReport date system =
    StarSystemDemesne $ StarSystemDemesneReport
                            { starSystemDemesneReportStarSystemId = entityKey system
                            , starSystemDemesneReportName = name
                            , starSystemDemesneReportFormalName = DemesneName ("Province of " ++ name)
                            , starSystemDemesneReportDate = date
                            }
    where
        name = (starSystemName . entityVal) system


-- | Person report of given person and taking HUMINT level into account
personReport :: StarDate
    -> PersonRelationData
    -> Maybe (Entity Dynasty)
    -> [PersonTrait]
    -> [PersonIntel]
    -> [Relation]
    -> Key Person
    -> PersonReport
personReport today info dynasty allTraits targetIntel relations avatarId =
    PersonReport { personReportId = pId
                 , personReportAvatar = pId == avatarId
                 , personReportName = personName person
                 , personReportShortTitle = shortTitle person
                 , personReportLongTitle = longTitle person
                 , personReportSex = personSex person
                 , personReportGender = personGender person
                 , personReportAge = age (personDateOfBirth person) today
                 , personReportStats = statReport person targetIntelTypes
                 , personReportRelations = (relationsReport targetTraitTypes allTraits) $ personRelationDataLinks info
                 , personReportIntelTypes = targetIntelTypes
                 , personReportDynasty =  dynastyReport <$> dynasty
                 , personReportTraits = if Traits `elem` targetIntel
                                            then Just $ traitReport <$> targetTraits
                                            else Nothing
                 , personReportAvatarOpinion = opinionReport
                                                (mkUniq $ personTraitType <$> avatarTraits)
                                                [minBound..]
                                                targetTraitTypes
                                                targetIntelTypes
                                                targetRelations
                 , personReportOpinionOfAvatar = opinionReport
                                                    targetTraitTypes
                                                    targetIntelTypes
                                                    (mkUniq $ personTraitType <$> avatarTraits)
                                                    [minBound..]
                                                    avatarRelations
                 }
                 where
                    person = (entityVal . personRelationDataPerson) info
                    pId = (entityKey . personRelationDataPerson) info
                    targetIntelTypes = mkUniq $
                                            fmap humanIntelligenceLevel
                                                 (mapMaybe personDataLinkTargetIntelligence $
                                                           personRelationDataLinks info)
                    avatarRelations = flipRelation <$> targetRelations
                    targetRelations = filter (\x -> relationTargetId x == avatarId) relations
                    avatarTraits = filter (\x -> personTraitPersonId x == avatarId) allTraits
                    targetTraits = filter (\x -> personTraitPersonId x == pId) allTraits
                    targetTraitTypes = mkUniq $ fmap personTraitType targetTraits


-- | Relations of a specific person
-- public relations are always known
-- family relations are known if intel includes family relations or secret relations
-- secret relations are known only if intel includes secret relations
relationsReport :: [TraitType] -> [PersonTrait] -> [PersonDataLink] -> [RelationLink]
relationsReport originatorTraits allTraits links =
    mapMaybe (relationLink originatorTraits allTraits) grouped
    where
        known = filter knownLink links
        grouped = groupBy (\x y -> (entityKey . personDataLinkPerson) x == (entityKey . personDataLinkPerson) y) known


-- | Is this link known based on available human intelligence
knownLink :: PersonDataLink -> Bool
knownLink item =
    case (relationVisibility . personDataLinkRelation) item of
        PublicRelation ->
            True

        FamilyRelation ->
            FamilyRelations `elem` intel
            || SecretRelations `elem` intel

        SecretRelation ->
            SecretRelations `elem` intel
    where
        intel = catMaybes [ humanIntelligenceLevel <$> personDataLinkTargetIntelligence item
                          , humanIntelligenceLevel <$> personDataLinkOriginatorIntelligence item
                          ]


-- | Collect all relations of given person and report them
relationLink :: [TraitType] -> [PersonTrait] -> [PersonDataLink] -> Maybe RelationLink
relationLink _ _ [] =
    Nothing

relationLink originatorTraits allTraits links =
    Just $ RelationLink
            { relationLinkName = personName person
            , relationLinkShortTitle = shortTitle person
            , relationLinkLongTitle = longTitle person
            , relationLinkTypes = types
            , relationLinkId = (entityKey . personDataLinkPerson . P.head) links
            , relationLinkOpinion = opinionReport originatorTraits originatorIntel targetTraits targetIntel targetRelations
            }
    where
        person = (entityVal . personDataLinkPerson . P.head) links
        pId = (entityKey . personDataLinkPerson . P.head) links
        types = mkUniq $ fmap (relationType . personDataLinkRelation) links
        targetTraits = mkUniq $ mapMaybe (\x -> if personTraitPersonId x == pId
                                                    then Just $ personTraitType x
                                                    else Nothing)
                                         allTraits
        originatorIntel = mkUniq $ links ^.. traverse . personDataLinkOriginatorIntelligenceL . _Just . humanIntelligenceLevelL
        targetIntel = mkUniq $ links ^.. traverse . personDataLinkTargetIntelligenceL . _Just . humanIntelligenceLevelL
        targetRelations = nub $ personDataLinkRelation <$> links


shortTitle :: Person -> Maybe ShortTitle
shortTitle Person { personPlanetTitle = Just _ } =
    Just "Praetor"

shortTitle Person { personStarSystemTitle = Just _ } =
    Just "Procurator"

shortTitle _ =
    Nothing


longTitle :: Person -> Maybe LongTitle
longTitle _ =
    Just "unimplemented"


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


data RelationLink = RelationLink
    { relationLinkName :: !PersonName
    , relationLinkShortTitle :: !(Maybe ShortTitle)
    , relationLinkLongTitle :: !(Maybe LongTitle)
    , relationLinkTypes :: ![RelationType]
    , relationLinkId :: !(Key Person)
    , relationLinkOpinion :: !OpinionReport
    } deriving (Show, Read, Eq)


data DynastyReport = DynastyReport
    { dynastyReportId :: !(Key Dynasty)
    , dynastyReportName :: !DynastyName
    } deriving (Show, Read, Eq)


dynastyReport :: Entity Dynasty -> DynastyReport
dynastyReport dynasty =
    DynastyReport
        { dynastyReportId = entityKey dynasty
        , dynastyReportName = (dynastyName . entityVal) dynasty
        }


data TraitReport = TraitReport
    { traitReportName :: !TraitName
    , traitReportDescription :: !TraitDescription
    , traitReportType :: !TraitType
    , traitReportValidUntil :: !(Maybe StarDate)
    } deriving (Show, Read, Eq)


newtype TraitName = TraitName { unTraitName :: Text }
    deriving (Show, Read, Eq)


instance IsString TraitName where
    fromString = TraitName . fromString


instance ToJSON TraitName where
    toJSON = toJSON . unTraitName


instance FromJSON TraitName where
    parseJSON =
        withText "trait name"
            (\x -> return $ TraitName x)


newtype TraitDescription = TraitDescription { unTraitDescription :: Text }
    deriving (Show, Read, Eq)


instance IsString TraitDescription where
    fromString = TraitDescription . fromString


instance ToJSON TraitDescription where
    toJSON = toJSON . unTraitDescription


instance FromJSON TraitDescription where
    parseJSON =
        withText "trait description"
            (\x -> return $ TraitDescription x)


traitReport :: PersonTrait -> TraitReport
traitReport trait =
    TraitReport
        { traitReportName = (traitName . personTraitType) trait
        , traitReportDescription = (traitDescription . personTraitType) trait
        , traitReportType = personTraitType trait
        , traitReportValidUntil = personTraitValidUntil trait
        }


traitName :: TraitType -> TraitName
traitName Brave = "Brave"
traitName Coward = "Coward"
traitName Chaste = "Chaste"
traitName Temperate = "Temperate"
traitName Charitable = "Charitable"
traitName Diligent = "Diligent"
traitName Patient = "Patient"
traitName Kind = "Kind"
traitName Humble = "Humble"
traitName Lustful = "Lustful"
traitName Gluttonous = "Gluttonous"
traitName Greedy = "Greedy"
traitName Slothful = "Slothful"
traitName Wroth = "Wroth"
traitName Envious = "Envious"
traitName Proud = "Proud"
traitName Ambitious = "Ambitious"
traitName Content = "Content"
traitName Cruel = "Cruel"
traitName Cynical = "Cynical"
traitName Deceitful = "Deceitful"
traitName Honest = "Honest"
traitName Shy = "Shy"


traitDescription :: TraitType -> TraitDescription
traitDescription Brave = "Brave person doesn't shy away from confrontation"
traitDescription Coward = "Cowardly person shyes away from confrontation"
traitDescription Chaste = "Virtuous or pure person refrains from unlawful sexual activity"
traitDescription Temperate = "Temperate person avoids extremeties in all matters"
traitDescription Charitable = "Charitable person willingly gives to those in need"
traitDescription Diligent = "Work in itself is good and sufficient rewards for one's efforts"
traitDescription Patient = "Steadfast can face long term difficulties and not lose their will"
traitDescription Kind = "Benevolent person treating others with respect"
traitDescription Humble = "Although we are all special, nobody is truly special"
traitDescription Lustful = "Lustful person chases carnal pleasures in excess"
traitDescription Gluttonous = "Lack of moderation in regards to food and drink"
traitDescription Greedy = "Person showing selfish desire for wealth and fortune"
traitDescription Slothful = "Laziness and lack of work has made this character indifferent"
traitDescription Wroth = "Uncontrolled feeling of anger, rage and hathred"
traitDescription Envious = "Coveting what's not yours"
traitDescription Proud = "Placing oneself above others"
traitDescription Ambitious = "Ambitious person has set their goals high"
traitDescription Content = "Content person is happy how things are currently"
traitDescription Cruel = "Indifference of suffering of others"
traitDescription Cynical = "This person has strong distrust on motives of others"
traitDescription Deceitful = "For this person, cheating and deceiving has become second nature"
traitDescription Honest = "Free from fraud and deception"
traitDescription Shy = "This person is nervous or uncomfortable with other people"


-- Lens for accessing originator id of relation
relationOriginatorIdL :: Lens' Relation (Key Person)
relationOriginatorIdL = lens relationOriginatorId (\r v -> r { relationOriginatorId = v})


-- Lens for accessing target id of relation
relationTargetIdL :: Lens' Relation (Key Person)
relationTargetIdL = lens relationTargetId (\r v -> r { relationTargetId = v})


relationTypeL :: Lens' Relation RelationType
relationTypeL = lens relationType (\r v -> r { relationType = v})


humanIntelligenceLevelL :: Lens' HumanIntelligence PersonIntel
humanIntelligenceLevelL = lens humanIntelligenceLevel (\r v -> r { humanIntelligenceLevel = v})


-- | Flipped relation where originator and target id has been swapped
-- and relation type flipped
flipRelation :: Relation -> Relation
flipRelation relation =
    relation & relationOriginatorIdL .~ (relationTargetId relation)
             & relationTargetIdL .~ (relationOriginatorId relation)
             & relationTypeL %~ flipRelationType


-- | Inverse of relation type (Parent -> Child)
flipRelationType :: RelationType -> RelationType
flipRelationType Parent = Child
flipRelationType Child = Parent
flipRelationType Sibling = Sibling
flipRelationType StepParent = StepChild
flipRelationType StepChild = StepParent
flipRelationType StepSibling = StepSibling
flipRelationType Betrothed = Betrothed
flipRelationType Spouse = Spouse
flipRelationType ExSpouse = ExSpouse
flipRelationType Lover = Lover
flipRelationType ExLover = ExLover
flipRelationType Friend = Friend
flipRelationType Rival = Rival


$(deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''PersonReport)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''StatReport)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''RelationLink)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 13 } ''DynastyReport)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 11 } ''TraitReport)
