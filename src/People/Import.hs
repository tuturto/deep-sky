{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module People.Import
    ( PersonReport(..), StatReport(..), RelationLink(..), personReport
    , demesneReport, shortTitle, longTitle, knownRelation, relationsReport
    )
    where

import Import
import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import CustomTypes ( StarDate, Age, age )
import People.Data ( PersonIntel(..), Diplomacy, Martial, Stewardship
                   , Intrique, Learning, StatScore, PersonName, Sex
                   , Gender, DemesneName(..), ShortTitle(..), LongTitle(..)
                   , RelationVisibility(..), RelationType(..)
                   )


data PersonReport = PersonReport
    { personReportId :: Key Person
    , personReportName :: PersonName
    , personReportShortTitle :: Maybe ShortTitle
    , personReportLongTitle :: Maybe LongTitle
    , personReportSex :: Sex
    , personReportGender :: Gender
    , personReportAge :: Age
    , personReportStats :: Maybe StatReport
    , personReportRelations :: [RelationLink]
    } deriving (Show, Read, Eq)


data StatReport = StatReport
    { statReportDiplomacy :: StatScore Diplomacy
    , statReportMartial :: StatScore Martial
    , statReportStewardship :: StatScore Stewardship
    , statReportIntrique :: StatScore Intrique
    , statReportLearning :: StatScore Learning
    } deriving (Show, Read, Eq)


data DemesneReport =
    PlanetDemesne PlanetDemesneReport
    | StarSystemDemesne StarSystemDemesneReport
    deriving (Show, Read, Eq)


data PlanetDemesneReport = PlanetDemesneReport
    { planetDemesneReportPlanetId :: Key Planet
    , planetDemesneReportStarSystemId :: Key StarSystem
    , planetDemesneReportName :: Text
    , planetDemesneReportFormalName :: DemesneName
    , planetDemesneReport :: StarDate
    } deriving (Show, Read, Eq)


data StarSystemDemesneReport = StarSystemDemesneReport
    { starSystemDemesneReportStarSystemId :: Key StarSystem
    , starSystemDemesneReportName :: Text
    , starSystemDemesneReportFormalName :: DemesneName
    , starSystemDemesneReportDate :: StarDate
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
personReport :: StarDate -> Entity Person -> [PersonIntel] -> [Relation] -> [Entity Person] -> PersonReport
personReport today personE intel relations related =
    PersonReport { personReportId = entityKey personE
                 , personReportName = personName person
                 , personReportShortTitle = shortTitle person
                 , personReportLongTitle = longTitle person
                 , personReportSex = personSex person
                 , personReportGender = personGender person
                 , personReportAge = age (personDateOfBirth person) today
                 , personReportStats = statReport person intel
                 , personReportRelations = relationsReport relations related intel
                 }
    where
        person = entityVal personE


-- | Relations of a specific person
-- public relations are always known
-- family relations are known if intel includes family relations or secret relations
-- secret relations are known only if intel includes secret relations
relationsReport :: [Relation] -> [Entity Person] -> [PersonIntel] -> [RelationLink]
relationsReport relations related intel =
    (relationLink known) <$> related
    where
        known = filter (knownRelation intel) relations


-- | Is relation known with given level of intel
knownRelation :: [PersonIntel] -> Relation -> Bool
knownRelation _ Relation { relationVisibility = PublicRelation } =
    True

knownRelation intel Relation { relationVisibility = FamilyRelation } =
    FamilyRelations `elem` intel
    || SecretRelations `elem` intel

knownRelation intel Relation { relationVisibility = SecretRelation } =
    SecretRelations `elem` intel


-- | Collect all relations of given person and report them
relationLink :: [Relation] -> Entity Person -> RelationLink
relationLink relations personE =
    RelationLink
        { relationLinkName = personName person
        , relationLinkShortTitle = shortTitle person
        , relationLinkLongTitle = longTitle person
        , relationLinkTypes = relationType <$> relevant
        , relationLinkId = entityKey personE
        }
    where
        person = entityVal personE
        relevant = filter (\x -> relationOriginatorId x == entityKey personE) relations


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
    { relationLinkName :: PersonName
    , relationLinkShortTitle :: Maybe ShortTitle
    , relationLinkLongTitle :: Maybe LongTitle
    , relationLinkTypes :: [RelationType]
    , relationLinkId :: Key Person
    } deriving (Show, Read, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''PersonReport)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 10 } ''StatReport)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''RelationLink)
