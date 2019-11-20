{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE LambdaCase                 #-}

module People.Data
    ( PersonName(..), FirstName(..), FamilyName(..), Cognomen(..)
    , RegnalNumber(..), Sex(..), Gender(..), PersonIntel(..), StatScore(..)
    , Diplomacy(..), Martial(..), Stewardship(..), Intrique(..), Learning(..)
    , DemesneName(..), ShortTitle(..), LongTitle(..), RelationType(..)
    , RelationVisibility(..), DynastyName(..), MarriageStatus(..), TraitType(..)
    , OpinionIntel(..), PetType(..), PetName(..), opinionIntelVisibility
    , displayPetType, firstName, cognomen, familyName, regnalNumber
    )
    where

import Data.Aeson ( ToJSON(..), Object, withScientific, withText, withObject )
import Data.Aeson.TH ( deriveJSON, defaultOptions )
import Data.Aeson.Types ( Parser )
import Data.Scientific ( toBoundedInteger )
import Database.Persist.TH
import Database.Persist.Sql
import ClassyPrelude.Yesod   as Import
import System.Random


data PersonName =
    RegularName FirstName FamilyName (Maybe Cognomen)
    | SimpleName FirstName (Maybe Cognomen)
    | RegalName FirstName FamilyName RegnalNumber (Maybe Cognomen)
    deriving (Show, Read, Eq)


firstName :: PersonName -> FirstName
firstName name =
    case name of
        SimpleName s _ ->
            s

        RegularName s _ _ ->
            s

        RegalName s _ _ _ ->
            s


familyName :: PersonName -> Maybe FamilyName
familyName name =
    case name of
        SimpleName _ _ ->
            Nothing

        RegularName _ s _ ->
            Just s

        RegalName _ s _ _ ->
            Just s


cognomen :: PersonName -> Maybe Cognomen
cognomen name =
    case name of
        SimpleName _ s ->
            s

        RegularName _ _ s ->
            s

        RegalName _ _ _ s ->
            s


regnalNumber :: PersonName -> Maybe RegnalNumber
regnalNumber name =
    case name of
        SimpleName _ _ ->
            Nothing

        RegularName _ _ _ ->
            Nothing

        RegalName _ _ n _ ->
            Just n


instance ToJSON PersonName where
    toJSON (RegularName a b c) =
        object [ "Tag" .= ("RegularName" :: Text)
               , "FirstName" .= a
               , "FamilyName" .= b
               , "Cognomen" .= c
               ]

    toJSON (SimpleName a b) =
        object [ "Tag" .= ("SimpleName" :: Text)
               , "FirstName" .= a
               , "Cognomen" .= b
               ]

    toJSON (RegalName a b c d) =
        object [ "Tag" .= ("RegalName" :: Text)
               , "FirstName" .= a
               , "FamilyName" .= b
               , "RegnalNumber" .= c
               , "Cognomen" .= d
               ]


instance FromJSON PersonName where
    parseJSON = withObject "person name" $ \o -> do
        tag <- o .: "Tag"
        name <- parseName tag o
        return name


-- | Parser for person names
-- first parameter defines value constructor to use
parseName :: Text -> Object -> Parser PersonName
parseName "RegularName" o = do
    a <- o .: "FirstName"
    b <- o .: "FamilyName"
    c <- o .: "Cognomen"
    return $ RegularName a b c

parseName "SimpleName" o = do
    a <- o .: "FirstName"
    b <- o .: "Cognomen"
    return $ SimpleName a b

parseName "RegalName" o = do
    a <- o .: "FirstName"
    b <- o .: "FamilyName"
    c <- o .: "RegnalNumber"
    d <- o .: "Cognomen"
    return $ RegalName a b c d

parseName _ _  = mempty


newtype FirstName = FirstName { unFirstName :: Text }
    deriving (Show, Read, Eq)


instance IsString FirstName where
    fromString = FirstName . fromString


instance ToJSON FirstName where
    toJSON = toJSON . unFirstName


instance FromJSON FirstName where
    parseJSON =
        withText "first name"
            (\x -> return $ FirstName x)


newtype FamilyName = FamilyName { unFamilyName :: Text }
    deriving (Show, Read, Eq)


instance IsString FamilyName where
    fromString = FamilyName . fromString


instance ToJSON FamilyName where
    toJSON = toJSON . unFamilyName


instance FromJSON FamilyName where
    parseJSON =
        withText "family name"
            (\x -> return $ FamilyName x)


newtype Cognomen = Cognomen { unCognomen :: Text }
    deriving (Show, Read, Eq)


instance IsString Cognomen where
    fromString = Cognomen . fromString


instance ToJSON Cognomen where
    toJSON = toJSON . unCognomen


instance FromJSON Cognomen where
    parseJSON =
        withText "cognomen"
            (\x -> return $ Cognomen x)


newtype RegnalNumber = RegnalNumber { unRegnalNumber :: Int }
    deriving (Show, Read, Eq, Ord, Num)


instance ToJSON RegnalNumber where
    toJSON = toJSON . unRegnalNumber


instance FromJSON RegnalNumber where
    parseJSON =
        withScientific "regnal number"
            (\x -> case toBoundedInteger x of
                Nothing ->
                    return $ RegnalNumber 1

                Just n ->
                    return $ RegnalNumber n)


data Sex =
    Male
    | Female
    | Intersex
    deriving (Show, Read, Eq, Enum, Bounded, Ord)


data Gender =
    Man
    | Woman
    | Agender
    | Nonbinary
    deriving (Show, Read, Eq, Enum, Bounded, Ord)


data PersonIntel =
    Stats
    | Demesne
    | FamilyRelations
    | SecretRelations
    | Opinions OpinionIntel
    | Traits
    | Location
    | Activity
    deriving (Show, Read, Eq)


instance Ord PersonIntel where
    a < b = fromEnum a < fromEnum b
    a <= b = fromEnum a <= fromEnum b
    a > b = fromEnum a > fromEnum b
    a >= b = fromEnum a >= fromEnum b


instance Bounded PersonIntel where
    minBound =
        Stats

    maxBound =
        Activity


instance Enum PersonIntel where
    enumFrom x =
        enumFromTo x maxBound

    enumFromThen x y =
        enumFromThenTo x y bound
        where
            bound | fromEnum y >= fromEnum x = maxBound
                  | otherwise = minBound

    fromEnum Stats = 0
    fromEnum Demesne = 1
    fromEnum FamilyRelations = 2
    fromEnum SecretRelations = 3
    fromEnum (Opinions (BaseOpinionIntel PublicRelation)) = 4
    fromEnum (Opinions (BaseOpinionIntel FamilyRelation)) = 5
    fromEnum (Opinions (BaseOpinionIntel SecretRelation)) = 6
    fromEnum (Opinions (ReasonsForOpinions PublicRelation)) = 7
    fromEnum (Opinions (ReasonsForOpinions FamilyRelation)) = 8
    fromEnum (Opinions (ReasonsForOpinions SecretRelation)) = 9
    fromEnum (Opinions (DetailedOpinions PublicRelation)) = 10
    fromEnum (Opinions (DetailedOpinions FamilyRelation)) = 11
    fromEnum (Opinions (DetailedOpinions SecretRelation)) = 12
    fromEnum Traits = 13
    fromEnum Location = 14
    fromEnum Activity = 15

    toEnum 0 = Stats
    toEnum 1 = Demesne
    toEnum 2 = FamilyRelations
    toEnum 3 = SecretRelations
    toEnum 4 = Opinions $ BaseOpinionIntel PublicRelation
    toEnum 5 = Opinions $ BaseOpinionIntel FamilyRelation
    toEnum 6 = Opinions $ BaseOpinionIntel SecretRelation
    toEnum 7 = Opinions $ ReasonsForOpinions PublicRelation
    toEnum 8 = Opinions $ ReasonsForOpinions FamilyRelation
    toEnum 9 = Opinions $ ReasonsForOpinions SecretRelation
    toEnum 10 = Opinions $ DetailedOpinions PublicRelation
    toEnum 11 = Opinions $ DetailedOpinions FamilyRelation
    toEnum 12 = Opinions $ DetailedOpinions SecretRelation
    toEnum 13 = Traits
    toEnum 14 = Location
    toEnum 15 = Activity
    toEnum n = error $ "failed to map: " ++ show n


data OpinionIntel =
    BaseOpinionIntel RelationVisibility
    | ReasonsForOpinions RelationVisibility
    | DetailedOpinions RelationVisibility
    deriving (Show, Read, Eq, Ord)


opinionIntelVisibility :: OpinionIntel -> RelationVisibility
opinionIntelVisibility =
    \case
        BaseOpinionIntel x ->
            x

        ReasonsForOpinions x ->
            x

        DetailedOpinions x ->
            x


instance Bounded OpinionIntel where
    minBound =
        BaseOpinionIntel PublicRelation

    maxBound =
        DetailedOpinions SecretRelation


newtype StatScore a = StatScore { unStatScore :: Int }
    deriving (Show, Read, Eq, Num, Ord)


instance ToJSON (StatScore a) where
    toJSON = toJSON . unStatScore


instance FromJSON (StatScore a) where
    parseJSON =
        withScientific "stat score"
            (\x -> case toBoundedInteger x of
                Nothing ->
                    mempty

                Just n ->
                    return $ StatScore n)


instance PersistField (StatScore a) where
    toPersistValue (StatScore n) =
        PersistInt64 $ fromIntegral n

    fromPersistValue (PersistInt64 n) =
        Right $ StatScore $ fromIntegral n

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql (StatScore a) where
    sqlType _ = SqlInt64


instance Random (StatScore a) where
    randomR (a, b) g =
        (StatScore stat, g')
        where
            (stat, g') = randomR (unStatScore a, unStatScore b) g

    random g =
        randomR (1, 20) g


data Diplomacy = Diplomacy

data Martial = Martial

data Stewardship = Stewardship

data Intrique = Intrique

data Learning = Learning


newtype DemesneName = DemesneName { unDemesneName :: Text }
    deriving (Show, Read, Eq)


instance IsString DemesneName where
    fromString = DemesneName . fromString


instance ToJSON DemesneName where
    toJSON = toJSON . unDemesneName


newtype ShortTitle = ShortTitle { unShortTitle :: Text}
    deriving (Show, Read, Eq)


instance IsString ShortTitle where
    fromString = ShortTitle . fromString


instance ToJSON ShortTitle where
    toJSON = toJSON . unShortTitle


instance FromJSON ShortTitle where
    parseJSON =
        withText "short title"
            (\x -> return $ ShortTitle x)


newtype LongTitle = LongTitle { unLongTitle :: Text}
    deriving (Show, Read, Eq)


instance IsString LongTitle where
    fromString = LongTitle . fromString


instance ToJSON LongTitle where
    toJSON = toJSON . unLongTitle


instance FromJSON LongTitle where
    parseJSON =
        withText "long title"
            (\x -> return $ LongTitle x)


data RelationType =
    Parent
    | Child
    | Sibling
    | StepParent
    | StepChild
    | StepSibling
    | Betrothed
    | Spouse
    | ExSpouse
    | Lover
    | ExLover
    | Friend
    | Rival
    deriving (Show, Read, Eq, Enum, Bounded, Ord)


data RelationVisibility =
    SecretRelation
    | FamilyRelation
    | PublicRelation
    deriving (Show, Read, Eq, Enum, Bounded, Ord)


newtype DynastyName = MkDynastyName { unDynastyName :: Text }
    deriving (Show, Read, Eq)


instance IsString DynastyName where
    fromString = MkDynastyName . fromString


instance ToJSON DynastyName where
    toJSON = toJSON . unDynastyName


instance FromJSON DynastyName where
    parseJSON =
        withText "dynasty name"
            (\x -> return $ MkDynastyName x)


instance PersistField DynastyName where
    toPersistValue (MkDynastyName s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ MkDynastyName s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql DynastyName where
    sqlType _ = SqlString


data MarriageStatus =
    Engaged
    | Married
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


data TraitType =
    Brave
    | Coward
    | Chaste
    | Temperate
    | Charitable
    | Diligent
    | Patient
    | Kind
    | Humble
    | Lustful
    | Gluttonous
    | Greedy
    | Slothful
    | Wroth
    | Envious
    | Proud
    | Ambitious
    | Content
    | Cruel
    | Cynical
    | Deceitful
    | Honest
    | Shy
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


data PetType =
    Cat
    | Rat
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


displayPetType :: PetType -> Text
displayPetType = \case
    Cat ->
        "cat"

    Rat ->
        "rat"


newtype PetName = MkPetName { unPetName :: Text }
    deriving (Show, Read, Eq)


instance IsString PetName where
    fromString = MkPetName . fromString


instance ToJSON PetName where
    toJSON = toJSON . unPetName


instance FromJSON PetName where
    parseJSON =
        withText "pet name"
            (\x -> return $ MkPetName x)

instance PersistField PetName where
    toPersistValue (MkPetName s) =
        PersistText s

    fromPersistValue (PersistText s) =
        Right $ MkPetName s

    fromPersistValue _ =
        Left "Failed to deserialize"


instance PersistFieldSql PetName where
    sqlType _ = SqlString


derivePersistField "PersonName"
derivePersistField "Sex"
derivePersistField "Gender"
derivePersistField "PersonIntel"
derivePersistField "RelationType"
derivePersistField "RelationVisibility"
derivePersistField "MarriageStatus"
derivePersistField "TraitType"
derivePersistField "PetType"

$(deriveJSON defaultOptions ''Sex)
$(deriveJSON defaultOptions ''Gender)
$(deriveJSON defaultOptions ''PersonIntel) -- TODO: hand written instance?
$(deriveJSON defaultOptions ''RelationType)
$(deriveJSON defaultOptions ''RelationVisibility)
$(deriveJSON defaultOptions ''MarriageStatus)
$(deriveJSON defaultOptions ''OpinionIntel) -- TODO: hand written instance?
$(deriveJSON defaultOptions ''TraitType)
$(deriveJSON defaultOptions ''PetType)
