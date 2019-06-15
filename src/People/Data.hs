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

module People.Data
    ( PersonName(..), FirstName(..), FamilyName(..), Cognomen(..)
    , RegnalNumber(..), Sex(..), Gender(..), PersonIntel(..), StatScore(..)
    , Diplomacy(..), Martial(..), Stewardship(..), Intrique(..), Learning(..)
    , DemesneName(..), ShortTitle(..), LongTitle(..), RelationType(..)
    , RelationVisibility(..), DynastyName(..) )
    where

import Data.Aeson ( ToJSON(..), Object, withScientific, withText, withObject )
import Data.Aeson.TH ( deriveJSON, defaultOptions )
import Data.Aeson.Types ( Parser )
import Data.Scientific ( toBoundedInteger )
import Database.Persist.TH
import Database.Persist.Sql
import ClassyPrelude.Yesod   as Import


data PersonName =
    RegularName FirstName FamilyName (Maybe Cognomen)
    | SimpleName FirstName (Maybe Cognomen)
    | RegalName FirstName FamilyName RegnalNumber (Maybe Cognomen)
    deriving (Show, Read, Eq)


instance ToJSON PersonName where
    toJSON (RegularName firstName familyName cognomen) =
        object [ "Tag" .= ("RegularName" :: Text)
               , "FirstName" .= firstName
               , "FamilyName" .= familyName
               , "Cognomen" .= cognomen
               ]

    toJSON (SimpleName firstName cognomen) =
        object [ "Tag" .= ("SimpleName" :: Text)
               , "FirstName" .= firstName
               , "Cognomen" .= cognomen
               ]

    toJSON (RegalName firstName familyName regnalNumber cognomen) =
        object [ "Tag" .= ("RegalName" :: Text)
               , "FirstName" .= firstName
               , "FamilyName" .= familyName
               , "RegnalNumber" .= regnalNumber
               , "Cognomen" .= cognomen
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
    firstName <- o .: "FirstName"
    familyName <- o .: "FamilyName"
    cognomen <- o .: "Cognomen"
    return $ RegularName firstName familyName cognomen

parseName "SimpleName" o = do
    firstName <- o .: "FirstName"
    cognomen <- o .: "Cognomen"
    return $ SimpleName firstName cognomen

parseName "RegalName" o = do
    firstName <- o .: "FirstName"
    familyName <- o .: "FamilyName"
    regnalNumber <- o .: "RegnalNumber"
    cognomen <- o .: "Cognomen"
    return $ RegalName firstName familyName regnalNumber cognomen

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
    deriving (Show, Read, Eq, Enum, Bounded)


data Gender =
    Man
    | Woman
    | Agender
    | Nonbinary
    deriving (Show, Read, Eq, Enum, Bounded)


data PersonIntel =
    Stats
    | Demesne
    | FamilyRelations
    | SecretRelations
    deriving (Show, Read, Eq, Enum, Bounded)


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
    | Spouse
    | ExSpouse
    | Lover
    | ExLover
    | Friend
    | Rival
    deriving (Show, Read, Eq, Enum, Bounded)


data RelationVisibility =
    SecretRelation
    | FamilyRelation
    | PublicRelation
    deriving (Show, Read, Eq, Enum, Bounded)


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


derivePersistField "PersonName"
derivePersistField "Sex"
derivePersistField "Gender"
derivePersistField "PersonIntel"
derivePersistField "RelationType"
derivePersistField "RelationVisibility"

$(deriveJSON defaultOptions ''Sex)
$(deriveJSON defaultOptions ''Gender)
$(deriveJSON defaultOptions ''PersonIntel)
$(deriveJSON defaultOptions ''RelationType)
$(deriveJSON defaultOptions ''RelationVisibility)
