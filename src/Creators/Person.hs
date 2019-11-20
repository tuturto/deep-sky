{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}


module Creators.Person ( PersonOptions(..), AgeOptions(..), generatePersonM
    , sexes, generateSexM, sexToGenderFreq, generateGenderM, generateNameM
    , generateDoB )
    where

import Control.Monad.Random
import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import Data.Maybe ( fromJust )
import Import

import Common ( Frequency(..), chooseM)
import CustomTypes ( Age(..), StarDate(..) )
import Names ( greekMasculineNameM, greekFemineNameM, greekNameM )
import People.Data ( Gender(..), PersonName(..), Sex(..), Gender(..) )


-- | Options for person creation
data PersonOptions = PersonOptions
    { personOptionsAge :: Maybe AgeOptions
    } deriving (Show, Read, Eq)


-- | Options for new person's age
data AgeOptions =
    AgeBracket Age Age
    | ExactAge Age
    deriving (Show, Read, Eq)


-- | Generate a random person according to given specifications
generatePersonM :: RandomGen g => StarDate -> PersonOptions -> Rand g Person
generatePersonM date opt = do
    diplomacy <- getRandom
    martial <- getRandom
    stewardship <- getRandom
    intrique <- getRandom
    learning <- getRandom
    sex <- generateSexM sexes
    gender <- generateGenderM sexToGenderFreq sex
    name <- generateNameM gender
    dob <- generateDoB date $ personOptionsAge opt

    let person = Person { personName = name
                        , personSex = sex
                        , personGender = gender
                        , personDateOfBirth = dob
                        , personDiplomacy = diplomacy
                        , personMartial = martial
                        , personStewardship = stewardship
                        , personIntrique = intrique
                        , personLearning = learning
                        , personFactionId = Nothing
                        , personPlanetTitle = Nothing
                        , personStarSystemTitle = Nothing
                        , personDynastyId = Nothing
                        }
    return person


-- | Relative distributions between sexes
sexes :: [Frequency Sex]
sexes =
    [ Frequency 48 Female
    , Frequency 4 Intersex
    , Frequency 48 Male
    ]


-- | Randomly selected sex, based on given distribution
generateSexM :: RandomGen g => [Frequency Sex] -> Rand g Sex
generateSexM freqs = do
    sex <- chooseM freqs
    return $ fromJust sex


-- | Likelyhood of given sex-gender - pairing
sexToGenderFreq :: Sex -> Gender -> Frequency Gender
sexToGenderFreq Female   Agender =   Frequency 5  Agender
sexToGenderFreq Female   Man =       Frequency 5  Man
sexToGenderFreq Female   Nonbinary = Frequency 5  Nonbinary
sexToGenderFreq Female   Woman =     Frequency 85 Woman

sexToGenderFreq Intersex Agender =   Frequency 10 Agender
sexToGenderFreq Intersex Man =       Frequency 40 Man
sexToGenderFreq Intersex Nonbinary = Frequency 10 Nonbinary
sexToGenderFreq Intersex Woman =     Frequency 40 Woman

sexToGenderFreq Male     Agender =   Frequency 5  Agender
sexToGenderFreq Male     Man =       Frequency 85 Man
sexToGenderFreq Male     Nonbinary = Frequency 5  Nonbinary
sexToGenderFreq Male     Woman =     Frequency 5  Woman


-- | Randomly selected gender, based on given probabilistic mapping and sex
generateGenderM :: RandomGen g => (Sex -> Gender -> Frequency Gender) -> Sex -> Rand g Gender
generateGenderM mapping sex = do
    gender <- chooseM ((mapping sex) <$> [minBound..])
    return $ fromJust gender


-- | Name generated based on gender
generateNameM :: RandomGen g => Gender -> Rand g PersonName
generateNameM Man = do
    name <- greekMasculineNameM
    return $ SimpleName name Nothing

generateNameM Woman = do
    name <- greekFemineNameM
    return $ SimpleName name Nothing

generateNameM _ = do
    name <- greekNameM
    return $ SimpleName name Nothing


-- | Date of birth generated based on current star date and age options
generateDoB :: RandomGen g => StarDate -> Maybe AgeOptions -> Rand g StarDate
generateDoB (StarDate date) (Just (AgeBracket (Age start) (Age end))) = do
    months <- getRandomR (0, 9)
    let dateStart = date - (fromIntegral start * 10) - months
    let dateEnd = date - (fromIntegral end * 10)
    dob <- getRandomR (dateStart, dateEnd)
    return $ StarDate dob

generateDoB (StarDate date) (Just (ExactAge (Age target))) = do
    months <- getRandomR (0, 9)
    return $ StarDate $ date - (fromIntegral target * 10) - months

generateDoB (StarDate date) Nothing = do
    dob <- getRandomR (date - 800, date - 1)
    return $ StarDate dob


$(deriveJSON defaultOptions { fieldLabelModifier = drop 13 } ''PersonOptions)
$(deriveJSON defaultOptions ''AgeOptions)
