{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE LambdaCase                 #-}


module Events.News
    ( ResultsReport(..), kragiiWormsEvent, scurryingSoundsEvent, namingPetEvent
    )
    where

import Import
import Data.Maybe ( fromJust )
import System.Random ( newStdGen )

import Common ( safeHead )
import CustomTypes ( StarDate )
import Events.Kragii ( KragiiWormsChoice(..), KragiiResults(..), KragiiNews(..)
                     , KragiiWormsEvent(..) )
import Events.Pets ( ScurryingSoundsEvent(..), ScurryingSoundsChoice(..)
                   , ScurryingSoundsResult(..), ScurryingSoundsNews(..)
                   , NamingPetEvent(..), NamingPetChoice(..)
                   , NamingPetResult(..), NamingPetNews(..)
                   )
import Names ( petNames )
import News.Data ( NewsArticle(..), SpecialNews(..), mkFactionNews
                 , mkFactionSpecialNews, mkPersonalNews
                 , mkPersonalSpecialNews )
import People.Data ( PetType(..), PetName(..), displayPetType )
import Resources ( RawResource(..) )


-- | class used to turn special event with given choice and results into a news article
class ResultsReport a b c | a -> b, a -> c where
    report :: StarDate -> a -> Maybe b -> [c] -> News


-- | Special event of person hearing scurrying sounds in walls of their house
scurryingSoundsEvent :: PersonId -> StarDate -> News
scurryingSoundsEvent pId date =
    let
        content = ScurryingSounds (ScurryingSoundsEvent
            { scurryingSoundsEventPersonId = pId
            , scurryingSoundsEventDate = date
            }) [] Nothing
    in
        mkPersonalSpecialNews date pId content


instance ResultsReport ScurryingSoundsEvent ScurryingSoundsChoice ScurryingSoundsResult where
    report date event choice results =
        let content = ScurryingSoundsNews { scurryingSoundsNewsExplanation = repText
                                          , scurryingSoundsNewsPetId = petId
                                          , scurryingSoundsNewsPetType = pType
                                          , scurryingSoundsNewsDate = date
                                          }
        in
            mkPersonalNews (scurryingSoundsEventPersonId event) date $ ScurryingSoundsResolution content
        where
            repText = header choice <> result results

            header (Just GetCat) = "You ordered a kitten to be brought to you, so that they could hunt maker of noises. "
            header (Just TameRat) = "Intriqued by sounds, you decided to try and tame whatever was making them. "
            header (Just GetRidSomehowElse) = "As you didn't have time to deal with this, you ordered servant to get rid of the noises. "
            header Nothing = "Noises weren't actually that bad, so you postponed your decision. "

            result :: [ScurryingSoundsResult] -> Text
            result res
                | pType == Just Cat = "Fluffy kitten quickly made home of your quarters and drove source of sounds away."
                | pType == Just Rat = "After patiently feeding large rat, it seems that you now have a new pet."
                | TooManyPets `elem` res = "Later on you realised that you already have lots of pets, and sounds have stopped already anyway."
                | CrittersRemoved `elem` res = "You don't know particulars, but noises have stopped now."
                | SoundsStoppedByThemselves `elem` res = "Noises have stopped by themselves."
                | otherwise = ""

            pet = safeHead $ filter (\x -> case x of
                                            (PetObtained _ _) ->
                                                True
                                            _ ->
                                                False)
                                    results
            petId = case pet of
                        Just (PetObtained _ pId) ->
                            Just pId

                        _ ->
                            Nothing
            pType = case pet of
                            Just (PetObtained x _) ->
                                Just x

                            _ ->
                                Nothing

-- | Special event of kragii worms attacking a given planet
-- In case the planet is not currently owned by anyone, event is not created
kragiiWormsEvent :: Entity Planet -> Entity StarSystem -> StarDate -> FactionId -> Maybe News
kragiiWormsEvent planetEntity systemEntity date fId =
    let
        planet = entityVal planetEntity
        content = KragiiWorms (KragiiWormsEvent
                        { kragiiWormsPlanetId = entityKey planetEntity
                        , kragiiWormsPlanetName = planetName planet
                        , kragiiWormsSystemId = entityKey systemEntity
                        , kragiiWormsSystemName = starSystemName $ entityVal systemEntity
                        , kragiiWormsFactionId = fId
                        , kragiiWormsDate = date
                        }) [] Nothing
    in
        mkFactionSpecialNews date <$> planetOwnerId planet
            <*> Just content


instance ResultsReport KragiiWormsEvent KragiiWormsChoice KragiiResults where
    report date event choice results =
        let
            content = KragiiNews { kragiiNewsPlanetId = kragiiWormsPlanetId event
                                 , kragiiNewsPlanetName = kragiiWormsPlanetName event
                                 , kragiiNewsSystemId = kragiiWormsSystemId event
                                 , kragiiNewsSystemName = kragiiWormsSystemName event
                                 , kragiiNewsExplanation = repText
                                 , kragiiNewsDate = date
                                 , kragiiNewsFactionId = kragiiWormsFactionId event
                                 }
        in
            mkFactionNews (kragiiWormsFactionId event) date $ KragiiResolution content
        where
            repText = header choice <> " " <> removed choice (WormsRemoved `elem` results) <> " " <> injury <> " " <> destruction <> " "

            header (Just EvadeWorms) = "Local farmers had chosen to work on their fields, while avoiding the kragii worms."
            header (Just AttackWorms) = "Local farmers had decided to attack the worms with chemicals and burning."
            header (Just TameWorms) = "Decision to try and tame the kragii had been taken."
            header Nothing = "No decision what to do about worms had been taken."

            removed (Just EvadeWorms) True = "After some time, there has been no new kragii sightings and it seems that the threat is now over."
            removed (Just AttackWorms) True = "Attacks seem to have worked and there has been no new kragii sightings."
            removed (Just TameWorms) True = "Kragii has been tamed and put into use of improving soil quality."
            removed Nothing True = "Despite farmers doing nothing at all about the situation, kragii worms disappeared eventually."
            removed (Just EvadeWorms) False = "Kragii are still present on the planet and hamper farming operations considerability."
            removed (Just AttackWorms) False = "Despite the best efforts of farmers, kragii threat is still present."
            removed (Just TameWorms) False = "Taming of the worms was much harder than anticipated and they remain wild."
            removed Nothing False = "While farmers were debating best course of action, kragii reigned free and destroyed crops."

            injury = if FarmersInjured `elem` results
                        then "Some of the personnel involved in the event were seriously injured."
                        else "There are no known reports of personnel injuries."

            totalDestroyed = mconcat $ map (\case
                                                CropsDestroyed n -> n
                                                _ -> mempty) results
            destruction = if totalDestroyed > RawResource 0
                            then "In the end, " <> pack (show (unRawResource totalDestroyed)) <> " units of harvest was destroyed."
                            else "Despite of all this, no harvest was destroyed."


namingPetEvent :: (PersistQueryRead backend, MonadIO m,
    BaseBackend backend ~ SqlBackend) =>
    Entity Person -> Entity Pet -> StarDate -> ReaderT backend m News
namingPetEvent personE petE date = do
    pets <- selectList [ PetOwnerId ==. (entityKey personE)
                       , PetDateOfDeath ==. Nothing
                       ] []
    let names = (petName . entityVal) <$> pets
    g <- liftIO newStdGen
    let availableNames = take 3 $ filter (\x -> not (x `elem` names)) $ petNames g
    let content = NamingPet (NamingPetEvent { namingPetEventPersonId = entityKey personE
                                            , namingPetEventPetId = entityKey petE
                                            , namingPetEventPetType = (petType . entityVal) petE
                                            , namingPetEventDate = date
                                            , namingPetNameOptions = availableNames
                                            })
                            [] Nothing
    return $ mkPersonalSpecialNews date (entityKey personE) content


instance ResultsReport NamingPetEvent NamingPetChoice NamingPetResult where
    report date event _ results =
        let
            content = NamingPetNews { namingPetNewsExplanation = repText
                                    , namingPetNewsPetId = namingPetEventPetId event
                                    , namingPetNewsPetType = namingPetEventPetType event
                                    , namingPetNewsDate = date
                                    }
        in
            mkPersonalNews (namingPetEventPersonId event) date $ NamingPetResolution content
        where
            repText = fromJust $ (givenName <$> gName) <|> (randomName <$> rName) <|> failSafe
            gName = safeHead $ mapMaybe (\x -> case x of
                                                (PetNamed _ name) -> Just name
                                                _ -> Nothing)
                                        results
            rName = safeHead $ mapMaybe (\x -> case x of
                                                (RandomNameGiven _ name) -> Just name
                                                _ -> Nothing)
                                        results

            givenName name = "After careful consideration, you choose "
                                ++ (unPetName name)
                                ++ " as name for your new "
                                ++ displayPetType (namingPetEventPetType event)
                                ++ "."
            randomName name = "Even if you didn't name your "
                                ++ displayPetType (namingPetEventPetType event)
                                ++ ", one of your servants started using name "
                                ++ (unPetName name)
                                ++ " and eventually you started using it too."
            failSafe = Just ("Nobody came up with a suitable name and you had more pressing matters to attend, so your "
                                ++ displayPetType (namingPetEventPetType event)
                                ++ " was left without a name.")
