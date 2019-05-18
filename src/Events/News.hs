{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE LambdaCase                 #-}


module Events.News ( ResultsReport(..), kragiiWormsEvent )
    where

import Import
import CustomTypes ( StarDate )
import Events.Kragii ( KragiiWormsChoice(..), KragiiResults(..), KragiiNews(..)
                     , KragiiWormsEvent(..) )
import News.Data ( NewsArticle(..), SpecialNews(..), mkNews, mkSpecialNews )
import Resources ( RawResource(..) )


-- | class used to turn special event with given choice and results into a news article
class ResultsReport a b c | a -> b, a -> c where
    report :: Key Faction -> StarDate -> a -> Maybe b -> [c] -> News


-- | Special event of kragii worms attacking a given planet
-- In case the planet is not currently owned by anyone, event is not created
kragiiWormsEvent :: Entity Planet -> Entity StarSystem -> StarDate -> Maybe News
kragiiWormsEvent planetEntity systemEntity date =
    let
        planet = entityVal planetEntity
        content = KragiiWorms (KragiiWormsEvent
                        { kragiiWormsPlanetId = entityKey planetEntity
                        , kragiiWormsPlanetName = planetName planet
                        , kragiiWormsSystemId = entityKey systemEntity
                        , kragiiWormsSystemName = starSystemName $ entityVal systemEntity
                        , kragiiWormsDate = date
                        }) [] Nothing
    in
        mkSpecialNews date <$> planetOwnerId planet
            <*> Just content


instance ResultsReport KragiiWormsEvent KragiiWormsChoice KragiiResults where
    report fId date event choice results =
        let
            content = KragiiNews { kragiiNewsPlanetId = kragiiWormsPlanetId event
                                 , kragiiNewsPlanetName = kragiiWormsPlanetName event
                                 , kragiiNewsSystemId = kragiiWormsSystemId event
                                 , kragiiNewsSystemName = kragiiWormsSystemName event
                                 , kragiiNewsExplanation = repText
                                 , kragiiNewsDate = date
                                 }
        in
            mkNews fId date $ KragiiResolution content
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
