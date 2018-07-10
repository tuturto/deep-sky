{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Faction where

import Import
import MenuHelpers
import Yesod.Form.Bootstrap3

getFactionR :: Handler Html
getFactionR = do
    (_, user) <- requireAuthPair
    faction <- runDB $ maybeFaction user
    factions <- runDB $ selectList [] [ Asc FactionId ]
    (factionSelection, _) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ factionAForm factions 

    defaultLayout $ do
        setTitle "Deep Sky - Faction"
        $(widgetFile "faction")

postFactionR :: Handler Html
postFactionR = do
    (userId, _) <- requireAuthPair    
    factions <- runDB $ selectList [] [ Asc FactionId ]

    ((formRes, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ factionAForm factions 

    res <- case formRes of
                FormSuccess x -> return x
                _ -> redirect FactionR

    _ <- runDB $ update userId [ UserFactionId =. (Just $ fsFactionId res)]

    defaultLayout $ do
        setTitle "Deep Sky - Faction"
        $(widgetFile "joinedfaction")

data FactionSelection = FactionSelection
    { fsFactionId :: Key Faction
    }
    deriving Show

factionAForm :: [Entity Faction] -> AForm Handler FactionSelection
factionAForm factions = FactionSelection
        <$> areq (selectFieldList facs) "Faction: " Nothing
        <*  bootstrapSubmit (BootstrapSubmit ("Submit" :: Text) "btn-default" [])
    where
        facs :: [(Text, Key Faction)]
        facs = Import.map (\x -> ((factionName $ entityVal x), (entityKey x))) factions

