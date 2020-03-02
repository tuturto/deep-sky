module SaveData exposing
    ( SaveData(..)
    , fromResult
    , isLoading
    , map
    , succeed
    , toMaybe
    , toWebData
    , try
    , tryRemote
    , withDefault
    )

import Accessors exposing (Relation, makeOneToN)
import Http exposing (Error)
import RemoteData exposing (RemoteData(..), WebData)


tryRemote : Relation elem sub wrap -> Relation (RemoteData err elem) sub (RemoteData err wrap)
tryRemote =
    makeOneToN
        RemoteData.map
        RemoteData.map


try : Relation elem sub wrap -> Relation (SaveData elem) sub (SaveData wrap)
try =
    makeOneToN
        map
        map


{-| Data that can be saved and retained on client while save is in progress
-}
type SaveData a
    = RData (WebData a)
    | Saving a


{-| Is data being loaded currently. In case it is being saved, return True
Otherwise defaults to RemoteData.isLoading behaviour.
-}
isLoading : SaveData a -> Bool
isLoading d =
    case d of
        RData details ->
            RemoteData.isLoading details

        Saving _ ->
            True


{-| Map function over SaveData
-}
map : (a -> b) -> SaveData a -> SaveData b
map f d =
    case d of
        RData details ->
            RData <| RemoteData.map f details

        Saving details ->
            Saving <| f details


{-| Turn Result into SaveData
-}
fromResult : Result Error a -> SaveData a
fromResult res =
    RData <| RemoteData.fromResult res


{-| Lift data into SaveData
-}
succeed : a -> SaveData a
succeed d =
    RData <| RemoteData.succeed d


withDefault : a -> SaveData a -> a
withDefault def d =
    case d of
        RData details ->
            RemoteData.withDefault def details

        Saving details ->
            details


toMaybe : SaveData a -> Maybe a
toMaybe d =
    case d of
        RData details ->
            RemoteData.toMaybe details

        Saving details ->
            Just details



-- | coerce SaveData to WebData
-- Saving a is coerced to NotAsked


toWebData : SaveData a -> WebData a
toWebData d =
    case d of
        RData r ->
            r

        Saving _ ->
            NotAsked
