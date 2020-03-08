module SaveData exposing
    ( SaveData(..)
    , fromResult
    , isLoading
    , map
    , succeed
    , try
    , tryRemote
    )

import Accessors exposing (Relation, makeOneToN)
import Http exposing (Error)
import RemoteData exposing (RemoteData, WebData)


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
