module SaveData exposing
    ( SaveData(..)
    , fromResult
    , isLoading
    , map
    , succeed
    )

import Http exposing (Error)
import RemoteData exposing (WebData)


{-| Extension to RemoteData module, allowing retaining information on client
when updating data on the server.
-}


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
