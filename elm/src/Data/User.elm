module Data.User exposing
    ( Role(..)
    , UserName(..)
    , unUserName
    )


type Role
    = PlayerRole
    | AdminRole


type UserName
    = UserName String


unUserName : UserName -> String
unUserName (UserName x) =
    x
