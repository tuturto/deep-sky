module Api.Messages exposing (deleteNews, getIcons, getNews, postNews)

import Accessors
import Api.Common
    exposing
        ( delete
        , get
        , post
        , starDateDecoder
        , starDateEncoder
        )
import Api.Endpoints exposing (Endpoint(..))
import Api.StarSystem
    exposing
        ( buildingIdDecoder
        , planetIdDecoder
        , starSystemIdDecoder
        )
import Api.User exposing (userNameDecoder)
import Data.Accessors exposing (iconsA)
import Data.Common exposing (MessageId(..), StarDate(..), triple)
import Data.Messages
    exposing
        ( BuildingFinishedNews
        , DesignCreatedNews
        , NewsArticle
        , NewsContent(..)
        , PlanetFoundNews
        , ShipFinishedNews
        , StarFoundNews
        , UserIcon(..)
        , UserWrittenNews
        )
import Data.Model exposing (ApiMsg(..), Model, Msg(..))
import Data.User exposing (UserName(..), unUserName)
import Http
import Json.Decode as Decode
    exposing
        ( andThen
        , fail
        , field
        , float
        , index
        , int
        , list
        , maybe
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Extra
    exposing
        ( andMap
        , when
        )
import Json.Encode as Encode
import Tuple exposing (pair)


{-| Retrieve recent news from the server
This function never uses cached values, but will always retrieve data from the server
-}
getNews : Cmd Msg
getNews =
    Http.send (ApiMsgCompleted << NewsReceived) (get ApiMessageList (list newsDecoder))


{-| Mark news entry read by current user
-}
deleteNews : MessageId -> Cmd Msg
deleteNews mId =
    Http.send (ApiMsgCompleted << NewsReceived) (delete (ApiSingleMessage mId) (list newsDecoder))



-- post : Endpoint -> Encode.Value -> Decode.Decoder a -> Http.Request a


{-| Submit user written news entry
-}
postNews : String -> UserIcon -> Cmd Msg
postNews message icon =
    let
        content =
            UserWritten
                { message = message
                , author = UserName "" -- Filled in by server
                , icon = icon
                }

        news =
            { messageId = MessageId 0 -- Filled in by server
            , starDate = StarDate 0 -- Filled in by server
            , icon = "" -- Filled in by server
            , content = content
            }
    in
    Http.send (ApiMsgCompleted << NewsReceived) (post ApiMessageList (newsEncoder news) (list newsDecoder))


newsEncoder : NewsArticle -> Encode.Value
newsEncoder article =
    Encode.object
        [ ( "id", messageIdEncoder article.messageId )
        , ( "contents", newsContentEncoder article.starDate article.content )
        , ( "tag", Encode.string "UserWritten" )
        , ( "icon", Encode.string article.icon )
        , ( "starDate", starDateEncoder article.starDate )
        ]


newsContentEncoder : StarDate -> NewsContent -> Encode.Value
newsContentEncoder sDate content =
    case content of
        UserWritten article ->
            Encode.object
                [ ( "content", Encode.string article.message )
                , ( "starDate", starDateEncoder sDate )
                , ( "userName", Encode.string (unUserName article.author) )
                , ( "icon", userIconEncoder article.icon )
                ]

        _ ->
            Debug.todo "implement later"


{-| Retrieve user news icons and links to corresponding images
This method will not retrieve them again if supplied model contains cached ones
-}
getIcons : Model -> Cmd Msg
getIcons model =
    case Accessors.get iconsA model of
        Just _ ->
            Cmd.none

        Nothing ->
            Http.send (ApiMsgCompleted << IconsReceived) (get ApiIcon (list iconDefinitionDecoder))


{-| Helper function to write fluent looking decoders
-}
is : String -> String -> Bool
is a b =
    a == b


{-| Decoder for news article
-}
newsDecoder : Decode.Decoder NewsArticle
newsDecoder =
    let
        body header =
            newsContentDecoder
                |> andThen (result header)

        result ( icon, mId, sDate ) content =
            succeed
                { messageId = mId
                , starDate = sDate
                , icon = icon
                , content = content
                }
    in
    newsHeader
        |> andThen body


{-| Decoder to handle news specific bodies
-}
newsContentDecoder : Decode.Decoder NewsContent
newsContentDecoder =
    oneOf
        [ when newsType (is "StarFound") (field "contents" starFoundNewsArticle)
        , when newsType (is "PlanetFound") (field "contents" planetFoundNewsArticle)
        , when newsType (is "UserWritten") (field "contents" userWrittenNewsArticle)
        , when newsType (is "BuildingFinished") (field "contents" buildingFinishedNewsArticle)
        , when newsType (is "DesignCreated") (field "contents" designCreatedNewsArticle)
        , when newsType (is "ShipFinished") (field "contents" shipFinishedNewsArticle)
        ]


{-| Decoder for news type string
-}
newsType : Decode.Decoder String
newsType =
    field "tag" string


{-| Decoder for star found news article
-}
starFoundNewsArticle : Decode.Decoder NewsContent
starFoundNewsArticle =
    let
        decoder =
            succeed StarFoundNews
                |> andMap (field "starName" string)
                |> andMap (field "systemName" string)
                |> andMap (field "systemId" starSystemIdDecoder)
    in
    succeed StarFound
        |> andMap decoder


{-| Decoder for planet found news article
-}
planetFoundNewsArticle : Decode.Decoder NewsContent
planetFoundNewsArticle =
    let
        decoder =
            succeed PlanetFoundNews
                |> andMap (field "planetName" string)
                |> andMap (field "systemName" string)
                |> andMap (field "systemId" starSystemIdDecoder)
                |> andMap (field "planetId" planetIdDecoder)
    in
    succeed PlanetFound
        |> andMap decoder


{-| Decoder for user written news article
-}
userWrittenNewsArticle : Decode.Decoder NewsContent
userWrittenNewsArticle =
    let
        decoder =
            succeed UserWrittenNews
                |> andMap (field "content" string)
                |> andMap (field "userName" userNameDecoder)
                |> andMap (field "icon" userIconDecoder)
    in
    succeed UserWritten
        |> andMap decoder


{-| Decode common message header containing link to icon and message id
-}
newsHeader : Decode.Decoder ( String, MessageId, StarDate )
newsHeader =
    succeed triple
        |> andMap (field "icon" string)
        |> andMap (field "id" messageId)
        |> andMap (field "starDate" starDateDecoder)


{-| Decoder for message id
-}
messageId : Decode.Decoder MessageId
messageId =
    succeed MessageId
        |> andMap int


{-| Encoder for message id
-}
messageIdEncoder : MessageId -> Encode.Value
messageIdEncoder (MessageId x) =
    Encode.int x


{-| Decoder for design created news article
-}
designCreatedNewsArticle : Decode.Decoder NewsContent
designCreatedNewsArticle =
    fail "not implemented"


{-| Decoder for building finished news article
-}
buildingFinishedNewsArticle : Decode.Decoder NewsContent
buildingFinishedNewsArticle =
    let
        decoder =
            succeed BuildingFinishedNews
                |> andMap (field "planetName" string)
                |> andMap (field "planetId" planetIdDecoder)
                |> andMap (field "systemName" string)
                |> andMap (field "systemId" starSystemIdDecoder)
                |> andMap (field "constructionName" string)
                |> andMap (field "buildingId" buildingIdDecoder)
    in
    succeed BuildingFinished
        |> andMap decoder


{-| Decoder for ship finished news article
-}
shipFinishedNewsArticle : Decode.Decoder NewsContent
shipFinishedNewsArticle =
    fail "not implemented"


{-| Decoder for user icons and links to their resources
-}
iconDefinitionDecoder : Decode.Decoder ( UserIcon, String )
iconDefinitionDecoder =
    succeed pair
        |> andMap (index 0 userIconDecoder)
        |> andMap (index 1 string)


userIconDecoder : Decode.Decoder UserIcon
userIconDecoder =
    string |> andThen stringToUserIcon


stringToUserIcon : String -> Decode.Decoder UserIcon
stringToUserIcon s =
    case s of
        "GenericUserNews" ->
            succeed GenericUserNewsIcon

        "JubilationUserNews" ->
            succeed JubilationUserNewsIcon

        "CatUserNews" ->
            succeed CatUserNewsIcon

        _ ->
            fail <| "Unknown user icon: " ++ s


userIconEncoder : UserIcon -> Encode.Value
userIconEncoder icon =
    case icon of
        GenericUserNewsIcon ->
            Encode.string "GenericUserNews"

        JubilationUserNewsIcon ->
            Encode.string "JubilationUserNews"

        CatUserNewsIcon ->
            Encode.string "CatUserNews"
