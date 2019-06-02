module Api.Messages exposing
    ( deleteNews
    , getIcons
    , getNews
    , postNews
    , putNews
    )

import Accessors
import Api.Common
    exposing
        ( delete
        , get
        , is
        , planetIdDecoder
        , planetIdEncoder
        , post
        , put
        , resourceTypeDecoder
        , resourceTypeEncoder
        , starDateDecoder
        , starDateEncoder
        , starSystemIdDecoder
        , starSystemIdEncoder
        )
import Api.Designer exposing (designIdDecoder, designNameDecoder)
import Api.Endpoints exposing (Endpoint(..))
import Api.StarSystem exposing (buildingIdDecoder)
import Api.User
    exposing
        ( factionIdDecoder
        , factionIdEncoder
        , userNameDecoder
        )
import Data.Accessors exposing (iconsA)
import Data.Common exposing (MessageId(..), StarDate(..), triple)
import Data.Messages
    exposing
        ( BuildingFinishedNews
        , DesignCreatedNews
        , KragiiResolution
        , KragiiSpecialEvent
        , NewsArticle
        , NewsContent(..)
        , PlanetFoundNews
        , ProductionChangeNews
        , ResearchCompletedNews
        , ShipFinishedNews
        , SpecialEventChoice(..)
        , SpecialEventOption
        , StarFoundNews
        , UserIcon(..)
        , UserWrittenNews
        , unSpecialEventChoice
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
        , optionalField
        , when
        , withDefault
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
    Http.send (ApiMsgCompleted << NewsReceived) (delete (ApiSingleMessage mId) Nothing (list newsDecoder))


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
            , options = [] -- Filled in by server
            , choice = Nothing -- Not used in user written entries
            }
    in
    Http.send (ApiMsgCompleted << NewsReceived) (post ApiMessageList (newsEncoder news) (list newsDecoder))


{-| Update news article. Used for making choice for special events
-}
putNews : NewsArticle -> Cmd Msg
putNews article =
    Http.send (ApiMsgCompleted << NewsReceived) (put (ApiSingleMessage article.messageId) (newsEncoder article) (list newsDecoder))


newsEncoder : NewsArticle -> Encode.Value
newsEncoder article =
    Encode.object
        [ ( "id", messageIdEncoder article.messageId )
        , ( "contents", newsContentEncoder article )
        , ( "tag", Encode.string <| newsTag article )
        , ( "icon", Encode.string article.icon )
        , ( "starDate", starDateEncoder article.starDate )
        ]


{-| Tag of NewsArticle, used to identify NewsArticle type in JSON
-}
newsTag : NewsArticle -> String
newsTag article =
    case article.content of
        StarFound _ ->
            "StarFound"

        PlanetFound _ ->
            "PlanetFound"

        UserWritten _ ->
            "UserWritten"

        DesignCreated _ ->
            "DesignCreated"

        BuildingFinished _ ->
            "BuildingFinished"

        ShipFinished _ ->
            "ShipFinished"

        ProductionBoostStarted _ ->
            "ProductionBoostStarted"

        ProductionSlowdownStarted _ ->
            "ProductionSlowdownStarted"

        ProductionBoostEnded _ ->
            "ProductionBoostEnded"

        ProductionSlowdownEnded _ ->
            "ProductionSlowdownEnded"

        ResearchCompleted _ ->
            "ResearchCompleted"

        KragiiEvent _ ->
            "KragiiEvent"

        KragiiResolved _ ->
            "KragiiResolution"


newsContentEncoder : NewsArticle -> Encode.Value
newsContentEncoder newsArticle =
    case newsArticle.content of
        UserWritten article ->
            Encode.object
                [ ( "content", Encode.string article.message )
                , ( "starDate", starDateEncoder newsArticle.starDate )
                , ( "userName", Encode.string (unUserName article.author) )
                , ( "icon", userIconEncoder article.icon )
                ]

        KragiiEvent article ->
            Encode.object
                [ ( "PlanetId", planetIdEncoder article.planetId )
                , ( "SystemId", starSystemIdEncoder article.systemId )
                , ( "PlanetName", Encode.string article.planetName )
                , ( "SystemName", Encode.string article.systemName )
                , ( "Date", starDateEncoder newsArticle.starDate )
                , ( "Options", Encode.list Encode.string [] ) -- not used by server
                , ( "Choice"
                  , case newsArticle.choice of
                        Just x ->
                            specialEventChoiceEncoder x

                        Nothing ->
                            Encode.null
                  )
                ]

        _ ->
            Debug.todo "not implemented"


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


{-| Decoder for news article
-}
newsDecoder : Decode.Decoder NewsArticle
newsDecoder =
    let
        body header =
            newsContentDecoder
                |> andThen (result header)

        result header content =
            succeed
                { messageId = header.id
                , starDate = header.starDate
                , icon = header.icon
                , content = content
                , options = header.options
                , choice = header.choice
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
        , when newsType (is "KragiiEvent") (field "contents" kragiiEventNewsArticle)
        , when newsType (is "KragiiResolution") (field "contents" kragiiResolutionNewsArticle)
        , when newsType (is "ProductionBoostStarted") (field "contents" productionBoostStarted)
        , when newsType (is "ProductionSlowdownStarted") (field "contents" productionSlowdownStarted)
        , when newsType (is "ProductionBoostEnded") (field "contents" productionBoostEnded)
        , when newsType (is "ProductionSlowdownEnded") (field "contents" productionSlowdownEnded)
        , when newsType (is "ResearchCompleted") (field "contents" researchCompleted)
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
newsHeader : Decode.Decoder NewsHeader
newsHeader =
    succeed NewsHeader
        |> andMap (field "icon" string)
        |> andMap (field "id" messageId)
        |> andMap (field "starDate" starDateDecoder)
        |> andMap (field "contents" (field "Options" (list specialEventOptionDecoder)) |> withDefault [])
        |> andMap (field "contents" (field "Choice" (maybe specialEventChoiceDecoder)) |> withDefault Nothing)


type alias NewsHeader =
    { icon : String
    , id : MessageId
    , starDate : StarDate
    , options : List SpecialEventOption
    , choice : Maybe SpecialEventChoice
    }


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
    let
        decoder =
            succeed DesignCreatedNews
                |> andMap (field "designId" designIdDecoder)
                |> andMap (field "name" designNameDecoder)
    in
    succeed DesignCreated
        |> andMap decoder


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


kragiiEventNewsArticle : Decode.Decoder NewsContent
kragiiEventNewsArticle =
    let
        decoder =
            succeed KragiiSpecialEvent
                |> andMap (field "PlanetName" string)
                |> andMap (field "PlanetId" planetIdDecoder)
                |> andMap (field "SystemName" string)
                |> andMap (field "SystemId" starSystemIdDecoder)

        -- |> andMap (field "Options" (list specialEventOptionDecoder))
    in
    succeed KragiiEvent
        |> andMap decoder


{-| Decoder for news detailing how kragii attack was handled
-}
kragiiResolutionNewsArticle : Decode.Decoder NewsContent
kragiiResolutionNewsArticle =
    let
        decoder =
            succeed KragiiResolution
                |> andMap (field "PlanetName" string)
                |> andMap (field "PlanetId" planetIdDecoder)
                |> andMap (field "SystemName" string)
                |> andMap (field "SystemId" starSystemIdDecoder)
                |> andMap (field "Resolution" string)
    in
    succeed KragiiResolved
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


specialEventOptionDecoder : Decode.Decoder SpecialEventOption
specialEventOptionDecoder =
    succeed SpecialEventOption
        |> andMap (field "Title" string)
        |> andMap (field "Explanation" (list string))
        |> andMap (field "Choice" specialEventChoiceDecoder)


specialEventChoiceDecoder : Decode.Decoder SpecialEventChoice
specialEventChoiceDecoder =
    succeed SpecialEventChoice
        |> andMap string


specialEventChoiceEncoder : SpecialEventChoice -> Encode.Value
specialEventChoiceEncoder choice =
    Encode.string (unSpecialEventChoice choice)


productionChangeDecoder : Decode.Decoder ProductionChangeNews
productionChangeDecoder =
    succeed ProductionChangeNews
        |> andMap (field "PlanetName" string)
        |> andMap (field "PlanetId" planetIdDecoder)
        |> andMap (field "SystemName" string)
        |> andMap (field "SystemId" starSystemIdDecoder)
        |> andMap (field "Type" resourceTypeDecoder)


productionBoostStarted : Decode.Decoder NewsContent
productionBoostStarted =
    succeed ProductionBoostStarted
        |> andMap productionChangeDecoder


productionSlowdownStarted : Decode.Decoder NewsContent
productionSlowdownStarted =
    succeed ProductionSlowdownStarted
        |> andMap productionChangeDecoder


productionBoostEnded : Decode.Decoder NewsContent
productionBoostEnded =
    succeed ProductionBoostEnded
        |> andMap productionChangeDecoder


productionSlowdownEnded : Decode.Decoder NewsContent
productionSlowdownEnded =
    succeed ProductionSlowdownEnded
        |> andMap productionChangeDecoder


researchCompleted : Decode.Decoder NewsContent
researchCompleted =
    let
        decoder =
            succeed ResearchCompletedNews
                |> andMap (field "Name" string)
    in
    succeed ResearchCompleted
        |> andMap decoder
