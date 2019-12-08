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
        , planetNameDecoder
        , planetNameEncoder
        , post
        , put
        , resourceTypeDecoder
        , starDateDecoder
        , starDateEncoder
        , starNameDecoder
        , starSystemIdDecoder
        , starSystemIdEncoder
        , starSystemNameDecoder
        , starSystemNameEncoder
        )
import Api.Designer exposing (designIdDecoder, designNameDecoder)
import Api.Endpoints exposing (Endpoint(..))
import Api.People
    exposing
        ( personIdDecoder
        , personIdEncoder
        , personNameDecoder
        , petIdDecoder
        , petIdEncoder
        , petTypeDecoder
        , petTypeEncoder
        )
import Api.StarSystem exposing (buildingIdDecoder)
import Api.User
    exposing
        ( factionIdDecoder
        , factionIdEncoder
        )
import Data.Accessors exposing (iconsA)
import Data.Common exposing (MessageId(..), StarDate(..))
import Data.Messages
    exposing
        ( BuildingFinishedNews
        , DesignCreatedNews
        , EventResolveType(..)
        , KragiiResolution
        , KragiiSpecialEvent
        , NamingPetResolution
        , NamingPetSpecialEvent
        , NewsArticle
        , NewsContent(..)
        , PlanetFoundNews
        , ProductionChangeNews
        , ResearchCompletedNews
        , ScurryingSoundsResolution
        , ScurryingSoundsSpecialEvent
        , SpecialEventChoice(..)
        , SpecialEventOption
        , StarFoundNews
        , UserIcon(..)
        , UserWrittenNews
        )
import Data.Model exposing (ApiMsg(..), Model, Msg(..))
import Data.PersonNames exposing (FirstName(..), PersonName(..))
import Data.User exposing (UserName(..))
import Http
import Json.Decode as Decode
    exposing
        ( andThen
        , fail
        , field
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
                , author = SimpleName (FirstName "") Nothing -- Filled in by server
                , icon = icon
                }

        news =
            { messageId = MessageId 0 -- Filled in by server
            , starDate = StarDate 0 -- Filled in by server
            , icon = "" -- Filled in by server
            , content = content
            , options = [] -- Filled in by server
            , choice = Nothing -- Not used in user written entries
            , resolveType = Nothing -- Not used in user written entries
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
        , ( "resolveType"
          , case article.resolveType of
                Nothing ->
                    Encode.null

                Just rt ->
                    eventResolveTypeEncoder rt
          )
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

        ScurryingSoundsEvent _ ->
            "ScurryingSoundsEvent"

        ScurryingSoundsResolved _ ->
            "ScurryingSoundsResolution"

        NamingPetEvent _ ->
            "NamingPetEvent"

        PetNamingResolved _ ->
            "NamingPetResolution"


newsContentEncoder : NewsArticle -> Encode.Value
newsContentEncoder newsArticle =
    case newsArticle.content of
        UserWritten article ->
            Encode.object
                [ ( "Content", Encode.string article.message )
                , ( "Date", starDateEncoder newsArticle.starDate )
                , ( "UserName", Encode.null ) -- filled in by server
                , ( "Icon", userIconEncoder article.icon )
                ]

        KragiiEvent article ->
            Encode.object
                [ ( "PlanetId", planetIdEncoder article.planetId )
                , ( "SystemId", starSystemIdEncoder article.systemId )
                , ( "PlanetName", planetNameEncoder article.planetName )
                , ( "SystemName", starSystemNameEncoder article.systemName )
                , ( "Date", starDateEncoder newsArticle.starDate )
                , ( "FactionId", factionIdEncoder article.factionId )
                , ( "Options", Encode.list Encode.string [] ) -- not used by server
                , choiceEncoder newsArticle
                ]

        ScurryingSoundsEvent article ->
            Encode.object
                [ ( "PersonId", personIdEncoder article.personId )
                , ( "Date", starDateEncoder newsArticle.starDate )
                , ( "Options", Encode.list Encode.string [] ) -- not used by server
                , choiceEncoder newsArticle
                ]

        NamingPetEvent article ->
            Encode.object
                [ ( "PersonId", personIdEncoder article.personId )
                , ( "PetId", petIdEncoder article.petId )
                , ( "PetType", petTypeEncoder article.petType )
                , ( "Date", starDateEncoder newsArticle.starDate )
                , ( "Options", Encode.list Encode.string [] ) -- not used by server
                , choiceEncoder newsArticle
                ]

        _ ->
            Debug.todo "not implemented"


{-| Internal helper for encoding user choice in special events
-}
choiceEncoder : NewsArticle -> ( String, Encode.Value )
choiceEncoder article =
    ( "Choice"
    , case article.choice of
        Just x ->
            specialEventChoiceEncoder x

        Nothing ->
            Encode.null
    )


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
                , resolveType = header.resolveType
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
        , when newsType (is "ScurryingSoundsEvent") (field "contents" scurryingSoundsEventNewsArticle)
        , when newsType (is "ScurryingSoundsResolution") (field "contents" scurryingSoundsResolutionNewsArticle)
        , when newsType (is "NamingPetEvent") (field "contents" namingPetEventNewsArticle)
        , when newsType (is "NamingPetResolution") (field "contents" namingPetResolutionNewsArticle)
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
                |> andMap (field "StarName" starNameDecoder)
                |> andMap (field "SystemName" starSystemNameDecoder)
                |> andMap (field "SystemId" starSystemIdDecoder)
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
                |> andMap (field "PlanetName" planetNameDecoder)
                |> andMap (field "SystemName" starSystemNameDecoder)
                |> andMap (field "SystemId" starSystemIdDecoder)
                |> andMap (field "PlanetId" planetIdDecoder)
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
                |> andMap (field "UserName" personNameDecoder)
                |> andMap (field "Icon" userIconDecoder)
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
        |> andMap (field "contents" (field "ResolveType" (maybe resolveType)) |> withDefault Nothing)


type alias NewsHeader =
    { icon : String
    , id : MessageId
    , starDate : StarDate
    , options : List SpecialEventOption
    , choice : Maybe SpecialEventChoice
    , resolveType : Maybe EventResolveType
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


{-| Decoder for resolve type
-}
resolveType : Decode.Decoder EventResolveType
resolveType =
    string |> andThen stringToEventResolveType


stringToEventResolveType : String -> Decode.Decoder EventResolveType
stringToEventResolveType s =
    case s of
        "ImmediateEvent" ->
            succeed ImmediateEvent

        "DelayedEvent" ->
            succeed DelayedEvent

        _ ->
            fail "Couldn't parse"


eventResolveTypeEncoder : EventResolveType -> Encode.Value
eventResolveTypeEncoder n =
    case n of
        ImmediateEvent ->
            Encode.string "ImmediateEvent"

        DelayedEvent ->
            Encode.string "DelayedEvent"


{-| Decoder for design created news article
-}
designCreatedNewsArticle : Decode.Decoder NewsContent
designCreatedNewsArticle =
    let
        decoder =
            succeed DesignCreatedNews
                |> andMap (field "DesignId" designIdDecoder)
                |> andMap (field "Name" designNameDecoder)
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
                |> andMap (field "PlanetName" planetNameDecoder)
                |> andMap (field "PlanetId" planetIdDecoder)
                |> andMap (field "SystemName" starSystemNameDecoder)
                |> andMap (field "SystemId" starSystemIdDecoder)
                |> andMap (field "ConstructionName" string)
                |> andMap (field "BuildingId" buildingIdDecoder)
    in
    succeed BuildingFinished
        |> andMap decoder


kragiiEventNewsArticle : Decode.Decoder NewsContent
kragiiEventNewsArticle =
    let
        decoder =
            succeed KragiiSpecialEvent
                |> andMap (field "PlanetName" planetNameDecoder)
                |> andMap (field "PlanetId" planetIdDecoder)
                |> andMap (field "SystemName" starSystemNameDecoder)
                |> andMap (field "SystemId" starSystemIdDecoder)
                |> andMap (field "FactionId" factionIdDecoder)
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
                |> andMap (field "PlanetName" planetNameDecoder)
                |> andMap (field "PlanetId" planetIdDecoder)
                |> andMap (field "SystemName" starSystemNameDecoder)
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
    oneOf
        [ succeed TagAndContents
            |> andMap (field "tag" string)
            |> andMap (field "contents" string)
        , succeed TagOnly
            |> andMap (field "tag" string)
        , succeed EnumOnly
            |> andMap string
        ]


specialEventChoiceEncoder : SpecialEventChoice -> Encode.Value
specialEventChoiceEncoder choice =
    case choice of
        EnumOnly s ->
            Encode.string s

        TagAndContents tag contents ->
            Encode.object
                [ ( "tag", Encode.string tag )
                , ( "contents", Encode.string contents )
                ]

        TagOnly tag ->
            Encode.object
                [ ( "tag", Encode.string tag )
                ]


productionChangeDecoder : Decode.Decoder ProductionChangeNews
productionChangeDecoder =
    succeed ProductionChangeNews
        |> andMap (field "PlanetName" planetNameDecoder)
        |> andMap (field "PlanetId" planetIdDecoder)
        |> andMap (field "SystemName" starSystemNameDecoder)
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


scurryingSoundsEventNewsArticle : Decode.Decoder NewsContent
scurryingSoundsEventNewsArticle =
    let
        decoder =
            succeed ScurryingSoundsSpecialEvent
                |> andMap (field "PersonId" personIdDecoder)
    in
    succeed ScurryingSoundsEvent
        |> andMap decoder


scurryingSoundsResolutionNewsArticle : Decode.Decoder NewsContent
scurryingSoundsResolutionNewsArticle =
    let
        decoder =
            succeed ScurryingSoundsResolution
                |> andMap (field "PetId" (maybe petIdDecoder))
                |> andMap (field "PetType" (maybe petTypeDecoder))
                |> andMap (field "Explanation" string)
    in
    succeed ScurryingSoundsResolved
        |> andMap decoder


namingPetEventNewsArticle : Decode.Decoder NewsContent
namingPetEventNewsArticle =
    let
        decoder =
            succeed NamingPetSpecialEvent
                |> andMap (field "PersonId" personIdDecoder)
                |> andMap (field "PetId" petIdDecoder)
                |> andMap (field "PetType" petTypeDecoder)
    in
    succeed NamingPetEvent
        |> andMap decoder


namingPetResolutionNewsArticle : Decode.Decoder NewsContent
namingPetResolutionNewsArticle =
    let
        decoder =
            succeed NamingPetResolution
                |> andMap (field "PetId" petIdDecoder)
                |> andMap (field "PetType" petTypeDecoder)
                |> andMap (field "Explanation" string)
    in
    succeed PetNamingResolved
        |> andMap decoder
