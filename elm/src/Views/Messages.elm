module Views.Messages exposing (init, page, update)

import Accessors exposing (get, over, set)
import Accessors.Library exposing (onEach, try)
import Api.Messages
    exposing
        ( deleteNews
        , getIcons
        , getNews
        , postNews
        )
import Data.Accessors
    exposing
        ( activeUserIconA
        , currentPageA
        , iconsA
        , messagesRA
        , newsA
        , newsPanelStatusA
        , pageSizeA
        , userEntryA
        , userEntryStatusA
        )
import Data.Common
    exposing
        ( InfoPanelStatus(..)
        , Route(..)
        , maxPage
        , unStarDate
        )
import Data.Messages exposing (NewsArticle, NewsContent(..), UserIcon(..))
import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..), unUserName)
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , hr
        , i
        , img
        , li
        , text
        , textarea
        , ul
        )
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import ViewModels.Messages exposing (MessagesRMsg(..))
import Views.Helpers
    exposing
        ( PanelSizing(..)
        , href
        , infoPanel
        , starDateToText
        , twinPanels
        )


{-| Render the page
-}
page : Model -> Html Msg
page model =
    div [] <|
        twinPanels RightPanelSmall
            (infoPanel
                { title = "Latest news"
                , currentStatus = get (messagesRA << newsPanelStatusA) model
                , openingMessage = NewsMessage <| NewsPanelStatusChanged InfoPanelOpen
                , closingMessage = NewsMessage <| NewsPanelStatusChanged InfoPanelClosed
                , refreshMessage = Just <| NewsMessage <| NewsPanelRefresh
                }
                (Just
                    { pageSize = get (messagesRA << pageSizeA) model
                    , currentPage = get (messagesRA << currentPageA) model
                    , maxPage =
                        get newsA model
                            |> withDefault []
                            |> maxPage (get (messagesRA << pageSizeA) model)
                    , pageChangedMessage = NewsMessage << PageChanged
                    }
                )
                leftPanel
            )
            (infoPanel
                { title = "Submit news"
                , currentStatus = get (messagesRA << userEntryStatusA) model
                , openingMessage = NewsMessage <| UserEntryPanelChanged InfoPanelOpen
                , closingMessage = NewsMessage <| UserEntryPanelChanged InfoPanelClosed
                , refreshMessage = Nothing
                }
                Nothing
                rightPanel
            )
            model


{-| Render left side of the page
-}
leftPanel : Model -> List (Html Msg)
leftPanel model =
    case get newsA model of
        Nothing ->
            []

        Just entries ->
            let
                shownEntries =
                    List.sortWith descendingStarDate entries
                        |> List.drop (5 * get (messagesRA << currentPageA) model)
                        |> List.take (get (messagesRA << pageSizeA) model)
            in
            List.map newsEntry shownEntries


{-| Sort by star date in descending order
-}
descendingStarDate a b =
    case compare (unStarDate a.starDate) (unStarDate b.starDate) of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            EQ


{-| Render single news article
-}
newsEntry : NewsArticle -> Html Msg
newsEntry article =
    div [ class "row news-article" ]
        [ div [ class "col-lg-2" ]
            [ img
                [ class "news-icon"
                , src <| article.icon
                ]
                []
            ]
        , div [ class "col-lg-10" ]
            [ div [ class "row" ]
                [ div [ class "col-lg-11" ]
                    [ ul [ class "news-title" ]
                        [ li [] [ starDateToText <| Just article.starDate ]
                        , li [] [ newsTitle article ]
                        ]
                    ]
                , div [ class "col-lg-1" ]
                    [ i [ class "fas fa-trash-alt news-title-button", onClick (NewsMessage <| NewsEntryDismissed article) ] [] ]
                ]
            , div [ class "row" ]
                [ div [ class "col-lg-12 news-body" ]
                    (newsBody article)
                ]
            ]
        ]


{-| Create title for news article
-}
newsTitle : NewsArticle -> Html Msg
newsTitle article =
    case article.content of
        PlanetFound _ ->
            text "New planet has been discovered!"

        StarFound _ ->
            text "New star has been discovered!"

        UserWritten details ->
            text <| unUserName details.author ++ " writes"

        DesignCreated _ ->
            text "New design has been approved"

        BuildingFinished _ ->
            text "Construction finished"

        ShipFinished _ ->
            text "Construction finished"


{-| Create body of news article
-}
newsBody : NewsArticle -> List (Html Msg)
newsBody article =
    case article.content of
        PlanetFound details ->
            [ text "a new planet has been discovered in "
            , a [ href (StarSystemR details.systemId) ] [ text details.systemName ]
            , text " and has been named to "
            , a [ href (PlanetR details.systemId details.planetId) ] [ text details.planetName ]
            , text "."
            ]

        StarFound details ->
            [ text "a new star has been discovered in "
            , a [ href (StarSystemR details.systemId) ] [ text details.systemName ]
            , text "."
            ]

        UserWritten details ->
            [ text details.message ]

        DesignCreated details ->
            [ text "a new design called "
            , text details.name
            , text " has been submitted and approved."
            ]

        BuildingFinished details ->
            [ text details.name
            , text " has been finished in "
            , a [ href (PlanetR details.systemId details.planetId) ] [ text details.planetName ]
            , text "."
            ]

        ShipFinished _ ->
            []


{-| Render right side of the page
-}
rightPanel : Model -> List (Html Msg)
rightPanel model =
    let
        buttonAttributes =
            case get (messagesRA << activeUserIconA) model of
                Nothing ->
                    [ class "btn btn-primary disabled pull-right"
                    ]

                Just icon ->
                    [ class "btn btn-primary pull-right"
                    , onClick (NewsMessage <| UserMessageSent icon (get (messagesRA << userEntryA) model))
                    ]
    in
    [ div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ textarea
                [ rows 6
                , maxlength 200
                , class "user-news-input"
                , onInput (NewsMessage << UserMessageTextChanged)
                ]
                []
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-12" ] <|
            iconBar model
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ button buttonAttributes [ text "Submit" ]
            ]
        ]
    ]


iconBar : Model -> List (Html Msg)
iconBar model =
    let
        activeButton =
            get (messagesRA << activeUserIconA) model
    in
    case get iconsA model of
        Nothing ->
            []

        Just icons ->
            List.map (iconInfo activeButton) icons


iconInfo : Maybe UserIcon -> ( UserIcon, String ) -> Html Msg
iconInfo selected ( icon, link ) =
    let
        passiveIcon =
            img
                [ class "news-icon-thumbnail"
                , src link
                , onClick ((NewsMessage << UserIconSelected) icon)
                ]
                []
    in
    case selected of
        Nothing ->
            passiveIcon

        Just activeIcon ->
            if icon == activeIcon then
                img [ class "news-icon-thumbnail-active", src link ] []

            else
                passiveIcon


{-| Initiate retrieval of data needed by this page
-}
init : Model -> Cmd Msg
init model =
    Cmd.batch
        [ getNews
        , getIcons model
        ]


{-| Handle messages specific to this page
-}
update : MessagesRMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewsPanelStatusChanged status ->
            ( set (messagesRA << newsPanelStatusA) status model
            , Cmd.none
            )

        UserEntryPanelChanged status ->
            ( set (messagesRA << userEntryStatusA) status model
            , Cmd.none
            )

        UserMessageTextChanged entry ->
            ( set (messagesRA << userEntryA) entry model
            , Cmd.none
            )

        UserIconSelected icon ->
            ( set (messagesRA << activeUserIconA) (Just icon) model
            , Cmd.none
            )

        UserMessageSent icon message ->
            ( model
            , postNews message icon
            )

        NewsEntryDismissed article ->
            ( model
            , deleteNews article.messageId
            )

        NewsPanelRefresh ->
            ( model
            , getNews
            )

        PageChanged pageNumber ->
            let
                lastPgNumber =
                    get newsA model
                        |> withDefault []
                        |> maxPage (get (messagesRA << pageSizeA) model)

                setPage target _ =
                    if target < 0 then
                        0

                    else if target > lastPgNumber then
                        lastPgNumber

                    else
                        target
            in
            ( over (messagesRA << currentPageA) (setPage pageNumber) model
            , Cmd.none
            )
