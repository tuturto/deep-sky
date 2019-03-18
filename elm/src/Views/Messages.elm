module Views.Messages exposing (init, page, update)

import Accessors exposing (get, over, set)
import Accessors.Library exposing (onEach, try)
import Api.Messages
    exposing
        ( deleteNews
        , getIcons
        , getNews
        , postNews
        , putNews
        )
import Data.Accessors
    exposing
        ( activeUserIconA
        , choiceA
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
        , ResourceType(..)
        , Route(..)
        , maxPage
        , unStarDate
        )
import Data.Messages
    exposing
        ( NewsArticle
        , NewsContent(..)
        , SpecialEventChoice
        , SpecialEventOption
        , UserIcon(..)
        , unSpecialEventChoice
        )
import Data.Model exposing (Model, Msg(..))
import Data.User exposing (Role(..), unUserName)
import Data.Vehicles exposing (unDesignName)
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
        , span
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
                        |> List.drop (get (messagesRA << pageSizeA) model * get (messagesRA << currentPageA) model)
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
    let
        titleRow =
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
            ]

        bodyRow =
            [ div [ class "row" ]
                [ div [ class "col-lg-12 news-body" ]
                    (newsBody article)
                ]
            ]

        optionsRows =
            List.map (optionSelection article) <| article.options
    in
    div [ class "row news-article" ]
        [ div [ class "col-lg-2" ]
            [ img
                [ class "news-icon"
                , src <| article.icon
                ]
                []
            ]
        , div [ class "col-lg-10" ] <|
            titleRow
                ++ bodyRow
                ++ optionsRows
        ]


optionSelection : NewsArticle -> SpecialEventOption -> Html Msg
optionSelection article option =
    let
        currentlySelected =
            case article.choice of
                Just x ->
                    unSpecialEventChoice x == unSpecialEventChoice option.choice

                Nothing ->
                    False

        titleBlock =
            if currentlySelected then
                [ text option.title
                , i [ class "fas fa-check-circle" ] []
                ]

            else
                [ span [ onClick (NewsMessage <| SpecialEventChoiceMade (set choiceA (Just option.choice) article)) ]
                    [ text option.title ]
                ]
    in
    div [ class "row" ]
        [ div [ class "col-lg-12" ]
            ([ div [ class "row" ]
                [ div [ class "col-lg-12" ]
                    titleBlock
                ]
             ]
                ++ List.map optionDescription option.explanation
            )
        ]


optionDescription : String -> Html Msg
optionDescription desc =
    div [ class "row option-desc" ]
        [ div [ class "col-lg-12" ]
            [ text desc ]
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

        ProductionBoostStarted _ ->
            text "Production is booming"

        ProductionSlowdownStarted _ ->
            text "Production in downturn"

        ProductionBoostEnded _ ->
            text "Production returns normal"

        ProductionSlowdownEnded _ ->
            text "Production returns normal"

        KragiiEvent _ ->
            text "Kragii sighting"

        KragiiResolved _ ->
            text "Kragii situation develops"

        ResearchCompleted _ ->
            text "Research completed"


{-| Create body of news article
-}
newsBody : NewsArticle -> List (Html Msg)
newsBody article =
    case article.content of
        PlanetFound details ->
            [ text "A new planet has been discovered in "
            , a [ href (StarSystemR details.systemId) ] [ text details.systemName ]
            , text " and has been named to "
            , a [ href (PlanetR details.systemId details.planetId) ] [ text details.planetName ]
            , text "."
            ]

        StarFound details ->
            [ text "A new star has been discovered in "
            , a [ href (StarSystemR details.systemId) ] [ text details.systemName ]
            , text "."
            ]

        UserWritten details ->
            [ text details.message ]

        DesignCreated details ->
            [ text "A new design called "
            , text <| unDesignName details.name
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

        ProductionBoostStarted details ->
            let
                productionType =
                    case details.resourceType of
                        BiologicalResource ->
                            "biologicals"

                        MechanicalResource ->
                            "mechanicals"

                        ChemicalResource ->
                            "chemicals"
            in
            [ text "Production of "
            , text productionType
            , text " on "
            , a [ href (PlanetR details.systemId details.planetId) ] [ text details.planetName ]
            , text " is booming and record results are expected."
            ]

        ProductionSlowdownStarted details ->
            let
                productionType =
                    case details.resourceType of
                        BiologicalResource ->
                            "biologicals"

                        MechanicalResource ->
                            "mechanicals"

                        ChemicalResource ->
                            "chemicals"
            in
            [ text "Production of "
            , text productionType
            , text " on "
            , a [ href (PlanetR details.systemId details.planetId) ] [ text details.planetName ]
            , text " is slowing down."
            ]

        ProductionBoostEnded details ->
            let
                productionType =
                    case details.resourceType of
                        BiologicalResource ->
                            "biologicals"

                        MechanicalResource ->
                            "mechanicals"

                        ChemicalResource ->
                            "chemicals"
            in
            [ text "Recent boom in production of "
            , text productionType
            , text " on "
            , a [ href (PlanetR details.systemId details.planetId) ] [ text details.planetName ]
            , text " has come to end and production levels are returning to normal."
            ]

        ProductionSlowdownEnded details ->
            let
                productionType =
                    case details.resourceType of
                        BiologicalResource ->
                            "biologicals"

                        MechanicalResource ->
                            "mechanicals"

                        ChemicalResource ->
                            "chemicals"
            in
            [ text "Recent downturn in production of "
            , text productionType
            , text " on "
            , a [ href (PlanetR details.systemId details.planetId) ] [ text details.planetName ]
            , text " has come to end and production levels are returning to normal."
            ]

        KragiiEvent details ->
            [ text "Large amount of kragii worms has been sighted on "
            , a [ href (PlanetR details.systemId details.planetId) ] [ text details.planetName ]
            , text ". As the situation develops, decision what to do about them needs to be made."
            ]

        KragiiResolved details ->
            [ text "Report from "
            , a [ href (PlanetR details.systemId details.planetId) ] [ text details.planetName ]
            , text ": \""
            , text details.report
            , text "\""
            ]

        ResearchCompleted details ->
            [ text <| "Research of " ++ details.name ++ " has been completed." ]


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

        SpecialEventChoiceMade article ->
            ( model
            , putNews article
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
