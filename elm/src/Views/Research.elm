module Views.Research exposing (init, page, update)

import Accessors exposing (get, set)
import Accessors.Library exposing (try)
import Api.Research
    exposing
        ( availableResearchCmd
        , cancelResearchCmd
        , currentResearchCmd
        , researchProductionCmd
        , startResearchCmd
        )
import Data.Accessors
    exposing
        ( currentResearchA
        , currentResearchStatusA
        , focusedTopCategoryA
        , productionStatusA
        , researchFieldStatusA
        , researchRA
        )
import Data.Common exposing (InfoPanelStatus(..))
import Data.Model exposing (Model, Msg(..))
import Data.Research
    exposing
        ( CurrentResearch
        , Research
        , ResearchCategory(..)
        , TopResearchCategory(..)
        , researchToCurrent
        , topResearchCategory
        , topResearchCategoryToString
        , topResearchCategoryToTag
        , unResearchScore
        , unResearchTier
        , unTechnology
        )
import Data.User exposing (Role(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import ViewModels.Research exposing (ResearchRMsg(..))
import Views.Helpers
    exposing
        ( PanelSizing(..)
        , infoPanel
        , twinPanels
        )


{-| Display research page
-}
page : Model -> Html Msg
page model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-lg-12" ] <|
                statusPanel model
            ]
        , div [ class "row" ]
            [ div [ class "col-lg-12" ] <|
                twinPanels EqualPanels leftPanel rightPanel model
            ]
        ]


{-| Display status panel at top of the screen
-}
statusPanel : Model -> List (Html Msg)
statusPanel model =
    infoPanel
        { title = "Production"
        , currentStatus = model.researchR.productionStatus
        , openingMessage = ResearchMessage <| ProductionStatusChanged InfoPanelOpen
        , closingMessage = ResearchMessage <| ProductionStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        productionStatus
        model


productionStatus : Model -> List (Html Msg)
productionStatus model =
    let
        ( eng, nat, soc ) =
            case model.researchProduction of
                Nothing ->
                    ( 0, 0, 0 )

                Just production ->
                    ( unResearchScore production.engineering
                    , unResearchScore production.natural
                    , unResearchScore production.social
                    )
    in
    [ div [ class "row" ]
        [ div [ class "col-lg-2" ]
            [ text <| "Engineering: " ++ String.fromInt eng ]
        , div [ class "col-lg-2" ]
            [ text <| "Natural sciences: " ++ String.fromInt nat ]
        , div [ class "col-lg-2" ]
            [ text <| "Social sciences: " ++ String.fromInt soc ]
        ]
    ]


{-| Display right panel that shows research that is available for selection
-}
rightPanel : Model -> List (Html Msg)
rightPanel model =
    infoPanel
        { title = "Research"
        , currentStatus = model.researchR.currentResearchStatus
        , openingMessage = ResearchMessage <| CurrentResearchDetailsStatusChanged InfoPanelOpen
        , closingMessage = ResearchMessage <| CurrentResearchDetailsStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        availableResearchList
        model


{-| Display left panel, that shows currently ongoing projects
-}
leftPanel : Model -> List (Html Msg)
leftPanel model =
    infoPanel
        { title = "Projects"
        , currentStatus = model.researchR.researchFieldStatus
        , openingMessage = ResearchMessage <| ResearchFieldDetailsStatusChanged InfoPanelOpen
        , closingMessage = ResearchMessage <| ResearchFieldDetailsStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        Nothing
        currentProjects
        model


{-| Display researches that are available for selection
-}
availableResearchList : Model -> List (Html Msg)
availableResearchList model =
    let
        candidates =
            Maybe.map
                (\topCat ->
                    Maybe.withDefault [] model.availableResearch
                        |> List.filter
                            (\res -> topCat == topResearchCategory res.category)
                )
                model.researchR.focusedTopCategory
                |> Maybe.withDefault []
    in
    List.map availableResearch candidates
        ++ availableResearchHelp candidates


{-| Display help text for available research section
-}
availableResearchHelp : List Research -> List (Html Msg)
availableResearchHelp research =
    let
        desc =
            if List.length research > 0 then
                "Click project to start researching it. It will immediately moved into research queue and scientists will start working on it."

            else
                "Currently there is no project that could be initiated"
    in
    [ div [ class "row" ]
        [ div [ id "available-research-help", class "col-lg-12" ] [ text desc ] ]
    ]


{-| Display research that is available for starting
-}
availableResearch : Research -> Html Msg
availableResearch res =
    let
        cost =
            List.maximum
                [ unResearchScore res.cost.engineering
                , unResearchScore res.cost.social
                , unResearchScore res.cost.natural
                ]
                |> Maybe.withDefault 0

        costText =
            String.fromInt cost
                ++ " "
                ++ (topResearchCategoryToString <| topResearchCategory res.category)
    in
    div
        [ class "row current-research", onClick <| ResearchMessage (ProjectStarted res) ]
        [ div [ class "col-lg-12" ]
            [ div [ class "row" ]
                [ div [ class "col-lg-12" ] [ text <| res.name ++ " - tier " ++ (String.fromInt <| unResearchTier res.tier) ] ]
            , div [ class "row" ]
                [ div [ class "col-lg-12" ] [ text <| "Cost: " ++ costText ] ]
            ]
        ]


{-| Display current project. In case where current research is Nothing, use
given top research category to label created button
-}
currentProject : Maybe CurrentResearch -> TopResearchCategory -> Html Msg
currentProject mp cat =
    case mp of
        Nothing ->
            div
                [ class "row current-project", onClick <| ResearchMessage (ProjectFocused (Just cat)) ]
                [ div [ class "col-lg-12 select-project" ]
                    [ text <| "Click to select " ++ topResearchCategoryToString cat ++ " project" ]
                ]

        Just project ->
            let
                cost =
                    List.maximum
                        [ unResearchScore project.research.cost.engineering
                        , unResearchScore project.research.cost.social
                        , unResearchScore project.research.cost.natural
                        ]
                        |> Maybe.withDefault 0

                progressText =
                    "Progress: "
                        ++ String.fromInt (unResearchScore project.progress)
                        ++ " / "
                        ++ String.fromInt cost
                        ++ " "
                        ++ (topResearchCategoryToString <| topResearchCategory project.research.category)
            in
            div
                [ class "row current-project", onClick <| ResearchMessage (ProjectFocused (Just cat)) ]
                [ div [ class "col-lg-11" ]
                    [ div [ class "row" ]
                        [ div [ class "col-lg-12 select-project" ]
                            [ div [ class "row" ]
                                [ div [ class "col-lg-12" ]
                                    [ text project.research.name ]
                                ]
                            , div [ class "row" ]
                                [ div [ class "col-lg-12" ] [ text progressText ] ]
                            ]
                        ]
                    ]
                , div [ class "col-lg-1" ]
                    [ i [ class "fas fa-trash-alt", onClick (ResearchMessage <| ProjectCancelled project) ] [] ]
                ]


{-| Display slots for current projects
Empty slots will have button for selecting a project
Filled slots will display brief overview of the current project
-}
currentProjects : Model -> List (Html Msg)
currentProjects model =
    let
        engProject =
            model.currentResearch
                |> Maybe.map (List.filter (\b -> topResearchCategory b.research.category == Eng))
                |> Maybe.withDefault []
                |> List.head

        natSciProject =
            model.currentResearch
                |> Maybe.map (List.filter (\b -> topResearchCategory b.research.category == NatSci))
                |> Maybe.withDefault []
                |> List.head

        socSciProject =
            model.currentResearch
                |> Maybe.map (List.filter (\b -> topResearchCategory b.research.category == SocSci))
                |> Maybe.withDefault []
                |> List.head
    in
    [ currentProject engProject Eng
    , currentProject natSciProject NatSci
    , currentProject socSciProject SocSci
    , currentProjectsHelp
    ]


currentProjectsHelp : Html Msg
currentProjectsHelp =
    div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ text <|
                "Click project slot to view list of projects available for research. "
                    ++ "Use trash can icon to cancel project that has been initiated. "
                    ++ "Changes are immediate and no confirmation will be asked."
            ]
        ]


{-| Handle incoming messages
-}
update : ResearchRMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentResearchDetailsStatusChanged status ->
            ( set (researchRA << currentResearchStatusA) status model
            , Cmd.none
            )

        ResearchFieldDetailsStatusChanged status ->
            ( set (researchRA << researchFieldStatusA) status model
            , Cmd.none
            )

        ProductionStatusChanged status ->
            ( set (researchRA << productionStatusA) status model
            , Cmd.none
            )

        ProjectFocused category ->
            ( set (researchRA << focusedTopCategoryA) category model
            , Cmd.none
            )

        ProjectStarted research ->
            ( model
            , startResearchCmd <| researchToCurrent research
            )

        ProjectCancelled research ->
            ( model
            , cancelResearchCmd research
            )


{-| Initialize view model
-}
init : Model -> Cmd Msg
init model =
    Cmd.batch
        [ availableResearchCmd
        , currentResearchCmd
        , researchProductionCmd
        ]
