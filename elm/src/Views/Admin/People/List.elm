module Views.Admin.People.List exposing (init, isReady, page, update)

import Accessors exposing (over, set)
import Api.Admin exposing (getPeople, getSimulationStatus)
import Data.Accessors
    exposing
        ( adminListPeopleRA
        , adminRA
        , currentPageA
        , errorsA
        , peopleA
        )
import Data.Admin exposing (Person)
import Data.Common
    exposing
        ( Route(..)
        , error
        , unPersonId
        )
import Data.Model exposing (Model, Msg(..))
import Data.PersonNames exposing (displayName)
import Dict
import Html
    exposing
        ( Html
        , a
        , div
        , i
        , span
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (class, colspan, id)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..), WebData)
import ViewModels.Admin.Main exposing (AdminRMsg(..))
import ViewModels.Admin.People.List exposing (AdminListPeopleRMsg(..))
import Views.Admin.Main
import Views.Admin.Menu exposing (adminLayout, personMenu)
import Views.Helpers exposing (href, pushUrl)


{-| Render list people view
-}
page : Model -> Html Msg
page model =
    adminLayout personMenu
        [ listControls model
        , listDisplay model
        ]
        model


{-| Render control block
-}
listControls : Model -> Html Msg
listControls model =
    let
        n =
            model.adminR.adminListPeopleR.currentPage

        lastPage =
            Dict.filter onlyAvailablePages model.adminR.adminListPeopleR.people
                |> Dict.keys
                |> List.maximum
                |> Maybe.withDefault 0
    in
    div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ a [ href AdminNewPersonR ]
                [ i [ id "add-new-person", class "fas fa-plus-square" ] [] ]
            , i [ id "first-page", class "fas fa-fast-backward small-space-left", onClick (AdminListPeopleMessage <| PageRequested 0) ] []
            , i [ id "previous-page", class "fas fa-step-backward small-space-left", onClick (AdminListPeopleMessage <| PageRequested (n - 1)) ] []
            , span [ id "current-page", class "small-space-left" ]
                [ text <| String.fromInt (n + 1)
                , text " / "
                , text <| String.fromInt (lastPage + 1)
                ]
            , i [ id "next-page", class "fas fa-step-forward small-space-left", onClick (AdminListPeopleMessage <| PageRequested (n + 1)) ] []
            , i [ id "last-page", class "fas fa-fast-forward small-space-left", onClick (AdminListPeopleMessage <| PageRequested lastPage) ] []
            ]
        ]


{-| Filter dictionary for pages that are available
-}
onlyAvailablePages : Int -> WebData (List a) -> Bool
onlyAvailablePages _ val =
    case val of
        Success people ->
            List.length people > 0

        Failure _ ->
            False

        NotAsked ->
            False

        Loading ->
            False


{-| Render search results
-}
listDisplay : Model -> Html Msg
listDisplay model =
    let
        pageNumber =
            model.adminR.adminListPeopleR.currentPage

        currentPage =
            Dict.get pageNumber model.adminR.adminListPeopleR.people

        content =
            case currentPage of
                Nothing ->
                    [ tr []
                        [ td [ colspan 2, class "noData" ] [ text "No data" ] ]
                    ]

                Just NotAsked ->
                    [ tr []
                        [ td [ colspan 2, class "noData" ] [ text "No data" ] ]
                    ]

                Just Loading ->
                    [ tr []
                        [ td [ colspan 2, class "noData" ] [] ]
                    ]

                Just (Failure _) ->
                    [ tr []
                        [ td [ colspan 2, class "noData" ] [ text "Failed to load page" ] ]
                    ]

                Just (Success people) ->
                    List.map personEntry people
    in
    div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ table []
                [ thead []
                    [ tr []
                        [ th [] [ text "Id" ]
                        , th [] [ text "Name" ]
                        ]
                    ]
                , tbody [] content
                ]
            ]
        ]


{-| Single row in person table
-}
personEntry : Person -> Html Msg
personEntry person =
    tr [ class "clickable", onClick <| AdminListPeopleMessage <| PersonSelected person.id ]
        [ td [] [ text <| String.fromInt <| unPersonId person.id ]
        , td [] [ text <| displayName person.name ]
        ]


{-| Initialize data retrieval from server
-}
init : Model -> Cmd Msg
init model =
    let
        size =
            model.adminR.adminListPeopleR.pageSize
    in
    Cmd.batch
        [ getPeople (AdminListPeopleMessage << PeopleReceived) (Just 0) (Just size)
        , getPeople (AdminListPeopleMessage << PeopleReceived) (Just size) (Just size)
        , getSimulationStatus (AdminMessage << SimulationStatusReceived)
        ]


isReady : Model -> Bool
isReady model =
    let
        vm =
            model.adminR.adminListPeopleR
    in
    (Dict.get vm.currentPage vm.people
        |> Maybe.withDefault NotAsked
        |> RemoteData.isLoading
        |> not
    )
        && Views.Admin.Main.isReady model


{-| Handle incoming messages
-}
update : AdminListPeopleRMsg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PeopleReceived (Success res) ->
            ( over (adminRA << adminListPeopleRA << peopleA) (Dict.insert res.page (Success res.results)) model
            , Cmd.none
            )

        PeopleReceived (Failure err) ->
            ( over errorsA (\errors -> error err "Failed to load people" :: errors) model
            , Cmd.none
            )

        PeopleReceived _ ->
            ( model
            , Cmd.none
            )

        PersonSelected pId ->
            ( model
            , pushUrl model (AdminPersonR pId)
            )

        PageRequested n ->
            let
                people =
                    model.adminR.adminListPeopleR.people

                size =
                    model.adminR.adminListPeopleR.pageSize

                maxPage =
                    Dict.keys people
                        |> List.maximum
                        |> Maybe.withDefault 0

                clamped =
                    if n < 0 then
                        0

                    else if n > maxPage then
                        maxPage

                    else
                        n

                nextPage =
                    clamped + 1
            in
            case Dict.get clamped people of
                Nothing ->
                    ( set (adminRA << adminListPeopleRA << currentPageA) clamped model
                        |> over (adminRA << adminListPeopleRA << peopleA) (Dict.insert clamped Loading)
                    , getPeople (AdminListPeopleMessage << PeopleReceived) (Just (clamped * size)) (Just size)
                    )

                Just (Success p) ->
                    if List.length p > 0 then
                        case Dict.get (clamped + 1) people of
                            Nothing ->
                                ( set (adminRA << adminListPeopleRA << currentPageA) clamped model
                                    |> over (adminRA << adminListPeopleRA << peopleA) (Dict.insert nextPage Loading)
                                , getPeople (AdminListPeopleMessage << PeopleReceived) (Just (nextPage * size)) (Just size)
                                )

                            Just (Success _) ->
                                ( set (adminRA << adminListPeopleRA << currentPageA) clamped model
                                , Cmd.none
                                )

                            Just NotAsked ->
                                ( set (adminRA << adminListPeopleRA << currentPageA) clamped model
                                    |> over (adminRA << adminListPeopleRA << peopleA) (Dict.insert nextPage Loading)
                                , getPeople (AdminListPeopleMessage << PeopleReceived) (Just (nextPage * size)) (Just size)
                                )

                            Just Loading ->
                                ( set (adminRA << adminListPeopleRA << currentPageA) clamped model
                                , Cmd.none
                                )

                            Just (Failure _) ->
                                ( set (adminRA << adminListPeopleRA << currentPageA) clamped model
                                , Cmd.none
                                )

                    else
                        ( model
                        , Cmd.none
                        )

                Just NotAsked ->
                    ( set (adminRA << adminListPeopleRA << currentPageA) clamped model
                        |> over (adminRA << adminListPeopleRA << peopleA) (Dict.insert clamped Loading)
                    , getPeople (AdminListPeopleMessage << PeopleReceived) (Just (clamped * size)) (Just size)
                    )

                Just Loading ->
                    ( set (adminRA << adminListPeopleRA << currentPageA) clamped model
                    , Cmd.none
                    )

                Just (Failure _) ->
                    ( model
                    , Cmd.none
                    )
