module Views.Admin.People.List exposing (init, page, update)

import Accessors exposing (over)
import Api.Admin exposing (getPeople, getSimulationStatus)
import Browser.Navigation exposing (pushUrl)
import Data.Accessors
    exposing
        ( adminListPeopleRA
        , adminRA
        , errorsA
        , peopleA
        )
import Data.Admin exposing (Person)
import Data.Common
    exposing
        ( Route(..)
        , error
        , routeToString
        , unPersonId
        )
import Data.Model exposing (Model, Msg(..))
import Data.People exposing (displayName)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Maybe.Extra
import ViewModels.Admin.Main exposing (AdminRMsg(..))
import ViewModels.Admin.People.List exposing (AdminListPeopleRMsg(..))
import Views.Admin.Menu exposing (adminLayout, personMenu)
import Views.Helpers exposing (href)


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
    div [ class "row" ]
        [ div [ class "col-lg-12" ]
            [ a [ href AdminNewPersonR ]
                [ i [ class "fas fa-plus-square" ] [] ]
            ]
        ]


{-| Render search results
-}
listDisplay : Model -> Html Msg
listDisplay model =
    let
        pageNumber =
            model.adminR.adminListPeopleR.currentPage

        fetchInitiated =
            Dict.member pageNumber model.adminR.adminListPeopleR.people

        currentPage =
            Maybe.Extra.join <| Dict.get pageNumber model.adminR.adminListPeopleR.people

        content =
            case fetchInitiated of
                False ->
                    [ tr []
                        [ td [] [ text "No data" ] ]
                    ]

                True ->
                    case currentPage of
                        Nothing ->
                            [ tr []
                                [ td [] [ text "Loading ..." ] ]
                            ]

                        Just people ->
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
    Cmd.batch
        [ getPeople (AdminListPeopleMessage << PeopleReceived) (Just 0) (Just 50)
        , getSimulationStatus (AdminMessage << SimulationStatusReceived)
        ]


{-| Handle incoming messages
-}
update : AdminListPeopleRMsg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PeopleReceived (Ok res) ->
            ( over (adminRA << adminListPeopleRA << peopleA) (Dict.insert res.page (Just res.results)) model
            , Cmd.none
            )

        PeopleReceived (Err err) ->
            ( over errorsA (\errors -> error err "Failed to load people" :: errors) model
            , Cmd.none
            )

        PersonSelected pId ->
            ( model
            , pushUrl model.key (routeToString <| AdminPersonR pId)
              -- TODO: typesafe pushUrl
            )
