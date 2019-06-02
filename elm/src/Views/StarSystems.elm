module Views.StarSystems exposing (init, page)

import Api.StarSystem exposing (getStarSystemsCmd)
import Data.Common
    exposing
        ( Location(..)
        , Route(..)
        , locationToString
        , unStarSystemName
        )
import Data.Model exposing (Model, Msg(..))
import Data.StarSystem exposing (StarSystem)
import Data.User exposing (Role(..))
import Dict exposing (Dict)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class)
import Maybe exposing (andThen, withDefault)
import Views.Helpers exposing (href, starDateToString)


page : Model -> Html Msg
page model =
    let
        header =
            div [ class "row" ]
                [ div [ class "col-lg-2" ]
                    [ text "Name" ]
                , div [ class "col-lg-1" ]
                    [ text "Location" ]
                , div [ class "col-lg-2" ]
                    [ text "Date" ]
                ]

        data =
            model.starSystems
                |> andThen (\x -> Just <| Dict.values x)
                |> andThen (\x -> Just <| List.map systemRow x)
                |> withDefault []
    in
    div []
        (header :: data)


init : Model -> Cmd Msg
init model =
    getStarSystemsCmd model


systemRow : StarSystem -> Html Msg
systemRow starSystem =
    div [ class "row" ]
        [ div [ class "col-lg-2" ]
            [ a [ href (StarSystemR starSystem.id) ] [ text (unStarSystemName starSystem.name) ] ]
        , div [ class "col-lg-1" ]
            [ text <| locationToString starSystem.location ]
        , div [ class "col-lg-2" ]
            [ text <| starDateToString starSystem.date ]
        ]
