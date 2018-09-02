module Render exposing ( view )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput )
import Types exposing (..)

view : Model -> Html Msg
view model =
  div []
  [ currentQueue model
  , searchField model
  , pagingControls model
  , searchResults model
  ]

currentQueue : Model -> Html Msg
currentQueue model =
  div [ class "row" ] 
  [ div [ class "col-lg-12" ]
    [ text "Current queue"
    ]
  ]

searchField : Model -> Html Msg
searchField model =
  div [ class "row" ]
  [ div [ class "col-lg-6" ]
    [ input [ type_ "text", placeholder "Search", value model.searchText, onInput TextSearch, style [ ("width", "100%") ] ] [] ]
  , i [ class "fas fa-times-circle" ] []
  ]

pagingControls : Model -> Html Msg
pagingControls model =
  div [ class "row" ]
  [ div [ class "col-lg-12" ]
    [ i [ class "fas fa-angle-double-left" ] []
    , text " "
    , i [ class "fas fa-angle-left" ] []
    , text " 1 / 1 "
    , i [ class "fas fa-angle-right" ] []
    , text " "
    , i [ class "fas fa-angle-double-right" ] []
    ]
  ]

searchResults : Model -> Html Msg
searchResults model =
  div [ class "row" ]
  [ div [ class "col-lg-12" ]
    [ text "Search results"
    ]
  ]