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
  div [ ]
    (List.map searchResult model.availableBuildings)

searchResult : BuildingInfo -> Html Msg
searchResult building =
  div [ class "row" ]
  [ div [ class "col-lg-6" ]
    [ text <| building.name ++ " (" ++ (toString building.level) ++ ")" ]
  , div [ class "col-lg-6" ]
    [ costDisplay building.cost ]
  ]

costDisplay : Cost -> Html Msg
costDisplay cost =
  div [ ]
  [ i [ class "fas fa-leaf" ] []
  , text <| " " ++ (toString <| cost.biological) ++ " "
  , i [ class "fas fa-cogs" ] []
  , text <| " " ++ (toString <| cost.mechanical) ++ " "
  , i [ class "fas fa-flask" ] []
  , text <| " " ++ (toString <| cost.chemical) ++ " "
  ]