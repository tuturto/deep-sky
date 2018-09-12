module Render exposing ( view )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput )
import Types exposing (..)

view : Model -> Html Msg
view model =
  div [ class "row" ]
  [ div [ class "col-lg-6" ]
    [ planetDetails model 
    , hr [] [] 
    , population model
    , hr [] []
    , constructionQueue model
    , hr [] []
    ]
  , div [ class "col-lg-6" ]
    [ buildings model
    , hr [] []
    , landedShips model
    , hr [] []
    , orbitingShips model
    , hr [] []
    ]
  ]

planetDetails : Model -> Html Msg
planetDetails model =
  div []
  [ div [ class "row" ]
    [ div [ class "col-lg-12 section-title" ]
      [ text "Planet details"]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-6" ]
      [ text "Name"]
    , div [ class "col-lg-6" ]
      [ text <| case model.planetDetails of
                  Nothing -> ""
                  Just x -> x.name ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-6" ]
      [ text "Position" ]      
    , div [ class "col-lg-6" ]
      [ text <| case model.planetDetails of
                  Nothing -> ""
                  Just x -> toString x.position ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-6" ]
      [ text "Gravity" ]
    , div [ class "col-lg-6" ]
      [ text <| case model.planetDetails of
                  Nothing -> ""
                  Just x -> (toString x.gravity) ++ "g" ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-6" ]
      [ text "Updated"]
    , div [ class "col-lg-6" ]
      [ text <| case model.planetDetails of
                  Nothing -> ""
                  Just x -> x.updated      
                            |> (\x -> (toFloat x) / 10)
                            |> toString ]
    ]
  ]

population : Model -> Html Msg
population model =
  div []
  <| List.append
    [ div [ class "row" ]
      [ div [ class "col-lg-12" ]
        [ span [ class "section-title" ] [ text "Population " ]
        , span [] 
          [ i [ class "fas fa-angle-double-left" ] []
          , text " "
          , i [ class "fas fa-angle-left" ] []
          , text " 1 / 1 "
          , i [ class "fas fa-angle-right" ] []
          , text " "
          , i [ class "fas fa-angle-double-right" ] []
          ]
        ]
      ]
    , div [ class "row" ]
      [ div [ class "col-lg-4" ]
        [ text "Name" ]
      , div [ class "col-lg-4" ]
        [ text "Population" ]
      , div [ class "col-lg-4" ]
        [ text "Updated" ]
      ]
    ]
    <| List.map populationData model.population

populationData : Population -> Html Msg
populationData population =
  div [ class "row" ]
  [ div [ class "col-lg-4" ]
    [ text population.race ]
  , div [ class "col-lg-4" ]
    [ population.population
      |> toString
      |> text ]
  , div [ class "col-lg-4" ]
    [ population.updated      
      |> (\x -> (toFloat x) / 10)
      |> toString
      |> text ]
  ]

buildings : Model -> Html Msg
buildings model = 
  div []
  <| List.append
    [ div [ class "row" ]
      [ div [ class "col-lg-12" ]
        [ span [ class "section-title" ] [ text "Buildings " ]
        , span [] 
          [ i [ class "fas fa-angle-double-left" ] []
          , text " "
          , i [ class "fas fa-angle-left" ] []
          , text " 1 / 1 "
          , i [ class "fas fa-angle-right" ] []
          , text " "
          , i [ class "fas fa-angle-double-right" ] []
          ]
        ]
      ]
    , div [ class "row" ]
      [ div [ class "col-lg-4" ]
        [ text "Type" ]
      , div [ class "col-lg-4" ]
        [ text "Damage" ]
      , div [ class "col-lg-4" ]
        [ text "Updated" ]    
      ]
    ]
    <| List.map buildingData model.buildings

buildingData : Building -> Html Msg
buildingData building =
  div [ class "row" ]
  [ div [ class "col-lg-4" ]
    [ buildingTypeToString building.buildingType
      |> (\x -> x ++ " " ++ (toString building.level))
      |> text ]
    , div [ class "col-lg-4" ]
      [ building.damage * 100
        |> (\x -> (toString x) ++ "%")
        |> text ]
    , div [ class "col-lg-4" ]
      [ building.updated      
        |> (\x -> (toFloat x) / 10)
        |> toString
        |> text ]
  ]  

buildingTypeToString : BuildingType -> String
buildingTypeToString bType =
  case bType of
    SensorStation -> "Sensor station"
    ResearchComplex -> "Research complex"
    Farm -> "Farm"
    ParticleAccelerator -> "Particle accelerator"
    NeutronDetector -> "Neutron Detector"
    BlackMatterScanner -> "Black matter scanner"
    GravityWaveSensor -> "Gravity wave sensor"

landedShips : Model -> Html Msg
landedShips model =
  div []
  [ div [ class "row" ]
    [ div [ class "col-lg-12" ]
      [ span [ class "section-title" ] [ text "Landed ships " ]
      , span []
        [ i [ class "fas fa-angle-double-left" ] []
        , text " "
        , i [ class "fas fa-angle-left" ] []
        , text " - / - "
        , i [ class "fas fa-angle-right" ] []
        , text " "
        , i [ class "fas fa-angle-double-right" ] []
        ]
      ]
    ]
  ]

orbitingShips : Model -> Html Msg
orbitingShips model =
  div []
  [ div [ class "row" ]
    [ div [ class "col-lg-12" ]
      [ span [ class "section-title"] [ text "Orbiting ships " ]
      , span []
        [ i [ class "fas fa-angle-double-left" ] []
        , text " "
        , i [ class "fas fa-angle-left" ] []
        , text " - / - "
        , i [ class "fas fa-angle-right" ] []
        , text " "
        , i [ class "fas fa-angle-double-right" ] []
        ]
      ]
    ]
  ]

constructionQueue : Model -> Html Msg
constructionQueue model =
  div []
  [ div [ class "section-title" ] [ text "Construction queue" ]
  , currentQueue model
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

-- componentize?
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