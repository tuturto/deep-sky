import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
  beginnerProgram { model = model, view = view, update = update }
  

-- MODEL

model = 0


-- UPDATE

type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1
      

-- VIEW

statisticsPanel model =
  div [ class "design-panel" ]
  [
    div [ class "row" ]
    [
      div [ class "col-lg-12 design-panel-title" ]
      [
        text "Design statistics"
      ]
    ]
  , div [ class "row" ]
    [
      div [ class "col-lg-4" ]
      [
        text "Name"
      ]
    , div [ class "col-lg-8" ]
      [
        text "S.S.S. Kickstart"
      ]
    ]
  , div [ class "row" ]
    [
      div [ class "col-lg-4" ]
      [
        text "Type"
      ]
    , div [ class "col-lg-8" ]
      [
        text "Destroyer"
      ]
    ]
  , div [ class "row" ]
    [
      div [ class "col-lg-4" ]
      [
        text "Tonnage"
      ]
    , div [ class "col-lg-8" ]
      [
        text "150/150"
      ]
    ]
  , div [ class "row" ]
    [
      div [ class "col-lg-4" ]
      [
        text "Shields"
      ]
    , div [ class "col-lg-8" ]
      [
        text "0"
      ]
    ]
  , div [ class "row" ]
    [
      div [ class "col-lg-4" ]
      [
        text "Ordnance"
      ]
    , div [ class "col-lg-8" ]
      [
        text "15"
      ]
    ]
  , div [ class "row" ]
    [
      div [ class "col-lg-4" ]
      [
        text "Supply"
      ]
    , div [ class "col-lg-8" ]
      [
        text "100"
      ]
    ]
  , div [ class "row" ]
    [
      div [ class "col-lg-4" ]
      [
        text "Cost"
      ]
    ]
  , div [ class "row" ]
    [
      div [ class "col-lg-11 col-lg-offset-1" ]
      [
        i [ class "fas fa-leaf" ] []
      , text "150 "
      , i [ class "fas fa-cogs" ] []
      , text "150 "
      , i [ class "fas fa-flask" ] []
      , text "150"
      ]
    ]
  ]

componentList model =
  div [ class "design-panel" ]
  [
    div [ class "row" ]
    [
      div [ class "col-lg-12 design-panel-title" ]
      [
        text "Components"
      ]
    ]
  ]

leftPanel model =
  div []
  [
    statisticsPanel model
  , componentList model
  ]

middlePanel model =
  div []
  [
    div [ class "row design-panel" ]
    [
      div [ class "row" ]
      [
        div [ class "col-lg-12 design-panel-title" ]
        [
          text "Selected components"
        ]
      ]
    ]
  ]
  

rightPanel model =
  div []
  [
    div [ class "design-panel" ]
    [
      div [ class "row" ]
      [
        div [ class "col-lg-12 design-panel-title" ]
        [
          text "Warnings"
        ]
      ]
    ]
  ]

view model =
  div [ class "container" ]
    [
      div [ class "row" ] 
      [
        div [ class "col-lg-4" ]
        [
          leftPanel model
        ]
      , div [ class "col-lg-4" ]
        [
          middlePanel model
        ]
      , div [ class "col-lg-4" ]
        [
          rightPanel model
        ]
      ]
    ]
