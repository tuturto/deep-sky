import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Debug exposing (log)
import Json.Decode.Extra exposing ((|:))
import Json.Decode as Decode
-- import Json.Decode exposing ((:=))

main =
  program { init = init
          , view = view
          , update = update 
          , subscriptions = subscriptions }
  

type alias Component = 
  { name : String
  , description : String
  , weight : Int
  }

type alias Model =
  { components : List Component
  }

type EquipmentSlot = InnerSlot
                   | OuterSlot
                   | ArmourSlot
                   | UnknownSlot


-- MODEL

init : (Model, Cmd Msg)
init =
  let newModel = { components = []
                 }
      url = "http://localhost:3000/api/components"
      cmd = Http.send AvailableComponents (Http.get url (Decode.list componentDecoder))
  in
    (newModel, cmd)

stringToSlot : String -> EquipmentSlot
stringToSlot s =
  case s of
    "I" -> InnerSlot
    "O" -> OuterSlot
    "A" -> ArmourSlot
    _ -> UnknownSlot

componentDecoder : Decode.Decoder Component
componentDecoder =
  Decode.succeed Component
    |: (Decode.field "name" Decode.string)
    |: (Decode.field "desc" Decode.string)
    |: (Decode.field "weight" Decode.int)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- UPDATE

type Msg = AvailableComponents (Result Http.Error (List Component))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AvailableComponents (Ok components) ->
      ({ components = components
       }
      , Cmd.none)
    AvailableComponents (Err data) ->
      (model, Cmd.none)

-- VIEW

statisticsPanel : Model -> Html Msg
statisticsPanel model =
  div [ class "design-panel" ]
  [ div [ class "row" ]
    [ div [ class "col-lg-12 design-panel-title" ]
      [ text "Design statistics"
      ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Name"
      ]
    , div [ class "col-lg-8" ]
      [ text "S.S.S. Kickstart"
      ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Type"
      ]
    , div [ class "col-lg-8" ]
      [ text "Destroyer"
      ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Tonnage"
      ]
    , div [ class "col-lg-8" ]
      [ text "150/150"
      ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Shields"
      ]
    , div [ class "col-lg-8" ]
      [ text "0"
      ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Ordnance"
      ]
    , div [ class "col-lg-8" ]
      [ text "15"
      ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Supply"
      ]
    , div [ class "col-lg-8" ]
      [ text "100"
      ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Cost"
      ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-11 col-lg-offset-1" ]
      [ i [ class "fas fa-leaf" ] []
      , text "150 "
      , i [ class "fas fa-cogs" ] []
      , text "150 "
      , i [ class "fas fa-flask" ] []
      , text "150"
      ]
    ]
  ]

selectableComponent : Component -> Html Msg
selectableComponent component =
  div [] 
  [ div [ class "row" ]
    [ div [ class "col-lg-12" ]
      [ text component.name
      ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4 col-lg-offset-1"]
      [ text <| toString component.weight ]
    , div [] []
        -- <| List.map equipmentSlotIndicator component.slots      
    ]
  ]

equipmentSlotIndicator : EquipmentSlot -> Html Msg
equipmentSlotIndicator slot =
  case slot of
    InnerSlot -> text "I"
    OuterSlot -> text "O"
    ArmourSlot -> text "A"
    UnknownSlot -> text ""

componentList : Model -> Html Msg
componentList model =
  div [ class "design-panel" ]
    <| List.append 
      [ div [ class "row" ]
        [ div [ class "col-lg-12 design-panel-title" ]
          [ text "Components"
          ]
        ]     
      ]
      <| List.map selectableComponent model.components

leftPanel : Model -> Html Msg
leftPanel model =
  div []
  [ statisticsPanel model
  , componentList model
  ]

middlePanel : Model -> Html Msg
middlePanel model =
  div []
  [ div [ class "row design-panel" ]
    [ div [ class "row" ]
      [ div [ class "col-lg-12 design-panel-title" ]
        [ text "Selected components"
        ]
      ]
    ]
  ]
  
rightPanel : Model -> Html Msg
rightPanel model =
  div []
  [ div [ class "design-panel" ]
    [ div [ class "row" ]
      [ div [ class "col-lg-12 design-panel-title" ]
        [ text "Warnings"
        ]
      ]
    ]
  ]

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ div [ class "row" ] 
      [ div [ class "col-lg-4" ]
        [ leftPanel model
        ]
      , div [ class "col-lg-4" ]
        [ middlePanel model
        ]
      , div [ class "col-lg-4" ]
        [ rightPanel model
        ]
      ]
    ]
