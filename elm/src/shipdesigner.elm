import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode.Extra exposing ((|:))
import Json.Decode as Decode
import Focus exposing (..)

main : Program Never Model Msg
main =
  program { init = init
          , view = view
          , update = update 
          , subscriptions = subscriptions }
  

type alias Component = 
  { id : Int
  , name : String
  , description : String
  , weight : Int
  , slots : List EquipmentSlot
  , types : List EquipmentType
  }

type InstalledComponent = InstalledComponent Component Int

type alias Ship =
  { components : List InstalledComponent }

type alias Chassis =
  { name : String
  , maxTonnage : Int 
  , requiredTypes : List (Int, EquipmentType) }

type alias Model =
  { components : List Component
  , ship : Ship
  , chassis : Chassis
  }

type EquipmentSlot = InnerSlot
                   | OuterSlot
                   | ArmourSlot
                   | UnknownSlot

type EquipmentType = BridgeEquipment
                   | SensorEquipment                   
                   | UnknowEquipmentType

type alias ShipValidator = Model -> Maybe String

-- MODEL

init : (Model, Cmd Msg)
init =
  let newModel = { components = []
                 , ship = Ship []
                 , chassis = { name = "Destroyer"
                             , maxTonnage = 150
                             , requiredTypes = [ (1, BridgeEquipment) ] }
                 }
      url = "/api/components"
      cmd = Http.send AvailableComponents (Http.get url (Decode.list componentDecoder))
  in
    (newModel, cmd)

stringToSlot : String -> Decode.Decoder EquipmentSlot
stringToSlot s =
  case s of
    "InnerSlot" -> Decode.succeed InnerSlot
    "OuterSlot" -> Decode.succeed OuterSlot
    "ArmourSlot" -> Decode.succeed ArmourSlot
    _ -> Decode.succeed UnknownSlot

slotDecoder : Decode.Decoder EquipmentSlot
slotDecoder =
  Decode.string |> Decode.andThen stringToSlot

stringToEqType : String -> Decode.Decoder EquipmentType
stringToEqType s =
  case s of
    "BridgeEquipment" -> Decode.succeed BridgeEquipment
    "SensorEquipment" -> Decode.succeed SensorEquipment
    _ -> Decode.succeed UnknowEquipmentType

equipmentTypeDecoder : Decode.Decoder EquipmentType
equipmentTypeDecoder =
  Decode.string |> Decode.andThen stringToEqType

componentDecoder : Decode.Decoder Component
componentDecoder =
  Decode.succeed Component
    |: (Decode.field "id" Decode.int)
    |: (Decode.field "name" Decode.string)
    |: (Decode.field "description" Decode.string)
    |: (Decode.field "weight" Decode.int)
    |: (Decode.field "slots" <| Decode.list slotDecoder)
    |: (Decode.field "types" <| Decode.list equipmentTypeDecoder)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- UPDATE

type Msg = AvailableComponents (Result Http.Error (List Component))
         | AddComponent Component
         | RemoveComponent Component

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AvailableComponents (Ok components) ->
      ( model & modelComponents .= components
      , Cmd.none)
    AvailableComponents (Err data) ->
      (model, Cmd.none)
    AddComponent component ->
      ( addComponent model component
      , Cmd.none)
    RemoveComponent component ->
      ( removeComponent model component
      , Cmd.none)

addComponent : Model -> Component -> Model
addComponent model component =
  let
    matches = List.filter search model.ship.components
    search (InstalledComponent item _) = item.id == component.id
    newCount = case List.head matches of
                Just (InstalledComponent _ count) -> count + 1
                Nothing -> 1
  in
    if newCount > 1 then
      let 
        compRemoved = model & modelShipF => shipComponentsF $= List.filter (\x -> not <| search x)
      in
        compRemoved & modelShipF => shipComponentsF $= List.append [InstalledComponent component newCount]
    else
      model & modelShipF => shipComponentsF $= List.append [InstalledComponent component 1]

removeComponent : Model -> Component -> Model
removeComponent model component =
  let
    matches = List.filter search model.ship.components
    search (InstalledComponent item _) = item.id == component.id
    newCount = case List.head matches of
                Just (InstalledComponent _ count) -> count - 1
                Nothing -> 0
  in
    if newCount == 0 then
      model & modelShipF => shipComponentsF $= List.filter (\x -> not <| search x)
    else
      let 
        compRemoved = model & modelShipF => shipComponentsF $= List.filter (\x -> not <| search x)
      in
        compRemoved & modelShipF => shipComponentsF $= List.append [InstalledComponent component newCount]

modelComponents : Setter Model Model (List Component) (List Component)
modelComponents f model = { model | components = f model.components }

modelShipF : Setter Model Model Ship Ship
modelShipF f model = { model | ship = f model.ship }

shipComponentsF : Setter Ship Ship (List InstalledComponent) (List InstalledComponent)
shipComponentsF f ship = { ship | components = f ship.components }

totalTonnage : Ship -> Int
totalTonnage ship =
  List.foldr (\(InstalledComponent component amount) acc -> component.weight * amount + acc) 0 ship.components

tonnageCheck : ShipValidator
tonnageCheck model =
  if totalTonnage model.ship > model.chassis.maxTonnage
  then Just "Ship design exceeds max tonnage"
  else Nothing

equipmentRequirementToString : (Int, EquipmentType) -> String
equipmentRequirementToString (n, equipment) =
  case equipment of
    BridgeEquipment -> 
      "at least n bridges are required"
    SensorEquipment ->
      "at least n sensors are required"
    UnknowEquipmentType ->
      "unknown type"

componentCheck : ShipValidator
componentCheck model =
  let 
    --bridgeFound = List.any (\(InstalledComponent comp _) -> comp.bridge) model.ship.components
    bridgeFound = True
  in
    --if model.chassis.requiresBridge && not bridgeFound
    --then Just "Bridge is required"
    --else Nothing
    Nothing

validators : List ShipValidator
validators = [ tonnageCheck 
             , componentCheck ]

validateDesign : Model -> List String
validateDesign model =
  List.filterMap identity <| List.map (\x -> x model) validators

-- VIEW

statisticsPanel : Model -> Html Msg
statisticsPanel model =
  div [ class "design-panel" ]
  [ div [ class "row" ]
    [ div [ class "col-lg-12 design-panel-title" ]
      [ text "Design statistics" ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Name" ]
    , div [ class "col-lg-8" ]
      [ text "S.S.S. Kickstart" ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Type" ]
    , div [ class "col-lg-8" ]
      [ text model.chassis.name ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Tonnage" ]
    , div [ class "col-lg-8" ]
      [ text <| toString <| List.foldr (\(InstalledComponent component amount) acc -> component.weight * amount + acc) 0 model.ship.components 
      , text " / "
      , text <| toString model.chassis.maxTonnage ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Shields" ]
    , div [ class "col-lg-8" ]
      [ text "0" ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Ordnance" ]
    , div [ class "col-lg-8" ]
      [ text "15" ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Supply" ]
    , div [ class "col-lg-8" ]
      [ text "100" ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-4" ]
      [ text "Cost" ]
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
  div [ onClick <| AddComponent component ] 
  [ div [ class "row" ]
    [ div [ class "col-lg-12" ]
      [ text component.name ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-1 col-lg-offset-1" ]
      [ text <| toString component.weight ]
    , div [ class "col-lg-1" ]
        <| List.map equipmentSlotIndicator component.slots
    ]
  ]

selectedComponent : InstalledComponent -> Html Msg
selectedComponent (InstalledComponent component amount) =
  div [] 
  [ div [ class "row" ]
    [ div [ class "col-lg-12" ]
      [ text component.name
      , div [ class "btn btn-outline-dark btn-sm"
               , onClick <| RemoveComponent component ] 
        [ text " - "]
      , text <| toString amount
      , div [ class "btn btn-outline-dark btn-sm"
            , onClick <| AddComponent component ] 
        [ text " + "]
      ]
    ]
  , div [class "row" ]
    [ div [ class "col-lg-11 col-lg-offset-1" ]
      [ text component.description ]
    ]
  , div [ class "row" ]
    [ div [ class "col-lg-1 col-lg-offset-1" ]
      [ text <| toString component.weight ]
    , div [ class "col-lg-1" ]
        <| List.map equipmentSlotIndicator component.slots
    ]
  ]

equipmentSlotIndicator : EquipmentSlot -> Html Msg
equipmentSlotIndicator slot =
  case slot of
    InnerSlot -> text "I"
    OuterSlot -> text "O"
    ArmourSlot -> text "A"
    UnknownSlot -> text "?"

componentList : Model -> Html Msg
componentList model =
  div [ class "design-panel" ]
    <| List.append 
      [ div [ class "row" ]
        [ div [ class "col-lg-12 design-panel-title" ]
          [ text "Components" ]
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
    <| List.append
    [ div [ class "row" ]
      [ div [ class "col-lg-12 design-panel-title" ]
        [ text "Selected components" ]
      ]        
    ]
    <| List.map selectedComponent model.ship.components
  ]

warningMessages : List String -> List (Html Msg)
warningMessages s =
  let 
    mapper err = div [ class "row" ]
                 [ div [ class "col-lg-12" ]
                   [ text err]
                 ]
  in
    List.map mapper s


rightPanel : Model -> Html Msg
rightPanel model =
  div []
  <| List.append
    [ div [ class "design-panel" ]
      [ div [ class "row" ]
        [ div [ class "col-lg-12 design-panel-title" ]
          [ text "Warnings" ]
        ]
      ]      
    ]
    <| warningMessages <| validateDesign model   

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ div [ class "row" ] 
      [ div [ class "col-lg-3" ]
        [ leftPanel model ]
      , div [ class "col-lg-5" ]
        [ middlePanel model ]
      , div [ class "col-lg-4" ]
        [ rightPanel model ]
      ]
    ]
