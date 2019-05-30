module Views.Person exposing (init, page, update)

{-| Page displaying person details. The shown information depends on human
intelligence level. The higher the level, the more details are shown.
-}

import Accessors exposing (get, over, set)
import Accessors.Library exposing (try)
import Api.People exposing (personDetails)
import Data.Accessors
    exposing
        ( ageA
        , diplomacyA
        , errorsA
        , genderA
        , idA
        , intriqueA
        , learningA
        , martialA
        , nameA
        , personA
        , personDetailsStatusA
        , personRA
        , sexA
        , statsA
        , statsStatusA
        , stewardshipA
        )
import Data.Common
    exposing
        ( InfoPanelStatus(..)
        , PersonId
        , error
        , joinMaybe
        )
import Data.Model exposing (Model, Msg(..))
import Data.People
    exposing
        ( Gender(..)
        , Person
        , PersonName
        , Sex(..)
        , displayName
        , unAge
        , unStatValue
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe
import ViewModels.Person exposing (PersonRMsg(..), PersonViewModel)
import Views.Helpers
    exposing
        ( PanelSizing(..)
        , infoPanel
        , twinPanels
        )


{-| Initiate retrieval of data needed by this page
-}
init : PersonId -> Model -> Cmd Msg
init pId model =
    Cmd.batch [ personDetails (PersonMessage << PersonDetailsReceived) pId ]


{-| Render page of displaying the person
-}
page : Model -> Html Msg
page model =
    div [] <| twinPanels EqualPanels leftPanel rightPanel model


{-| Render left side of the screen
-}
leftPanel : Model -> List (Html Msg)
leftPanel model =
    personDetailsPanel model


{-| Render right side of the screen
-}
rightPanel : Model -> List (Html Msg)
rightPanel model =
    statsPanel model


{-| Panel showing basic details of the person
-}
personDetailsPanel : Model -> List (Html Msg)
personDetailsPanel model =
    infoPanel
        { title = "Details"
        , currentStatus = model.personR.personDetailsStatus
        , openingMessage = PersonMessage <| PersonDetailsStatusChanged InfoPanelOpen
        , closingMessage = PersonMessage <| PersonDetailsStatusChanged InfoPanelClosed
        , refreshMessage = Just <| PersonMessage <| PersonDetailsRefreshRequested
        }
        Nothing
        personDetailsContent
        model


{-| Render panel showing details of the person
-}
personDetailsContent : Model -> List (Html Msg)
personDetailsContent model =
    let
        name =
            get (personRA << personA << try << nameA) model
                |> Maybe.map displayName
                |> Maybe.withDefault "-"

        age =
            get (personRA << personA << try << ageA) model
                |> Maybe.map (String.fromInt << unAge)
                |> Maybe.withDefault "-"

        sex =
            get (personRA << personA << try << sexA) model
                |> Maybe.map displaySex
                |> Maybe.withDefault (text "-")

        gender =
            get (personRA << personA << try << genderA) model
                |> Maybe.map displayGender
                |> Maybe.withDefault (text "-")
    in
    [ div [ class "row" ]
        [ div [ class "col-lg-6 panel-table-heading" ] [ text "Name" ]
        , div [ class "col-lg-6" ] [ text name ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-6 panel-table-heading" ] [ text "Age" ]
        , div [ class "col-lg-6" ] [ text age ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-6 panel-table-heading" ] [ text "Sex" ]
        , div [ class "col-lg-6" ] [ sex ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-6 panel-table-heading" ] [ text "Gender" ]
        , div [ class "col-lg-6" ] [ gender ]
        ]
    ]


displaySex : Sex -> Html msg
displaySex s =
    case s of
        Male ->
            text "Male"

        Female ->
            text "Female"

        Intersex ->
            text "Intersex"


displayGender : Gender -> Html msg
displayGender g =
    case g of
        Man ->
            text "Man"

        Woman ->
            text "Woman"

        Agender ->
            text "Agender"

        Nonbinary ->
            text "Nonbinary"


{-| Panel showing stats of the person
-}
statsPanel : Model -> List (Html Msg)
statsPanel model =
    infoPanel
        { title = "Stats"
        , currentStatus = model.personR.statsStatus
        , openingMessage = PersonMessage <| StatsStatusChanged InfoPanelOpen
        , closingMessage = PersonMessage <| StatsStatusChanged InfoPanelClosed
        , refreshMessage = Just <| PersonMessage <| PersonDetailsRefreshRequested
        }
        Nothing
        statsContent
        model


statsContent : Model -> List (Html Msg)
statsContent model =
    let
        diplomacy =
            get (personRA << personA << try << statsA << try << diplomacyA) model
                |> joinMaybe
                |> Maybe.map (text << String.fromInt << unStatValue)
                |> Maybe.withDefault (text "-")

        intrique =
            get (personRA << personA << try << statsA << try << intriqueA) model
                |> joinMaybe
                |> Maybe.map (text << String.fromInt << unStatValue)
                |> Maybe.withDefault (text "-")

        stewardship =
            get (personRA << personA << try << statsA << try << stewardshipA) model
                |> joinMaybe
                |> Maybe.map (text << String.fromInt << unStatValue)
                |> Maybe.withDefault (text "-")

        learning =
            get (personRA << personA << try << statsA << try << learningA) model
                |> joinMaybe
                |> Maybe.map (text << String.fromInt << unStatValue)
                |> Maybe.withDefault (text "-")

        martial =
            get (personRA << personA << try << statsA << try << martialA) model
                |> joinMaybe
                |> Maybe.map (text << String.fromInt << unStatValue)
                |> Maybe.withDefault (text "-")
    in
    [ div [ class "row" ]
        [ div [ class "col-lg-6 panel-table-heading" ] [ text "Diplomacy" ]
        , div [ class "col-lg-6" ] [ diplomacy ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-6 panel-table-heading" ] [ text "Stewardship" ]
        , div [ class "col-lg-6" ] [ stewardship ]
        ]
    , div
        [ class "row" ]
        [ div [ class "col-lg-6 panel-table-heading" ] [ text "Martial" ]
        , div [ class "col-lg-6" ] [ martial ]
        ]
    , div
        [ class "row" ]
        [ div [ class "col-lg-6 panel-table-heading" ] [ text "Intrique" ]
        , div [ class "col-lg-6" ] [ intrique ]
        ]
    , div
        [ class "row" ]
        [ div [ class "col-lg-6 panel-table-heading" ] [ text "Learning" ]
        , div [ class "col-lg-6" ] [ learning ]
        ]
    ]


{-| Handle messages specific to this page
-}
update : PersonRMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PersonDetailsReceived (Ok person) ->
            ( set (personRA << personA) (Just person) model
            , Cmd.none
            )

        PersonDetailsReceived (Err err) ->
            ( set (personRA << personA) Nothing model
                |> over errorsA (\errors -> error err "Failed to load person details" :: errors)
            , Cmd.none
            )

        PersonDetailsStatusChanged status ->
            ( set (personRA << personDetailsStatusA) status model
            , Cmd.none
            )

        PersonDetailsRefreshRequested ->
            case get (personRA << personA << try << idA) model of
                Nothing ->
                    ( model, Cmd.none )

                Just pId ->
                    ( set (personRA << personA) Nothing model
                    , personDetails (PersonMessage << PersonDetailsReceived) pId
                    )

        StatsStatusChanged status ->
            ( set (personRA << statsStatusA) status model
            , Cmd.none
            )
