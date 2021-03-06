module Views.Admin.People.Add exposing
    ( init
    , isLoading
    , page
    , update
    )

import Accessors exposing (over, set)
import Api.Admin exposing (addPerson, getSimulationStatus)
import Data.Accessors
    exposing
        ( adminAddPersonRA
        , adminRA
        , ageFieldsA
        , ageOptionA
        , endAgeA
        , errorsA
        , exactAgeA
        , fieldsA
        , personA
        , startAgeA
        )
import Data.Admin exposing (AgeOptions(..), PersonOptions)
import Data.Common
    exposing
        ( Route(..)
        , apMaybe
        , error
        )
import Data.Model exposing (Model, Msg(..))
import Data.People exposing (Age(..))
import Html exposing (Html, div, input, option, select, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import RemoteData exposing (RemoteData(..))
import SaveData exposing (SaveData(..))
import ViewModels.Admin.Main exposing (AdminRMsg(..))
import ViewModels.Admin.People.Add
    exposing
        ( AdminAddPersonRMsg(..)
        , Fields
        )
import Views.Admin.Main
import Views.Admin.Menu exposing (adminLayout, personMenu)
import Views.Helpers exposing (pushUrl)


{-| Render add person view
-}
page : Model -> Html Msg
page model =
    let
        fields =
            model.adminR.adminAddPersonR.fields
    in
    adminLayout personMenu
        (div [ class "row" ]
            [ div [ class "col-lg-2" ] [ text "Age type" ]
            , div [ class "col-lg-4" ]
                [ select
                    [ value fields.ageOption
                    , onInput (AdminAddPersonMessage << AgeOptionChanged)
                    ]
                    [ option [ value <| ageOptionTypeToString NoChoice ] [ text "" ]
                    , option [ value <| ageOptionTypeToString Exact ] [ text "Exact age" ]
                    , option [ value <| ageOptionTypeToString Bracket ] [ text "Age bracket" ]
                    ]
                ]
            ]
            :: (ageSelection fields <| stringToAgeOptionType fields.ageOption)
            ++ buttonStrip model
        )
        model


buttonStrip : Model -> List (Html Msg)
buttonStrip model =
    let
        bClass =
            if isLoading model then
                [ class "btn btn-disabled btn-sm command-button" ]

            else
                [ class "btn btn-primary btn-sm command-button", onClick (AdminAddPersonMessage CreationRequested) ]
    in
    [ div [ class "row" ]
        [ div [ class "col-lg-10" ]
            [ text " " ]
        , div [ class "col-lg-2" ]
            [ div bClass [ text "Create" ]
            ]
        ]
    ]


ageSelection : Fields -> AgeOptionType -> List (Html Msg)
ageSelection fields opt =
    case opt of
        NoChoice ->
            []

        Exact ->
            [ div [ class "row" ]
                [ div [ class "col-lg-2" ] [ text "Age" ]
                , div [ class "col-lg-4 " ]
                    [ input
                        [ type_ "number"
                        , value fields.ageFields.exactAge
                        , onInput (AdminAddPersonMessage << ExactAgeChanged)
                        ]
                        []
                    ]
                ]
            ]

        Bracket ->
            [ div [ class "row" ]
                [ div [ class "col-lg-2" ] [ text "Age bracket" ]
                , div [ class "col-lg-4 " ]
                    [ input
                        [ type_ "number"
                        , value fields.ageFields.startAge
                        , onInput (AdminAddPersonMessage << StartAgeChanged)
                        ]
                        []
                    ]
                , div [ class "col-lg-4 " ]
                    [ input
                        [ type_ "number"
                        , value fields.ageFields.endAge
                        , onInput (AdminAddPersonMessage << EndAgeChanged)
                        ]
                        []
                    ]
                ]
            ]


type AgeOptionType
    = Exact
    | Bracket
    | NoChoice


ageOptionTypeToString : AgeOptionType -> String
ageOptionTypeToString opt =
    case opt of
        Exact ->
            "Exact"

        Bracket ->
            "Bracket"

        NoChoice ->
            ""


stringToAgeOptionType : String -> AgeOptionType
stringToAgeOptionType s =
    case s of
        "Exact" ->
            Exact

        "Bracket" ->
            Bracket

        _ ->
            NoChoice


{-| Initialize data retrieval from server
-}
init : Model -> Cmd Msg
init _ =
    Cmd.batch
        [ getSimulationStatus (AdminMessage << SimulationStatusReceived)
        ]


isLoading : Model -> Bool
isLoading model =
    RemoteData.isLoading model.adminR.adminAddPersonR.person
        || Views.Admin.Main.isLoading model


{-| Handle incoming messages
-}
update : AdminAddPersonRMsg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        AgeOptionChanged s ->
            ( set (adminRA << adminAddPersonRA << fieldsA << ageOptionA) s model
            , Cmd.none
            )

        ExactAgeChanged s ->
            ( set (adminRA << adminAddPersonRA << fieldsA << ageFieldsA << exactAgeA) s model
            , Cmd.none
            )

        StartAgeChanged s ->
            ( set (adminRA << adminAddPersonRA << fieldsA << ageFieldsA << startAgeA) s model
            , Cmd.none
            )

        EndAgeChanged s ->
            ( set (adminRA << adminAddPersonRA << fieldsA << ageFieldsA << endAgeA) s model
            , Cmd.none
            )

        CreationRequested ->
            let
                opt =
                    fieldsToPersonOptions model.adminR.adminAddPersonR.fields
            in
            case opt of
                Nothing ->
                    ( model, Cmd.none )

                Just msg ->
                    ( set (adminRA << adminAddPersonRA << personA) Loading model
                    , addPerson (AdminAddPersonMessage << PersonCreated) msg
                    )

        PersonCreated (Failure err) ->
            ( over errorsA (\errors -> error err "Failed to generate person" :: errors) model
                |> set (adminRA << adminAddPersonRA << personA) (Failure err)
            , Cmd.none
            )

        PersonCreated (Success res) ->
            ( set (adminRA << adminAddPersonRA << personA) (Success res) model
            , pushUrl model (AdminPersonR res.id)
            )

        PersonCreated NotAsked ->
            ( model
            , Cmd.none
            )

        PersonCreated Loading ->
            ( model
            , Cmd.none
            )


fieldsToPersonOptions : Fields -> Maybe PersonOptions
fieldsToPersonOptions fields =
    Just PersonOptions
        |> apMaybe (fieldsToAgeOptions fields)


fieldsToAgeOptions : Fields -> Maybe (Maybe AgeOptions)
fieldsToAgeOptions fields =
    case stringToAgeOptionType fields.ageOption of
        Exact ->
            apMaybe
                (Just ExactAge
                    |> apMaybe
                        (Just Age
                            |> apMaybe (String.toInt fields.ageFields.exactAge)
                        )
                )
                (Just Just)

        Bracket ->
            apMaybe
                (Just AgeBracket
                    |> apMaybe
                        (Just Age
                            |> apMaybe (String.toInt fields.ageFields.startAge)
                        )
                    |> apMaybe
                        (Just Age
                            |> apMaybe (String.toInt fields.ageFields.endAge)
                        )
                )
                (Just Just)

        NoChoice ->
            Just Nothing
