module Views.Admin.People.Edit exposing (init, page, update)

import Accessors exposing (get, over, set)
import Api.Admin
    exposing
        ( getPerson
        , getSimulationStatus
        , putPerson
        )
import Data.Accessors
    exposing
        ( adminEditPersonRA
        , adminRA
        , cognomenA
        , dateOfBirthA
        , diplomacyA
        , errorsA
        , familyNameA
        , fieldsA
        , firstNameA
        , genderA
        , intriqueA
        , learningA
        , martialA
        , nameTypeA
        , personA
        , regnalNumberA
        , sexA
        , stewardshipA
        )
import Data.Admin exposing (Person)
import Data.Common
    exposing
        ( DynastyId(..)
        , FactionId(..)
        , PersonId(..)
        , StarDate(..)
        , StarSystemId(..)
        , apMaybe
        , error
        )
import Data.Model exposing (Model, Msg(..))
import Data.People
    exposing
        ( Gender(..)
        , Sex(..)
        , StatValue(..)
        , age
        , unAge
        , unStatValue
        )
import Data.PersonNames
    exposing
        ( Cognomen(..)
        , FamilyName(..)
        , FirstName(..)
        , PersonName(..)
        , RegnalNumber(..)
        , getCognomen
        , getFamilyName
        , getFirstName
        , getRegnalNumber
        , unCognomen
        , unFamilyName
        , unFirstName
        , unRegnalNumber
        )
import Html exposing (Html, div, input, option, select, text)
import Html.Attributes exposing (class, disabled, maxlength, step, type_, value)
import Html.Events exposing (onClick, onInput)
import RemoteData
import ViewModels.Admin.Main exposing (AdminRMsg(..))
import ViewModels.Admin.People.Edit
    exposing
        ( AdminEditPersonRMsg(..)
        , Fields
        , emptyFields
        )
import Views.Admin.Menu exposing (adminLayout, personMenu)
import Views.Helpers exposing (starDateToString, stringToStarDate)


{-| Render edit person view
-}
page : Model -> Html Msg
page model =
    let
        fields =
            model.adminR.adminEditPersonR.fields

        familyNameDisabled =
            case stringToNameType fields.nameType of
                Just Simple ->
                    True

                Just Regular ->
                    False

                Just Regal ->
                    False

                Nothing ->
                    True

        regnalNumberDisabled =
            case stringToNameType fields.nameType of
                Just Simple ->
                    True

                Just Regular ->
                    True

                Just Regal ->
                    False

                Nothing ->
                    True
    in
    adminLayout personMenu
        [ div [ class "row" ]
            [ div [ class "col-lg-2" ]
                [ text "Name" ]
            , div [ class "col-lg-4" ]
                [ select
                    [ value fields.nameType
                    , onInput (AdminEditPersonMessage << NameTypeChanged)
                    ]
                    [ option [ value <| nameTypeToString Simple ] [ text <| nameTypeToString Simple ]
                    , option [ value <| nameTypeToString Regular ] [ text <| nameTypeToString Regular ]
                    , option [ value <| nameTypeToString Regal ] [ text <| nameTypeToString Regal ]
                    ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-lg-2" ]
                [ text "First name" ]
            , div [ class "col-lg-4" ]
                [ input
                    [ type_ "text"
                    , value fields.firstName
                    , onInput (AdminEditPersonMessage << FirstNameChanged)
                    ]
                    []
                ]
            , div [ class "col-lg-2" ]
                [ text "Cognomen" ]
            , div [ class "col-lg-4" ]
                [ input
                    [ type_ "text"
                    , value fields.cognomen
                    , onInput (AdminEditPersonMessage << CognomenChanged)
                    ]
                    []
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-lg-2" ]
                [ text "Family name" ]
            , div [ class "col-lg-4" ]
                [ input
                    [ type_ "text"
                    , value fields.familyName
                    , onInput (AdminEditPersonMessage << FamilyNameChanged)
                    , disabled familyNameDisabled
                    ]
                    []
                ]
            , div [ class "col-lg-2" ]
                [ text "Regnal number" ]
            , div [ class "col-lg-4" ]
                [ input
                    [ Html.Attributes.min "0"
                    , type_ "number"
                    , value fields.regnalNumber
                    , onInput (AdminEditPersonMessage << RegnalNumberChanged)
                    , disabled regnalNumberDisabled
                    ]
                    []
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-lg-2" ]
                [ text "Sex" ]
            , div [ class "col-lg-4" ]
                [ select
                    [ value fields.sex
                    , onInput (AdminEditPersonMessage << SexChanged)
                    ]
                    [ option [ value <| sexToString Female ] [ text <| sexToString Female ]
                    , option [ value <| sexToString Intersex ] [ text <| sexToString Intersex ]
                    , option [ value <| sexToString Male ] [ text <| sexToString Male ]
                    ]
                ]
            , div [ class "col-lg-2" ]
                [ text "Gender" ]
            , div [ class "col-lg-4" ]
                [ select
                    [ value fields.gender
                    , onInput (AdminEditPersonMessage << GenderChanged)
                    ]
                    [ option [ value <| genderToString Agender ] [ text <| genderToString Agender ]
                    , option [ value <| genderToString Man ] [ text <| genderToString Man ]
                    , option [ value <| genderToString Nonbinary ] [ text <| genderToString Nonbinary ]
                    , option [ value <| genderToString Woman ] [ text <| genderToString Woman ]
                    ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-lg-2" ]
                [ text "Date of birth" ]
            , div [ class "col-lg-4" ]
                [ input
                    [ maxlength 5
                    , Html.Attributes.min "0"
                    , type_ "number"
                    , step "0.1"
                    , value fields.dateOfBirth
                    , onInput (AdminEditPersonMessage << DateOfBirthChanged)
                    ]
                    []
                ]
            , div [ class "col-lg-2" ]
                [ text "Age" ]
            , div [ class "col-lg-4" ]
                [ Maybe.map2 age
                    (RemoteData.map .time model.adminR.simulation
                        |> RemoteData.toMaybe
                    )
                    (stringToStarDate fields.dateOfBirth)
                    |> Maybe.map unAge
                    |> Maybe.map String.fromInt
                    |> Maybe.withDefault "-"
                    |> text
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-lg-2" ]
                [ text "Diplomacy" ]
            , div [ class "col-lg-4" ]
                [ statField fields.diplomacy DiplomacyChanged ]
            , div [ class "col-lg-2" ]
                [ text "Martial" ]
            , div [ class "col-lg-4" ]
                [ statField fields.martial MartialChanged ]
            ]
        , div [ class "row" ]
            [ div [ class "col-lg-2" ]
                [ text "Stewardship" ]
            , div [ class "col-lg-4" ]
                [ statField fields.stewardship StewardshipChanged ]
            , div [ class "col-lg-2" ]
                [ text "Intrique" ]
            , div [ class "col-lg-4" ]
                [ statField fields.intrique IntriqueChanged ]
            ]
        , div [ class "row" ]
            [ div [ class "col-lg-2" ]
                [ text "Learning" ]
            , div [ class "col-lg-4" ]
                [ statField fields.learning LearningChanged ]
            ]
        , div [ class "row" ]
            [ div [ class "col-lg-10" ]
                [ text " " ]
            , div [ class "col-lg-2" ]
                [ div [ class "btn btn-primary btn-sm command-button", onClick (AdminEditPersonMessage SaveRequested) ]
                    [ text "Update" ]
                , div [ class "btn btn-default btn-sm command-button", onClick (AdminEditPersonMessage UndoRequested) ]
                    [ text "Undo" ]
                ]
            ]
        ]
        model


{-| Different name types
-}
type NameType
    = Simple
    | Regular
    | Regal


{-| Map nametype to string for use with dropdown lists
-}
nameTypeToString : NameType -> String
nameTypeToString n =
    case n of
        Simple ->
            "Simple"

        Regular ->
            "Regular"

        Regal ->
            "Regal"


{-| Map string to nametype for use with dropdown lists
-}
stringToNameType : String -> Maybe NameType
stringToNameType s =
    case s of
        "Simple" ->
            Just Simple

        "Regular" ->
            Just Regular

        "Regal" ->
            Just Regal

        _ ->
            Nothing


{-| Map sex to string for using with dropdown menus
-}
sexToString : Sex -> String
sexToString sex =
    case sex of
        Male ->
            "Male"

        Female ->
            "Female"

        Intersex ->
            "Intersex"


{-| Map string to maybe sex, useful for working with dropdown menus
-}
stringToSex : String -> Maybe Sex
stringToSex s =
    case s of
        "Male" ->
            Just Male

        "Female" ->
            Just Female

        "Intersex" ->
            Just Intersex

        _ ->
            Nothing


{-| Map gender to string for using with dropdown menus
-}
genderToString : Gender -> String
genderToString g =
    case g of
        Man ->
            "Man"

        Woman ->
            "Woman"

        Agender ->
            "Agender"

        Nonbinary ->
            "Nonbinary"


{-| Map string to maybe gender, useful for working with dropdown menus
-}
stringToGender : String -> Maybe Gender
stringToGender s =
    case s of
        "Man" ->
            Just Man

        "Woman" ->
            Just Woman

        "Agender" ->
            Just Agender

        "Nonbinary" ->
            Just Nonbinary

        _ ->
            Nothing


{-| Number field for displaying and editing stat value
-}
statField : String -> (String -> AdminEditPersonRMsg) -> Html Msg
statField stat changeMsg =
    input
        [ maxlength 2
        , Html.Attributes.min "0"
        , type_ "number"
        , value stat
        , onInput (AdminEditPersonMessage << changeMsg)
        ]
        []


{-| map Person to Fields that store current values edited on UI
-}
personToFields : Maybe Person -> Fields
personToFields person =
    case person of
        Nothing ->
            emptyFields

        Just p ->
            { firstName = unFirstName <| getFirstName p.name
            , familyName =
                getFamilyName p.name
                    |> Maybe.map unFamilyName
                    |> Maybe.withDefault ""
            , cognomen =
                getCognomen p.name
                    |> Maybe.map unCognomen
                    |> Maybe.withDefault ""
            , regnalNumber =
                getRegnalNumber p.name
                    |> Maybe.map unRegnalNumber
                    |> Maybe.map String.fromInt
                    |> Maybe.withDefault ""
            , sex = sexToString p.sex
            , gender = genderToString p.gender
            , dateOfBirth = starDateToString p.dateOfBirth
            , diplomacy = String.fromInt (unStatValue p.diplomacy)
            , martial = String.fromInt (unStatValue p.martial)
            , stewardship = String.fromInt (unStatValue p.stewardship)
            , intrique = String.fromInt (unStatValue p.intrique)
            , learning = String.fromInt (unStatValue p.learning)
            , nameType = personNameToNameTypeString p.name
            }


{-| Combine UI changes to original person
In case there are any invalid UI elements, result will be Nothing
-}
fieldsToPerson : Person -> Fields -> Maybe Person
fieldsToPerson person fields =
    Just Person
        |> apMaybe (Just person.id)
        |> apMaybe (fieldsToName fields)
        |> apMaybe (stringToSex fields.sex)
        |> apMaybe (stringToGender fields.gender)
        |> apMaybe (stringToStarDate fields.dateOfBirth)
        |> apMaybe (Maybe.map StatValue (String.toInt fields.diplomacy))
        |> apMaybe (Maybe.map StatValue (String.toInt fields.learning))
        |> apMaybe (Maybe.map StatValue (String.toInt fields.martial))
        |> apMaybe (Maybe.map StatValue (String.toInt fields.intrique))
        |> apMaybe (Maybe.map StatValue (String.toInt fields.stewardship))
        |> apMaybe (Just person.factionId)
        |> apMaybe (Just person.planetTitle)
        |> apMaybe (Just person.starSystemTitle)
        |> apMaybe (Just person.dynastyId)


{-| Map UI fields to person name
-}
fieldsToName : Fields -> Maybe PersonName
fieldsToName fields =
    let
        firstName =
            FirstName <| String.trim fields.firstName

        familyName =
            FamilyName <| String.trim fields.familyName

        cognomen =
            if String.isEmpty <| String.trim fields.cognomen then
                Nothing

            else
                Just <| Cognomen <| String.trim fields.cognomen

        regnalNumber =
            String.trim fields.regnalNumber
                |> String.toInt
                |> Maybe.map RegnalNumber
    in
    case fields.nameType of
        "Simple" ->
            Just <| SimpleName firstName cognomen

        "Regular" ->
            Just <| RegularName firstName familyName cognomen

        "Regal" ->
            Just RegalName
                |> apMaybe (Just firstName)
                |> apMaybe (Just familyName)
                |> apMaybe regnalNumber
                |> apMaybe (Just cognomen)

        _ ->
            Nothing


{-| Map person name to name type string, for use with dropdown lists
-}
personNameToNameTypeString : PersonName -> String
personNameToNameTypeString name =
    case name of
        SimpleName _ _ ->
            nameTypeToString Simple

        RegularName _ _ _ ->
            nameTypeToString Regular

        RegalName _ _ _ _ ->
            nameTypeToString Regal


{-| Initialize data retrieval from server
-}
init : PersonId -> Model -> Cmd Msg
init pId _ =
    Cmd.batch
        [ getSimulationStatus (AdminMessage << SimulationStatusReceived)
        , getPerson (AdminEditPersonMessage << PersonReceived) pId
        ]


{-| Handle incoming messages
-}
update : AdminEditPersonRMsg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PersonReceived (Ok res) ->
            ( set (adminRA << adminEditPersonRA << personA) (Just res) model
                |> set (adminRA << adminEditPersonRA << fieldsA) (personToFields (Just res))
            , Cmd.none
            )

        PersonReceived (Err err) ->
            ( over errorsA (\errors -> error err "Failed to load person" :: errors) model
                |> set (adminRA << adminEditPersonRA << personA) Nothing
                |> set (adminRA << adminEditPersonRA << fieldsA) (personToFields Nothing)
            , Cmd.none
            )

        DiplomacyChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << diplomacyA) value model
            , Cmd.none
            )

        MartialChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << martialA) value model
            , Cmd.none
            )

        StewardshipChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << stewardshipA) value model
            , Cmd.none
            )

        IntriqueChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << intriqueA) value model
            , Cmd.none
            )

        LearningChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << learningA) value model
            , Cmd.none
            )

        SexChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << sexA) value model
            , Cmd.none
            )

        GenderChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << genderA) value model
            , Cmd.none
            )

        NameTypeChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << nameTypeA) value model
            , Cmd.none
            )

        FirstNameChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << firstNameA) value model
            , Cmd.none
            )

        FamilyNameChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << familyNameA) value model
            , Cmd.none
            )

        CognomenChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << cognomenA) value model
            , Cmd.none
            )

        RegnalNumberChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << regnalNumberA) value model
            , Cmd.none
            )

        DateOfBirthChanged value ->
            ( set (adminRA << adminEditPersonRA << fieldsA << dateOfBirthA) value model
            , Cmd.none
            )

        UndoRequested ->
            ( set (adminRA << adminEditPersonRA << fieldsA)
                (personToFields <| get (adminRA << adminEditPersonRA << personA) model)
                model
            , Cmd.none
            )

        SaveRequested ->
            case model.adminR.adminEditPersonR.person of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just person ->
                    let
                        updatedPerson =
                            fieldsToPerson person model.adminR.adminEditPersonR.fields
                    in
                    case updatedPerson of
                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                        Just p ->
                            ( model
                            , putPerson (AdminEditPersonMessage << PersonReceived) p.id p
                            )
