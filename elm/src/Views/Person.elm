module Views.Person exposing (init, page, update)

{-| Page displaying person details. The shown information depends on human
intelligence level. The higher the level, the more details are shown.
-}

import Accessors exposing (get, over, set)
import Accessors.Library exposing (try)
import Api.People exposing (getDemesne, getPersonDetails)
import Data.Accessors
    exposing
        ( ageA
        , avatarA
        , avatarOpinionA
        , demesneA
        , demesneCurrentPageA
        , demesneStatusA
        , diplomacyA
        , dynastyA
        , errorsA
        , genderA
        , idA
        , intelTypesA
        , intriqueA
        , learningA
        , locationA
        , martialA
        , nameA
        , opinionOfAvatarA
        , personA
        , personDetailsStatusA
        , personRA
        , relationsA
        , relationsCurrentPageA
        , relationsStatusA
        , shortTitleA
        , statsA
        , statsStatusA
        , stewardshipA
        , traitsA
        , traitsCurrentPageA
        , traitsStatusA
        )
import Data.Common
    exposing
        ( InfoPanelStatus(..)
        , PersonId
        , Route(..)
        , error
        , joinMaybe
        , listOrdering
        , maxPage
        , unDemesneName
        , unDynastyName
        , unPlanetName
        , unStarSystemName
        )
import Data.Model exposing (Model, Msg(..))
import Data.People
    exposing
        ( DemesneShortInfo(..)
        , Gender(..)
        , OnUnitData
        , OpinionFeeling(..)
        , OpinionReport(..)
        , OpinionScore(..)
        , PersonLocation(..)
        , RelationLink
        , RelationType(..)
        , Trait
        , formalName
        , personIntelToString
        , relationTypeOrdering
        , relationTypeToString
        , traitOrdering
        , unAge
        , unOpinionScore
        , unStatValue
        , unTraitDescription
        , unTraitName
        )
import Data.PersonNames
    exposing
        ( PersonName(..)
        , ShortTitle
        , displayName
        , personNameOrdering
        , unShortTitle
        )
import Data.Vehicles
    exposing
        ( CrewPosition(..)
        , crewPositionToString
        , unUnitName
        )
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, title)
import Maybe
import Ordering exposing (Ordering)
import Set
import ViewModels.Person exposing (PersonRMsg(..))
import Views.Helpers
    exposing
        ( PanelSizing(..)
        , href
        , infoPanel
        , triplePanels
        , twinPanels
        )


{-| Initiate retrieval of data needed by this page
-}
init : PersonId -> Model -> Cmd Msg
init pId _ =
    Cmd.batch
        [ getPersonDetails (PersonMessage << PersonDetailsReceived) pId
        , getDemesne (PersonMessage << DemesneReceived) pId
        ]


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
        ++ demesnePanel model
        ++ relationsPanel model


{-| Render right side of the screen
-}
rightPanel : Model -> List (Html Msg)
rightPanel model =
    statsPanel model
        ++ traitsPanel model


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


personName : PersonName -> Maybe ShortTitle -> String
personName name shortTitle =
    case Maybe.map unShortTitle shortTitle of
        Nothing ->
            displayName name

        Just title ->
            title ++ " " ++ displayName name


{-| Render panel showing details of the person
-}
personDetailsContent : Model -> List (Html Msg)
personDetailsContent model =
    let
        aName =
            get (personRA << personA << try << nameA) model

        aDynasty =
            joinMaybe <| get (personRA << personA << try << dynastyA << try << nameA) model

        dynastyText =
            case get (personRA << personA) model of
                Nothing ->
                    "-"

                Just _ ->
                    case aDynasty of
                        Nothing ->
                            "lowborn"

                        Just s ->
                            unDynastyName s

        aTitle =
            get (personRA << personA << try << shortTitleA) model

        fullName =
            Maybe.map2 personName aName aTitle
                |> Maybe.withDefault "-"

        isPlayerAvatar =
            get (personRA << personA << try << avatarA) model
                |> Maybe.withDefault False

        age =
            get (personRA << personA << try << ageA) model
                |> Maybe.map (String.fromInt << unAge)
                |> Maybe.withDefault "-"

        gender =
            get (personRA << personA << try << genderA) model
                |> Maybe.map displayGender
                |> Maybe.withDefault (text "-")

        intel =
            get (personRA << personA << try << intelTypesA) model
                |> Maybe.withDefault []
                |> List.map personIntelToString
                |> Set.fromList
                |> Set.toList
                |> String.join ", "
                |> text

        avatarOpinion =
            get (personRA << personA << try << avatarOpinionA) model
                |> Maybe.map displayOpinion
                |> Maybe.withDefault (text "-")

        opinionOfAvatar =
            get (personRA << personA << try << opinionOfAvatarA) model
                |> Maybe.map displayOpinion
                |> Maybe.withDefault (text "-")

        location =
            case get (personRA << personA << try << locationA) model of
                Just (OnPlanet pDetails) ->
                    a [ href (PlanetR pDetails.planetId) ]
                        [ text <| unPlanetName pDetails.planetName ]

                Just (OnUnit uDetails) ->
                    displayOnUnitLocation uDetails

                Just UnknownLocation ->
                    text "Unknown"

                Nothing ->
                    text "Unknown"
    in
    [ div [ class "row" ]
        [ div [ class "col-lg-4 panel-table-heading" ] [ text "Name" ]
        , div [ class "col-lg-8" ]
            [ if isPlayerAvatar then
                text <| fullName ++ " (you)"

              else
                text fullName
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-4 panel-table-heading" ] [ text "Dynasty" ]
        , div [ class "col-lg-8" ] [ text dynastyText ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-4 panel-table-heading" ] [ text "Age" ]
        , div [ class "col-lg-8" ] [ text age ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-4 panel-table-heading" ] [ text "Gender" ]
        , div [ class "col-lg-8" ] [ gender ]
        ]
    , div [ class "row" ]
        [ div [ class "col-lg-4 panel-table-heading" ] [ text "Location" ]
        , div [ class "col-lg-8" ] [ location ]
        ]
    , if isPlayerAvatar then
        div [] []

      else
        div [ class "row" ]
            [ div [ class "col-lg-4 panel-table-heading" ] [ text "Opinion" ]
            , div [ class "col-lg-8" ]
                [ avatarOpinion
                , text " / "
                , opinionOfAvatar
                ]
            ]
    , div [ class "row" ]
        [ div [ class "col-lg-4 panel-table-heading" ] [ text "Intel" ]
        , div [ class "col-lg-8" ] [ intel ]
        ]
    ]


{-| Render location on unit into html link
-}
displayOnUnitLocation : OnUnitData -> Html Msg
displayOnUnitLocation pos =
    case pos.position of
        Just position ->
            let
                name =
                    crewPositionToString position
                        ++ " on "
                        ++ unUnitName pos.unitName
                        |> text
            in
            a [ href (UnitR pos.unitId) ] [ name ]

        Nothing ->
            a [ href (UnitR pos.unitId) ] [ text <| unUnitName pos.unitName ]


displayOpinion : OpinionReport -> Html Msg
displayOpinion report =
    case report of
        BaseOpinionReport feeling ->
            displayOpinionFeeling feeling

        OpinionReasonReport feeling _ ->
            displayOpinionFeeling feeling

        DetailedOpinionReport score _ ->
            displayOpinionScore score


displayOpinionFeeling : OpinionFeeling -> Html Msg
displayOpinionFeeling feeling =
    case feeling of
        PositiveFeeling ->
            text "positive"

        NeutralFeeling ->
            text "neutral"

        NegativeFeeling ->
            text "negative"


displayOpinionScore : OpinionScore -> Html Msg
displayOpinionScore score =
    unOpinionScore score
        |> String.fromInt
        |> text


{-| Map Gender into displayable tex
-}
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


{-| Contents of stats panel
-}
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


{-| Panel showing relations
-}
relationsPanel : Model -> List (Html Msg)
relationsPanel model =
    infoPanel
        { title = "Relations"
        , currentStatus = model.personR.relationsStatus
        , openingMessage = PersonMessage <| RelationsStatusChanged InfoPanelOpen
        , closingMessage = PersonMessage <| RelationsStatusChanged InfoPanelClosed
        , refreshMessage = Just <| PersonMessage PersonDetailsRefreshRequested
        }
        (Just
            { pageSize = model.personR.relationsPageSize
            , currentPage = model.personR.relationsCurrentPage
            , maxPage =
                get (personRA << personA << try << relationsA) model
                    |> Maybe.withDefault []
                    |> maxPage model.personR.relationsPageSize
            , pageChangedMessage = PersonMessage << RelationsPageChanged
            }
        )
        relationsContent
        model


{-| Content of relations panel
-}
relationsContent : Model -> List (Html Msg)
relationsContent model =
    div [ class "row panel-table-heading" ]
        [ div [ class "col-lg-6" ] [ text "Name" ]
        , div [ class "col-lg-3" ] [ text "Type" ]
        , div [ class "col-lg-3" ] [ text "Opinion" ]
        ]
        :: (get (personRA << personA << try << relationsA) model
                |> Maybe.withDefault []
                |> List.sortWith relationOrdering
                |> List.map relationEntry
           )


relationOrdering : Ordering RelationLink
relationOrdering a b =
    listOrdering relationTypeOrdering a.types b.types
        |> Ordering.ifStillTiedThen (personNameOrdering a.name b.name)


relationEntry : RelationLink -> Html Msg
relationEntry link =
    div [ class "row" ]
        [ div [ class "col-lg-6" ]
            [ a [ href (PersonR link.id) ] [ text <| personName link.name link.shortTitle ] ]
        , div [ class "col-lg-3" ]
            [ List.map relationTypeToString link.types
                |> String.join ", "
                |> text
            ]
        , div [ class "col-lg-3" ]
            [ displayOpinion link.opinion ]
        ]


{-| Panel showing demesne
-}
demesnePanel : Model -> List (Html Msg)
demesnePanel model =
    infoPanel
        { title = "Demesne"
        , currentStatus = model.personR.demesneStatus
        , openingMessage = PersonMessage <| DemesneStatusChanged InfoPanelOpen
        , closingMessage = PersonMessage <| DemesneStatusChanged InfoPanelClosed
        , refreshMessage = Just <| PersonMessage <| DemesneRefreshRequested
        }
        (Just
            { pageSize = model.personR.demesnePageSize
            , currentPage = model.personR.demesneCurrentPage
            , maxPage =
                model.personR.demesne
                    |> Maybe.withDefault []
                    |> maxPage model.personR.demesnePageSize
            , pageChangedMessage = PersonMessage << DemesnePageChanged
            }
        )
        demesneContent
        model


{-| Content of demesne panel
-}
demesneContent : Model -> List (Html Msg)
demesneContent model =
    div [ class "row panel-table-heading" ]
        [ div [ class "col-lg-8" ] [ text "Name" ]
        , div [ class "col-lg-4" ] [ text "Type" ]
        ]
        :: (model.personR.demesne
                |> Maybe.withDefault []
                |> List.sortWith demesneSorter
                |> List.map demesneEntry
           )


{-| Single demesne entry in demesne panel
-}
demesneEntry : DemesneShortInfo -> Html Msg
demesneEntry entry =
    let
        name =
            formalName entry
                |> unDemesneName

        link =
            case entry of
                PlanetDemesneShort report ->
                    a [ href (PlanetR report.planetId) ] [ text name ]

                StarSystemDemesneShort report ->
                    a [ href (StarSystemR report.starSystemId) ] [ text name ]
    in
    div [ class "row" ]
        [ div [ class "col-lg-8" ]
            [ link ]
        , div [ class "col-lg-4" ]
            [ text <| demesneType entry ]
        ]


{-| Sort demesne report by type and then by name
-}
demesneSorter : DemesneShortInfo -> DemesneShortInfo -> Order
demesneSorter a b =
    case a of
        PlanetDemesneShort pa ->
            case b of
                PlanetDemesneShort pb ->
                    compare (unPlanetName pa.name) (unPlanetName pb.name)

                StarSystemDemesneShort _ ->
                    GT

        StarSystemDemesneShort sa ->
            case b of
                PlanetDemesneShort _ ->
                    LT

                StarSystemDemesneShort sb ->
                    compare (unStarSystemName sa.name) (unStarSystemName sb.name)


demesneType : DemesneShortInfo -> String
demesneType info =
    case info of
        PlanetDemesneShort _ ->
            "Planet"

        StarSystemDemesneShort _ ->
            "Star system"


{-| Panel showing traits
-}
traitsPanel : Model -> List (Html Msg)
traitsPanel model =
    infoPanel
        { title = "Traits"
        , currentStatus = model.personR.traitsStatus
        , openingMessage = PersonMessage <| TraitsStatusChanged InfoPanelOpen
        , closingMessage = PersonMessage <| TraitsStatusChanged InfoPanelClosed
        , refreshMessage = Just <| PersonMessage <| PersonDetailsRefreshRequested
        }
        (Just
            { pageSize = model.personR.traitsPageSize
            , currentPage = model.personR.traitsCurrentPage
            , maxPage =
                get (personRA << personA << try << traitsA) model
                    |> joinMaybe
                    |> Maybe.withDefault []
                    |> maxPage model.personR.traitsPageSize
            , pageChangedMessage = PersonMessage << TraitsPageChanged
            }
        )
        traitsContent
        model


traitsContent : Model -> List (Html Msg)
traitsContent model =
    let
        traits =
            get (personRA << personA << try << traitsA) model
                |> joinMaybe
                |> Maybe.withDefault []
                |> List.sortWith traitOrdering

        blockSize =
            List.length traits // 3

        blockLeftOvers =
            remainderBy 3 (List.length traits)

        leftCount =
            if blockLeftOvers > 0 then
                blockSize + 1

            else
                blockSize

        leftTraits =
            List.take leftCount traits

        middleCount =
            if blockLeftOvers > 1 then
                blockSize + 1

            else
                blockSize

        middleTraits =
            List.drop leftCount traits
                |> List.take middleCount

        rightTraits =
            List.drop (leftCount + middleCount) traits

        leftColumn =
            traitList leftTraits

        middleColumn =
            traitList middleTraits

        rightColumn =
            traitList rightTraits
    in
    triplePanels leftColumn middleColumn rightColumn <| model


traitList : List Trait -> ( Model -> List (Html Msg), String )
traitList traits =
    ( \_ -> List.map traitEntry traits, "col-lg-4" )


traitEntry : Trait -> Html Msg
traitEntry trait =
    div [ class "row" ]
        [ div
            [ class "col-lg-12"
            , title <| unTraitDescription trait.description
            ]
            [ text <| unTraitName trait.name ]
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

        DemesneReceived (Ok demesne) ->
            ( set (personRA << demesneA) (Just demesne) model
            , Cmd.none
            )

        DemesneReceived (Err err) ->
            ( set (personRA << demesneA) Nothing model
                |> over errorsA (\errors -> error err "Failed to load demesne" :: errors)
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
                    , getPersonDetails (PersonMessage << PersonDetailsReceived) pId
                    )

        StatsStatusChanged status ->
            ( set (personRA << statsStatusA) status model
            , Cmd.none
            )

        DemesneStatusChanged status ->
            ( set (personRA << demesneStatusA) status model
            , Cmd.none
            )

        DemesneRefreshRequested ->
            case get (personRA << personA << try << idA) model of
                Nothing ->
                    ( model, Cmd.none )

                Just pId ->
                    ( set (personRA << demesneA) Nothing model
                    , getDemesne (PersonMessage << DemesneReceived) pId
                    )

        DemesnePageChanged pageNumber ->
            let
                lastPgNumber =
                    model.personR.demesne
                        |> Maybe.withDefault []
                        |> maxPage model.personR.demesnePageSize

                setPage target _ =
                    if target < 0 then
                        0

                    else if target > lastPgNumber then
                        lastPgNumber

                    else
                        target
            in
            ( over (personRA << demesneCurrentPageA) (setPage pageNumber) model
            , Cmd.none
            )

        RelationsStatusChanged status ->
            ( set (personRA << relationsStatusA) status model
            , Cmd.none
            )

        RelationsPageChanged pageNumber ->
            let
                lastPgNumber =
                    get (personRA << personA << try << relationsA) model
                        |> Maybe.withDefault []
                        |> maxPage model.personR.relationsPageSize

                setPage target _ =
                    if target < 0 then
                        0

                    else if target > lastPgNumber then
                        lastPgNumber

                    else
                        target
            in
            ( over (personRA << relationsCurrentPageA) (setPage pageNumber) model
            , Cmd.none
            )

        TraitsStatusChanged status ->
            ( set (personRA << traitsStatusA) status model
            , Cmd.none
            )

        TraitsPageChanged pageNumber ->
            let
                lastPgNumber =
                    get (personRA << personA << try << traitsA) model
                        |> joinMaybe
                        |> Maybe.withDefault []
                        |> maxPage model.personR.traitsPageSize

                setPage target _ =
                    if target < 0 then
                        0

                    else if target > lastPgNumber then
                        lastPgNumber

                    else
                        target
            in
            ( over (personRA << traitsCurrentPageA) (setPage pageNumber) model
            , Cmd.none
            )
