module Views.StarSystems exposing
    ( init
    , isLoading
    , page
    , update
    )

import Accessors exposing (over, set)
import Api.StarSystem exposing (getStarSystems)
import Data.Accessors
    exposing
        ( errorsA
        , starSystemsRA
        , systemsA
        , systemsCurrentPageA
        , systemsStatusA
        )
import Data.Common
    exposing
        ( InfoPanelStatus(..)
        , Location(..)
        , Route(..)
        , error
        , locationToString
        , maxPage
        , unStarSystemName
        )
import Data.Model exposing (Model, Msg(..))
import Data.StarSystem exposing (StarSystem)
import Data.User exposing (Role(..))
import Html
    exposing
        ( Html
        , div
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..))
import ViewModels.StarSystems exposing (StarSystemsRMsg(..))
import Views.Helpers
    exposing
        ( infoPanel
        , pushUrl
        , starDateToString
        )


page : Model -> Html Msg
page model =
    div [] <| systemsList model


systemsList : Model -> List (Html Msg)
systemsList model =
    infoPanel
        { title = "Star systems"
        , currentStatus = model.starSystemsR.systemsStatus
        , openingMessage = StarSystemsMessage <| SystemsStatusChanged InfoPanelOpen
        , closingMessage = StarSystemsMessage <| SystemsStatusChanged InfoPanelClosed
        , refreshMessage = Nothing
        }
        (Just
            { pageSize = model.starSystemsR.systemsPageSize
            , currentPage = model.starSystemsR.systemsCurrentPage
            , maxPage =
                model.starSystemsR.systems
                    |> RemoteData.withDefault []
                    |> maxPage model.starSystemsR.systemsPageSize
            , pageChangedMessage = StarSystemsMessage << SystemsPageChanged
            }
        )
        systemsListContent
        model


systemsListContent : Model -> List (Html Msg)
systemsListContent model =
    -- name, location, date
    let
        content =
            case model.starSystemsR.systems of
                NotAsked ->
                    []

                Loading ->
                    []

                Success systems ->
                    List.indexedMap systemRow systems

                Failure _ ->
                    []
    in
    [ table []
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Location" ]
                , th [] [ text "Date" ]
                ]
            ]
        , tbody [] content
        ]
    ]


systemRow : Int -> StarSystem -> Html Msg
systemRow i starSystem =
    let
        index =
            String.fromInt (i + 1)
    in
    tr
        [ id ("system-entry-" ++ index)
        , onClick <| StarSystemsMessage (ViewSystemRequested starSystem.id)
        ]
        [ td [ id <| "system-name-" ++ index ] [ text (unStarSystemName starSystem.name) ]
        , td [ id <| "system-location-" ++ index ] [ text <| locationToString starSystem.location ]
        , td [ id <| "system-date-" ++ index ] [ text <| starDateToString starSystem.date ]
        ]


init : Model -> Cmd Msg
init _ =
    getStarSystems (StarSystemsMessage << StarSystemsReceived)


update : StarSystemsRMsg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SystemsStatusChanged status ->
            ( set (starSystemsRA << systemsStatusA) status model
            , Cmd.none
            )

        SystemsPageChanged n ->
            ( set (starSystemsRA << systemsCurrentPageA) n model
            , Cmd.none
            )

        ViewSystemRequested systemId ->
            ( model
            , pushUrl model (StarSystemR systemId)
            )

        StarSystemsReceived NotAsked ->
            ( model
            , Cmd.none
            )

        StarSystemsReceived Loading ->
            ( model
            , Cmd.none
            )

        StarSystemsReceived (Success systems) ->
            ( set (starSystemsRA << systemsA) (Success systems) model
            , Cmd.none
            )

        StarSystemsReceived (Failure err) ->
            ( over errorsA (\errors -> error err "Failed to load star systems" :: errors) model
                |> set (starSystemsRA << systemsA) (Failure err)
            , Cmd.none
            )


isLoading : Model -> Bool
isLoading model =
    RemoteData.isLoading model.starSystemsR.systems
