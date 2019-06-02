module Api.Construction exposing
    ( availableBuildingsCmd
    , constructionIdDecoder
    , constructionsCmd
    , deleteConstructionCmd
    , getAvailableBuildingsCmd
    , getConstructionsCmd
    , postBuildingConstructionCmd
    , putConstructionCmd
    )

import Api.Common
    exposing
        ( delete
        , get
        , planetIdDecoder
        , planetIdEncoder
        , post
        , put
        , resourcesDecoder
        , resourcesEncoder
        )
import Api.Endpoints exposing (Endpoint(..), endpointToString)
import Api.StarSystem
    exposing
        ( buildingLevelDecoder
        , buildingLevelEncoder
        , buildingTypeDecoder
        , buildingTypeEncoder
        )
import Data.Common exposing (ConstructionId(..), PlanetId)
import Data.Construction
    exposing
        ( BuildingConstructionData
        , BuildingInfo
        , Construction(..)
        , ConstructionIndex(..)
        , ShipConstructionData
        )
import Data.Model
    exposing
        ( ApiMsg(..)
        , Model
        , Msg(..)
        )
import Http
import Json.Decode as Decode
    exposing
        ( andThen
        , fail
        , field
        , int
        , list
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Extra exposing (andMap)
import Json.Encode as Encode


{-| Command to load currently ongoing constructions of a planet
-}
constructionsCmd : PlanetId -> Cmd Msg
constructionsCmd planetId =
    Http.send (ApiMsgCompleted << ConstructionsReceived) (get (ApiConstructionQueue planetId) (list constructionDecoder))


{-| Create command to load currently ongoing constructions of a planet
if they have not yet been retrieved
-}
getConstructionsCmd : Model -> PlanetId -> Cmd Msg
getConstructionsCmd model =
    constructionsCmd


{-| Add new construction queue item
-}
postBuildingConstructionCmd : Construction -> Cmd Msg
postBuildingConstructionCmd construction =
    Http.send (ApiMsgCompleted << ConstructionsReceived)
        (post ApiBuildingConstruction (constructionEncoder construction) (list constructionDecoder))


{-| Modify existing construction queue item
-}
putConstructionCmd : Construction -> Cmd Msg
putConstructionCmd construction =
    Http.send (ApiMsgCompleted << ConstructionsReceived)
        (put (ApiConstruction construction) (constructionEncoder construction) (list constructionDecoder))


{-| Delete construction from construction queue
-}
deleteConstructionCmd : Construction -> Cmd Msg
deleteConstructionCmd construction =
    Http.send (ApiMsgCompleted << ConstructionsReceived)
        (delete (ApiConstruction construction) Nothing (list constructionDecoder))


{-| Command to retrieve buildings available for construction
-}
availableBuildingsCmd : Cmd Msg
availableBuildingsCmd =
    Http.send (ApiMsgCompleted << AvailableBuildingsReceived) (get ApiAvailableBuildings (list buildingInfoDecoder))


{-| Check provided model and if buildings available for construction
has not been fetched, create a command to do so
-}
getAvailableBuildingsCmd : Model -> Cmd Msg
getAvailableBuildingsCmd model =
    availableBuildingsCmd


{-| Decoder for Construction
-}
constructionDecoder : Decode.Decoder Construction
constructionDecoder =
    oneOf
        [ succeed BuildingConstruction |> andMap buildingConstructionDecoder
        , succeed ShipConstruction |> andMap shipConstructionDecoder
        ]


{-| Decoder for building construction
This is used by construction decoder to decode json data
-}
buildingConstructionDecoder : Decode.Decoder BuildingConstructionData
buildingConstructionDecoder =
    succeed BuildingConstructionData
        |> andMap (field "id" constructionIdDecoder)
        |> andMap (field "name" string)
        |> andMap (field "index" constructionIndexDecoder)
        |> andMap (field "level" buildingLevelDecoder)
        |> andMap (field "type" buildingTypeDecoder)
        |> andMap (field "planet" planetIdDecoder)
        |> andMap (field "costLeft" resourcesDecoder)


{-| Encode Construction into json data
-}
constructionEncoder : Construction -> Encode.Value
constructionEncoder construction =
    case construction of
        BuildingConstruction x ->
            buildingConstructionEncoder x

        ShipConstruction x ->
            shipConstructionEncoder x


{-| Encode building construction into json data.
This is used by construction encoder
-}
buildingConstructionEncoder : BuildingConstructionData -> Encode.Value
buildingConstructionEncoder building =
    Encode.object
        [ ( "id", constructionIdEncoder building.id )
        , ( "name", Encode.string building.name )
        , ( "index", constructionIndexEncoder building.index )
        , ( "level", buildingLevelEncoder building.level )
        , ( "type", buildingTypeEncoder building.buildingType )
        , ( "planet", planetIdEncoder building.planet )
        , ( "costLeft", resourcesEncoder building.workLeft )
        ]


{-| Placeholder for decoder handling ship constructions.
Currently it will always fail
-}
shipConstructionDecoder : Decode.Decoder ShipConstructionData
shipConstructionDecoder =
    fail "fail"


{-| Placeholder for ship construction encoder.
Currently it will always encode "not implemented"
-}
shipConstructionEncoder : ShipConstructionData -> Encode.Value
shipConstructionEncoder ship =
    Encode.string "not implemented"


{-| Decoder for construction id
-}
constructionIdDecoder : Decode.Decoder ConstructionId
constructionIdDecoder =
    succeed ConstructionId
        |> andMap int


{-| Encoder for construction id
-}
constructionIdEncoder : ConstructionId -> Encode.Value
constructionIdEncoder (ConstructionId x) =
    Encode.int x


{-| Decoder for construction index
-}
constructionIndexDecoder : Decode.Decoder ConstructionIndex
constructionIndexDecoder =
    succeed ConstructionIndex
        |> andMap int


{-| Encoder for construction index
-}
constructionIndexEncoder : ConstructionIndex -> Encode.Value
constructionIndexEncoder (ConstructionIndex index) =
    Encode.int index


{-| Decoder for building info
-}
buildingInfoDecoder : Decode.Decoder BuildingInfo
buildingInfoDecoder =
    succeed BuildingInfo
        |> andMap (field "id" buildingTypeDecoder)
        |> andMap (field "name" string)
        |> andMap (field "level" buildingLevelDecoder)
        |> andMap (field "cost" resourcesDecoder)
        |> andMap (field "description" string)
