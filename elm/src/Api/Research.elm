module Api.Research exposing
    ( availableResearchCmd
    , cancelResearchCmd
    , currentResearchCmd
    , researchDecoder
    , researchProductionCmd
    , researchTierDecoder
    , startResearchCmd
    , technologyDecoder
    )

import Api.Common
    exposing
        ( delete
        , get
        , is
        , post
        )
import Api.Endpoints exposing (Endpoint(..))
import Data.Model exposing (ApiMsg(..), Msg(..))
import Data.Research
    exposing
        ( CurrentResearch
        , EngineeringSubField(..)
        , NaturalScienceSubField(..)
        , Research
        , ResearchCategory(..)
        , ResearchScore(..)
        , ResearchTier(..)
        , SocialScienceSubField(..)
        , Technology(..)
        , TotalResearchScore
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
import Json.Decode.Extra exposing (andMap, when)
import Json.Encode as Encode


availableResearchCmd : Cmd Msg
availableResearchCmd =
    Http.send (ApiMsgCompleted << AvailableResearchReceived) (get ApiAvailableResearch (list researchDecoder))


currentResearchCmd : Cmd Msg
currentResearchCmd =
    Http.send (ApiMsgCompleted << CurrentResearchReceived) (get ApiCurrentResearch (list currentResearchDecoder))


startResearchCmd : CurrentResearch -> Cmd Msg
startResearchCmd research =
    Http.send (ApiMsgCompleted << CurrentResearchReceived) (post ApiCurrentResearch (currentResearchEncoder research) (list currentResearchDecoder))


cancelResearchCmd : CurrentResearch -> Cmd Msg
cancelResearchCmd research =
    Http.send (ApiMsgCompleted << CurrentResearchReceived) (delete ApiCurrentResearch (Just <| currentResearchEncoder research) (list currentResearchDecoder))


researchProductionCmd : Cmd Msg
researchProductionCmd =
    Http.send (ApiMsgCompleted << ResearchProductionReceived) (get ApiResearchProduction totalResearchScoreDecoder)


researchDecoder : Decode.Decoder Research
researchDecoder =
    succeed Research
        |> andMap (field "Name" string)
        |> andMap (field "Type" technologyDecoder)
        |> andMap (field "Category" categoryDecoder)
        |> andMap (field "Antecedents" (list technologyDecoder))
        |> andMap (field "Cost" totalResearchScoreDecoder)
        |> andMap (field "Tier" researchTierDecoder)


researchEncoder : Research -> Encode.Value
researchEncoder research =
    Encode.object
        [ ( "Name", Encode.string research.name )
        , ( "Type", technologyEncoder research.researchType )
        , ( "Category", categoryEncoder research.category )
        , ( "Antecedents", Encode.list technologyEncoder research.antecedents )
        , ( "Cost", totalResearchScoreEncoder research.cost )
        , ( "Tier", researchTierEncoder research.tier )
        ]


currentResearchDecoder : Decode.Decoder CurrentResearch
currentResearchDecoder =
    succeed CurrentResearch
        |> andMap (field "Research" researchDecoder)
        |> andMap (field "Progress" researchScoreDecoder)


currentResearchEncoder : CurrentResearch -> Encode.Value
currentResearchEncoder research =
    Encode.object
        [ ( "Research", researchEncoder research.research )
        , ( "Progress", researchScoreEncoder research.progress )
        ]


technologyEncoder : Technology -> Encode.Value
technologyEncoder (Technology s) =
    Encode.string s


technologyDecoder : Decode.Decoder Technology
technologyDecoder =
    succeed Technology
        |> andMap string


researchTierDecoder : Decode.Decoder ResearchTier
researchTierDecoder =
    succeed ResearchTier
        |> andMap int


researchTierEncoder : ResearchTier -> Encode.Value
researchTierEncoder (ResearchTier n) =
    Encode.int n


totalResearchScoreDecoder : Decode.Decoder TotalResearchScore
totalResearchScoreDecoder =
    succeed TotalResearchScore
        |> andMap (field "Engineering" researchScoreDecoder)
        |> andMap (field "Natural" researchScoreDecoder)
        |> andMap (field "Social" researchScoreDecoder)


totalResearchScoreEncoder : TotalResearchScore -> Encode.Value
totalResearchScoreEncoder totalScore =
    Encode.object
        [ ( "Engineering", researchScoreEncoder totalScore.engineering )
        , ( "Natural", researchScoreEncoder totalScore.natural )
        , ( "Social", researchScoreEncoder totalScore.social )
        ]


researchScoreDecoder : Decode.Decoder ResearchScore
researchScoreDecoder =
    succeed ResearchScore
        |> andMap int


researchScoreEncoder : ResearchScore -> Encode.Value
researchScoreEncoder (ResearchScore n) =
    Encode.int n


categoryDecoder : Decode.Decoder ResearchCategory
categoryDecoder =
    oneOf
        [ when (field "tag" string)
            (is "Engineering")
            (succeed Engineering
                |> andMap (field "contents" engineeringSubFieldDecoder)
            )
        , when (field "tag" string)
            (is "NaturalScience")
            (succeed NaturalScience
                |> andMap (field "contents" naturalScienceSubFieldDecoder)
            )
        , when (field "tag" string)
            (is "SocialScience")
            (succeed SocialScience
                |> andMap (field "contents" socialScienceSubFieldDecoder)
            )
        ]


categoryEncoder : ResearchCategory -> Encode.Value
categoryEncoder category =
    case category of
        Engineering eng ->
            Encode.object
                [ ( "tag", Encode.string "Engineering" )
                , ( "contents", engineeringSubFieldEncoder eng )
                ]

        NaturalScience nat ->
            Encode.object
                [ ( "tag", Encode.string "NaturalScience" )
                , ( "contents", naturalScienceSubFieldEncoder nat )
                ]

        SocialScience soc ->
            Encode.object
                [ ( "tag", Encode.string "SocialScience" )
                , ( "contents", socialScienceSubFieldEncoder soc )
                ]


engineeringSubFieldDecoder : Decode.Decoder EngineeringSubField
engineeringSubFieldDecoder =
    string |> andThen stringToEngSubField


stringToEngSubField : String -> Decode.Decoder EngineeringSubField
stringToEngSubField s =
    case s of
        "Industry" ->
            succeed Industry

        "Materials" ->
            succeed Materials

        "Propulsion" ->
            succeed Propulsion

        "FieldManipulation" ->
            succeed FieldManipulation

        _ ->
            fail <| "unknown engineering sub field: " ++ s


engineeringSubFieldEncoder : EngineeringSubField -> Encode.Value
engineeringSubFieldEncoder eng =
    case eng of
        Industry ->
            Encode.string "Industry"

        Materials ->
            Encode.string "Materials"

        Propulsion ->
            Encode.string "Propulsion"

        FieldManipulation ->
            Encode.string "FieldManipulation"


naturalScienceSubFieldDecoder : Decode.Decoder NaturalScienceSubField
naturalScienceSubFieldDecoder =
    string |> andThen stringToNatSubField


stringToNatSubField : String -> Decode.Decoder NaturalScienceSubField
stringToNatSubField s =
    case s of
        "Computing" ->
            succeed Computing

        "EnergyManipulation" ->
            succeed EnergyManipulation

        "Particles" ->
            succeed Particles

        "Biology" ->
            succeed Biology

        _ ->
            fail <| "unknown natural science sub field: " ++ s


naturalScienceSubFieldEncoder : NaturalScienceSubField -> Encode.Value
naturalScienceSubFieldEncoder nat =
    case nat of
        Computing ->
            Encode.string "Computing"

        EnergyManipulation ->
            Encode.string "EnergyManipulation"

        Particles ->
            Encode.string "Particles"

        Biology ->
            Encode.string "Biology"


socialScienceSubFieldDecoder : Decode.Decoder SocialScienceSubField
socialScienceSubFieldDecoder =
    string |> andThen stringToSocScience


stringToSocScience : String -> Decode.Decoder SocialScienceSubField
stringToSocScience s =
    case s of
        "MilitaryTheory" ->
            succeed MilitaryTheory

        "StateCraft" ->
            succeed Statecraft

        "ShadowOps" ->
            succeed ShadowOps

        _ ->
            fail <| "unknown social science sub field: " ++ s


socialScienceSubFieldEncoder : SocialScienceSubField -> Encode.Value
socialScienceSubFieldEncoder soc =
    case soc of
        MilitaryTheory ->
            Encode.string "MilitaryTheory"

        Statecraft ->
            Encode.string "Statecraft"

        ShadowOps ->
            Encode.string "ShadowOps"
