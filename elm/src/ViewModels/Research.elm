module ViewModels.Research exposing
    ( ResearchRMsg(..)
    , ResearchViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.Research
    exposing
        ( CurrentResearch
        , Research
        , TopResearchCategory
        , TotalResearchScore
        )
import RemoteData exposing (RemoteData(..), WebData)
import SaveData exposing (SaveData(..))


type ResearchRMsg
    = CurrentResearchDetailsStatusChanged InfoPanelStatus
    | ResearchFieldDetailsStatusChanged InfoPanelStatus
    | ProductionStatusChanged InfoPanelStatus
    | ProjectFocused (Maybe TopResearchCategory)
    | ProjectStarted Research
    | ProjectCancelled CurrentResearch
    | AvailableResearchReceived (WebData (List Research))
    | CurrentResearchReceived (SaveData (List CurrentResearch))
    | ResearchProductionReceived (WebData TotalResearchScore)


type alias ResearchViewModel =
    { currentResearchStatus : InfoPanelStatus
    , researchFieldStatus : InfoPanelStatus
    , productionStatus : InfoPanelStatus
    , focusedTopCategory : Maybe TopResearchCategory
    , researchProduction : WebData TotalResearchScore
    , currentResearch : SaveData (List CurrentResearch)
    , availableResearch : WebData (List Research)
    }


{-| Create initial research view model
-}
init : ResearchViewModel
init =
    { currentResearchStatus = InfoPanelOpen
    , researchFieldStatus = InfoPanelOpen
    , productionStatus = InfoPanelOpen
    , focusedTopCategory = Nothing
    , researchProduction = Loading
    , currentResearch = RData Loading
    , availableResearch = Loading
    }
