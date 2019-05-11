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
        )


type ResearchRMsg
    = CurrentResearchDetailsStatusChanged InfoPanelStatus
    | ResearchFieldDetailsStatusChanged InfoPanelStatus
    | ProductionStatusChanged InfoPanelStatus
    | ProjectFocused (Maybe TopResearchCategory)
    | ProjectStarted Research
    | ProjectCancelled CurrentResearch


type alias ResearchViewModel =
    { currentResearchStatus : InfoPanelStatus
    , researchFieldStatus : InfoPanelStatus
    , productionStatus : InfoPanelStatus
    , focusedTopCategory : Maybe TopResearchCategory
    }


{-| Create initial research view model
-}
init : ResearchViewModel
init =
    { currentResearchStatus = InfoPanelOpen
    , researchFieldStatus = InfoPanelOpen
    , productionStatus = InfoPanelOpen
    , focusedTopCategory = Nothing
    }