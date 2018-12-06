module ViewModels.Messages exposing
    ( MessagesRMsg(..)
    , MessagesViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.Messages exposing (NewsArticle, UserIcon)


type alias MessagesViewModel =
    { currentPage : Int
    , pageSize : Int
    , userEntry : String
    , newsPanelStatus : InfoPanelStatus
    , userEntryStatus : InfoPanelStatus
    , activeUserIcon : Maybe UserIcon
    }


type MessagesRMsg
    = UserMessageTextChanged String
    | UserMessageSent UserIcon String
    | UserIconSelected UserIcon
    | NewsPanelStatusChanged InfoPanelStatus
    | NewsPanelRefresh
    | PageChanged Int
    | UserEntryPanelChanged InfoPanelStatus
    | NewsEntryDismissed NewsArticle


init : MessagesViewModel
init =
    { currentPage = 0
    , pageSize = 5
    , userEntry = ""
    , newsPanelStatus = InfoPanelOpen
    , userEntryStatus = InfoPanelOpen
    , activeUserIcon = Nothing
    }
