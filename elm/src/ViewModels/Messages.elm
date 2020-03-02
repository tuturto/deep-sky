module ViewModels.Messages exposing
    ( MessagesRMsg(..)
    , MessagesViewModel
    , init
    )

import Data.Common exposing (InfoPanelStatus(..))
import Data.Messages exposing (NewsArticle, UserIcon)
import RemoteData exposing (RemoteData(..), WebData)
import SaveData exposing (SaveData(..))


type alias MessagesViewModel =
    { currentPage : Int
    , pageSize : Int
    , userEntry : String
    , newsPanelStatus : InfoPanelStatus
    , userEntryStatus : InfoPanelStatus
    , activeUserIcon : Maybe UserIcon
    , icons : WebData (List ( UserIcon, String ))
    , news : SaveData (List NewsArticle)
    }


type MessagesRMsg
    = UserMessageTextChanged String
    | UserMessageSent UserIcon String
    | SpecialEventChoiceMade NewsArticle
    | UserIconSelected UserIcon
    | NewsPanelStatusChanged InfoPanelStatus
    | NewsPanelRefresh
    | PageChanged Int
    | UserEntryPanelChanged InfoPanelStatus
    | NewsEntryDismissed NewsArticle
    | NewsReceived (SaveData (List NewsArticle))
    | IconsReceived (WebData (List ( UserIcon, String )))


init : MessagesViewModel
init =
    { currentPage = 0
    , pageSize = 5
    , userEntry = ""
    , news = RData Loading
    , icons = Loading
    , newsPanelStatus = InfoPanelOpen
    , userEntryStatus = InfoPanelOpen
    , activeUserIcon = Nothing
    }
