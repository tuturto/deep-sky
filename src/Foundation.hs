{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE InstanceSigs          #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)
import qualified MenuHelpers
import Resources (RawResources(..), RawResource(..))

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import Yesod.Auth.OpenId    (authOpenId, IdentifierType (Claimed))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import CustomTypes ( UserIdentity(..) )

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        (fromMaybe (getApprootText guessApproot app req)
            (appRoot $ appSettings app))

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        let mAvatarId = (userAvatar . snd) <$> muser

        mcurrentRoute <- getCurrentRoute
        adminOnly <- case muser of
                        Just (userId, _) -> runDB $ MenuHelpers.isAdmin userId
                        _ -> return False

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs
        currentStarDate <- runDB MenuHelpers.starDate
        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Profile"
                    , menuItemRoute = ProfileR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Star Systems"
                    , menuItemRoute = StarSystemsR
                    , menuItemAccessCallback = isJust mAvatarId
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Bases"
                    , menuItemRoute = BasesR
                    , menuItemAccessCallback = isJust mAvatarId
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Fleet"
                    , menuItemRoute = FleetR
                    , menuItemAccessCallback = isJust mAvatarId
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Designer"
                    , menuItemRoute = DesignerR
                    , menuItemAccessCallback = isJust mAvatarId
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Research"
                    , menuItemRoute = ResearchR
                    , menuItemAccessCallback = isJust mAvatarId
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Construction"
                    , menuItemRoute = ConstructionR
                    , menuItemAccessCallback = isJust mAvatarId
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Messages"
                    , menuItemRoute = MessageR
                    , menuItemAccessCallback = isJust mAvatarId
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Admin"
                    , menuItemRoute = AdminPanelR
                    , menuItemAccessCallback = adminOnly
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        -- TODO: remove unCost and just pass total resources to hamlet template
        totalResources <- runDB $ MenuHelpers.statusBarScore muser
        let statusBarBiologicals = (unRawResource . ccdBiologicalCost) totalResources
        let statusBarMechanicals = (unRawResource . ccdMechanicalCost) totalResources
        let statusBarChemicals = (unRawResource . ccdChemicalCost) totalResources

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication
    isAuthorized (AuthR _) _         = return Authorized
    isAuthorized HomeR _             = return Authorized
    isAuthorized FaviconR _          = return Authorized
    isAuthorized RobotsR _           = return Authorized
    isAuthorized (StaticR _) _       = return Authorized

    -- Routes requiring authentication
    isAuthorized ApiStarDateR _       = isAuthenticated
    isAuthorized ApiResourcesR _      = isAuthenticated
    isAuthorized ProfileR _           = isAuthenticated
    isAuthorized FactionR _           = isAuthenticated
    isAuthorized NewHomeR _           = isAuthenticated
    isAuthorized MessageR _           = isAuthenticated
    -- Routes requiring faction membership
    isAuthorized StarSystemsR _       = hasAvatar
    isAuthorized (StarSystemR _) _    = hasAvatar
    isAuthorized (PlanetR _) _        = hasAvatar
    isAuthorized BasesR _             = hasAvatar
    isAuthorized BaseR {} _           = hasAvatar
    isAuthorized ResearchR _          = hasAvatar
    isAuthorized FleetR _             = hasAvatar
    isAuthorized DesignerR _          = hasAvatar
    isAuthorized ConstructionR _      = hasAvatar
    isAuthorized (PersonR _) _        = hasAvatar
    isAuthorized PeopleR _            = hasAvatar
    isAuthorized (UnitR _) _          = hasAvatar

    -- Special authorization for admin panel
    isAuthorized AdminPanelR _        = isAdmin
    isAuthorized AdminPeopleR _       = isAdmin
    isAuthorized (AdminPersonR _) _   = isAdmin
    isAuthorized AdminAddPersonR _    = isAdmin

    -- Admin API routes
    isAuthorized AdminApiSimulationR _              = return Authorized
    isAuthorized AdminApiPeopleR _                  = return Authorized
    isAuthorized (AdminApiPersonR _) _              = return Authorized
    isAuthorized AdminApiAddPersonR _               = return Authorized

    -- API routes
    isAuthorized ApiStarSystemsR _                  = return Authorized
    isAuthorized (ApiStarSystemR _) _               = return Authorized
    isAuthorized ApiStarsR _                        = return Authorized
    isAuthorized ApiComponentsR _                   = return Authorized
    isAuthorized ApiChassisR _                      = return Authorized
    isAuthorized ApiDesignR _                       = return Authorized
    isAuthorized (ApiDesignIdR _) _                 = return Authorized
    isAuthorized ApiBuildingsR _                    = return Authorized
    isAuthorized ApiAllPlanetsR _                   = return Authorized
    isAuthorized (ApiPlanetR _) _                   = return Authorized
    isAuthorized (ApiPlanetBuildingsR _) _          = return Authorized
    isAuthorized (ApiPlanetPopulationR _) _         = return Authorized
    isAuthorized (ApiPlanetConstQueueR _) _         = return Authorized
    isAuthorized (ApiPlanetStatusR _) _             = return Authorized
    isAuthorized ApiBuildingConstructionR _         = return Authorized
    isAuthorized (ApiBuildingConstructionIdR _) _   = return Authorized
    isAuthorized ApiMessageR _                      = return Authorized
    isAuthorized (ApiMessageIdR _) _                = return Authorized
    isAuthorized ApiMessageIconsR _                 = return Authorized
    isAuthorized ApiAvailableResearchR _            = return Authorized
    isAuthorized ApiCurrentResearchR _              = return Authorized
    isAuthorized ApiResearchProductionR _           = return Authorized
    isAuthorized (ApiPersonR _) _                   = return Authorized
    isAuthorized (ApiDemesneR _) _                  = return Authorized
    isAuthorized ApiDoDesignEstimateR _             = return Authorized
    isAuthorized ApiUnitsR _                        = return Authorized
    isAuthorized (ApiUnitR _) _                     = return Authorized
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger


-- | Authorize admin user
isAdmin :: HandlerFor App AuthResult
isAdmin = do
    muid <- maybeAuthId
    MenuHelpers.authorizeAdmin muid


-- | Authorize user that has logged in and have selected an avatar
hasAvatar :: Handler AuthResult
hasAvatar = do
    muser <- maybeAuthPair
    return $ case (userAvatar . snd) <$> muser of
                Nothing ->
                    Unauthorized "You must have an avatar"

                Just _ ->
                    Authorized


-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    -- This is provided and required by Yesod, but not used for anything currently
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb _ = return ("", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser (UserIdentity $ credsIdent creds)
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userIdent = UserIdentity $ credsIdent creds
                , userPassword = Nothing
                , userAvatar = Nothing
                }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = authOpenId Claimed [] : extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
