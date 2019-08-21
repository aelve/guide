{-# LANGUAGE FlexibleContexts    #-}

-- | Description : The main module that starts the server.
module Guide.Main
(
  -- * Main
  main,

  -- * All supported commands
  runServer,
  dryRun,
  loadPublic,
  apiDocs,
)
where


import Imports

-- Concurrent
import Control.Concurrent.Async
-- Monads and monad transformers
import Control.Monad.Morph
-- Web
import Lucid hiding (for_)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Spock hiding (get, head, text)
import Web.Spock.Config
import Web.Spock.Lucid
-- Spock-digestive
import Web.Spock.Digestive (runForm)
-- Highlighting
import CMark.Highlight (pygments, styleToCss)
-- acid-state
import Data.Acid as Acid
import Data.SafeCopy as SafeCopy
import Data.Serialize.Get as Cereal
-- IO
import System.IO
-- Catching Ctrl-C and termination
import System.Signal

-- HVect
import Data.HVect hiding (length)

import Guide.Api (runApiServer, apiSwaggerRendered)
import Guide.App
import Guide.Cli
import Guide.Config
import Guide.Handlers
import Guide.JS (JS (..), allJSFunctions)
import Guide.Logger
import Guide.Routes (authRoute, haskellRoute)
import Guide.ServerStuff
import Guide.Session
import Guide.State
import Guide.Types
import Guide.Utils
import Guide.Views
import Guide.Views.Utils (getCSS, getCsrfHeader, getJS, protectForm)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Web.Spock as Spock


{- Note [acid-state]
~~~~~~~~~~~~~~~~~~~~

Until we are done with migrating to PostgreSQL, this app uses acid-state.
Acid-state works as follows:

  * Everything is stored as Haskell values (in particular, all data is stored
    in 'GlobalState').

  * All changes to the state (and all queries) have to be done by using
    'dbUpdate'/'dbQuery' and types (GetItem, SetItemName, etc) from the
    Types.hs module.

  * The data is kept in-memory, but all changes are logged to the disk (which
    lets us recover the state in case of a crash by reapplying the changes)
    and you can't access the state directly. When the application exits, it
    creates a snapshot of the state (called “checkpoint”) and writes it to
    the disk. Additionally, a checkpoint is created every hour (grep for
    “createCheckpoint”).

  * acid-state has a nasty feature – when the state hasn't changed,
    'createCheckpoint' appends it to the previous checkpoint. When state
    doesn't change for a long time, it means that checkpoints can grow to 100
    MB or more. So, we employ a dirty bit and use createCheckpoint' instead
    of createCheckpoint. The former only creates the checkpoint if the dirty
    bit is set, which is good.

  * When any type is changed, we have to write a migration function that
    would read the old version of the type and turn it into the new
    version. This is done by 'changelog' – you only need to provide the list
    of differences between the old type and the new type.

  * There are actually ways to access the state directly (GetGlobalState and
    SetGlobalState), but the latter should only be used when doing something
    one-off (e.g. if you need to migrate all IDs to a different ID scheme).

-}

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

-- | Parse an input and run a command.
main :: IO ()
main = do
  command <- parseCommandLine
  config <- readConfig
  runCommand config command

-- | Run a specific 'Command' with the given 'Config'.
runCommand :: Config -> Command -> IO ()
runCommand config = \case
  RunServer -> runServer config
  DryRun -> dryRun config
  LoadPublic path -> loadPublic config path
  ApiDocs -> apiDocs config

----------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------

-- | Start the server.
runServer :: Config -> IO ()
runServer config@Config{..} = withLogger config $ \logger -> do
  installTerminationCatcher =<< myThreadId
  workAsync <- async $ withDB (pure ()) $ \db -> do
    hSetBuffering stdout NoBuffering
    -- Run checkpoints creator, new and old server concurrently.
    mapConcurrently_ id
      [ checkPoint db
      , runNewApi logger config db
      , runOldServer logger config db
      ]
  -- Hold processes running and finish on exit or exception.
  forever (threadDelay (1000000 * 60))
    `finally` cancel workAsync

-- | Load database from @state/@, check that it can be loaded successfully,
-- and exit.
dryRun :: Config -> IO ()
dryRun config = withLogger config $ \logger -> do
  db :: DB <- openLocalStateFrom "state/" (error "couldn't load state")
  logDebugIO logger "loaded the database successfully"
  closeAcidState db

-- | Load 'PublicDB' from given file, create acid-state database from it,
-- and exit.
loadPublic :: Config -> FilePath -> IO ()
loadPublic config path = withLogger config $ \logger ->
  (Cereal.runGet SafeCopy.safeGet <$> BS.readFile path) >>= \case
    Left err -> error err
    Right publicDB -> do
      db <- openLocalStateFrom "state/" emptyState
      Acid.update db (ImportPublicDB publicDB)
      createCheckpointAndClose' db
      logDebugIO logger "PublicDB imported to GlobalState"

-- | Dump API docs to the output.
apiDocs :: Config -> IO ()
apiDocs config = withLogger config $ \_logger ->
  T.putStrLn apiSwaggerRendered

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

lucidWithConfig
  :: (MonadIO m, HasSpock (ActionCtxT cxt m),
      SpockState (ActionCtxT cxt m) ~ ServerState)
  => HtmlT (ReaderT Config IO) a -> ActionCtxT cxt m a
lucidWithConfig x = do
  cfg <- getConfig
  lucidIO (hoist (flip runReaderT cfg) x)

-- | Create a checkpoint every six hours. Note: if nothing was changed, the
-- checkpoint won't be created, which saves us some space.
checkPoint :: DB -> IO b
checkPoint db = forever $ do
  createCheckpoint' db
  threadDelay (1000000 * 3600 * 6)

-- | Run the API (new server)
runNewApi :: Logger -> Config -> AcidState GlobalState -> IO ()
runNewApi logger = runApiServer (pushLogger "api" logger)

-- | Run Spock (old server).
runOldServer :: Logger -> Config -> DB -> IO ()
runOldServer logger config@Config{..} db = do
  let serverState = ServerState {
        _config = config,
        _db     = db }
  spockConfig <- do
    cfg <- defaultSpockCfg () PCNoDatabase serverState
    store <- newAcidSessionStore db
    let sessionCfg = SessionCfg {
          sc_cookieName = "spockcookie",
          sc_sessionTTL = 3600,
          sc_sessionIdEntropy = 64,
          sc_sessionExpandTTL = True,
          sc_emptySession = emptyGuideData,
          sc_store = store,
          sc_housekeepingInterval = 60 * 10,
          sc_hooks = defaultSessionHooks
        }
    return cfg {
      spc_maxRequestSize = Just (1024*1024),
      spc_csrfProtection = True,
      spc_sessionCfg = sessionCfg }
  logDebugIO logger $ format "Spock is running on port {}" portMain
  runSpockNoBanner portMain $ spock spockConfig guideApp

-- TODO: Fix indentation after rebasing.
guideApp :: GuideApp ()
guideApp = do
    createAdminUser  -- TODO: perhaps it needs to be inside of “prehook
                     -- initHook”? (I don't actually know what “prehook
                     -- initHook” does, feel free to edit.)
    prehook initHook $ do
      middleware (staticPolicy (addBase "static"))
      -- Javascript
      Spock.get "/js.js" $ do
        setHeader "Content-Type" "application/javascript; charset=utf-8"
        (csrfTokenName, csrfTokenValue) <- getCsrfHeader
        let jqueryCsrfProtection =
              format "guidejs.csrfProtection.enable(\"{}\", \"{}\");"
                     csrfTokenName csrfTokenValue
        js <- getJS
        Spock.bytes $ toUtf8ByteString (fromJS allJSFunctions <> js <> jqueryCsrfProtection)
      -- CSS
      Spock.get "/highlight.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        Spock.bytes $ toUtf8ByteString (styleToCss pygments)
      Spock.get "/css.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        css <- getCSS
        Spock.bytes $ toUtf8ByteString css
      Spock.get "/admin.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        css <- getCSS
        admincss <- liftIO $ T.readFile "static/admin.css"
        Spock.bytes $ toUtf8ByteString (css <> admincss)

      -- Main page
      Spock.get root $
        lucidWithConfig renderRoot

      -- Admin page
      prehook authHook $ prehook adminHook $ do
        Spock.get "admin" $ do
          s <- dbQuery GetGlobalState
          lucidIO $ renderAdmin s
        adminMethods
        Spock.get ("admin" <//> "links") $ do
          s <- dbQuery GetGlobalState
          lucidIO $ renderAdminLinks s

      -- Static pages
      Spock.get "markdown" $ lucidWithConfig $
        renderStaticMd "Markdown" "markdown.md"
      Spock.get "license" $ lucidWithConfig $
        renderStaticMd "License" "license.md"

      -- Haskell
      Spock.get (haskellRoute <//> root) $ do
        s <- dbQuery GetGlobalState
        q <- param "q"
        lucidWithConfig $ renderHaskellRoot s q
      -- Category pages
      Spock.get (haskellRoute <//> var) $ \path -> do
        -- The links look like /parsers-gao238b1 (because it's nice when
        -- you can find out where a link leads just by looking at it)
        let (_, catId) = T.breakOnEnd "-" path
        when (T.null catId)
          Spock.jumpNext
        mbCategory <- dbQuery (GetCategoryMaybe (Uid catId))
        case mbCategory of
          Nothing -> Spock.jumpNext
          Just category -> do
            -- If the slug in the url is old (i.e. if it doesn't match the
            -- one we would've generated now), let's do a redirect
            when (categorySlug category /= path) $
              -- TODO: this link shouldn't be absolute [absolute-links]
              Spock.redirect ("/haskell/" <> categorySlug category)
            lucidWithConfig $ renderCategoryPage category
      -- The add/set methods return rendered parts of the structure (added
      -- categories, changed items, etc) so that the Javascript part could
      -- take them and inject into the page. We don't want to duplicate
      -- rendering on server side and on client side.
      methods

      -- plain "/auth" logs out a logged-in user and lets a logged-out user
      -- log in (this is not the best idea, granted, and we should just
      -- show logged-in users a “logout” link and logged-out users a
      -- “login” link instead)
      Spock.get (authRoute <//> root) $ do
        user <- getLoggedInUser
        if isJust user
          then Spock.redirect "/auth/logout"
          else Spock.redirect "/auth/login"
      Spock.getpost (authRoute <//> "login") $ authRedirect "/" loginAction
      Spock.get (authRoute <//> "logout") logoutAction
      Spock.getpost (authRoute <//> "register") $ authRedirect "/" signupAction

loginAction :: GuideAction ctx ()
loginAction = do
  r <- runForm "login" loginForm
  case r of
    (v, Nothing) -> do
      formHtml <- protectForm loginFormView v
      lucidWithConfig $ renderRegister formHtml
    (v, Just Login {..}) -> do
      loginAttempt <- dbQuery $
        LoginUser loginEmail (toUtf8ByteString loginUserPassword)
      case loginAttempt of
        Right user -> do
          modifySession (sessionUserID ?~ userID user)
          Spock.redirect "/"
        -- TODO: *properly* show error message/validation of input
        Left err -> do
          formHtml <- protectForm loginFormView v
          lucidWithConfig $ renderRegister $ do
            div_ $ toHtml ("Error: " <> err)
            formHtml

logoutAction :: GuideAction ctx ()
logoutAction = do
  modifySession (sessionUserID .~ Nothing)
  Spock.redirect "/"

signupAction :: GuideAction ctx ()
signupAction = do
  r <- runForm "register" registerForm
  case r of
    (v, Nothing) -> do
      formHtml <- protectForm registerFormView v
      lucidWithConfig $ renderRegister formHtml
    (v, Just UserRegistration {..}) -> do
      user <- makeUser registerUserName registerUserEmail
                       (toUtf8ByteString registerUserPassword)
      success <- dbUpdate $ CreateUser user
      if success
        then do
          modifySession (sessionUserID ?~ userID user)
          Spock.redirect ""
        else do
          formHtml <- protectForm registerFormView v
          lucidWithConfig $ renderRegister formHtml

initHook :: GuideAction () (HVect '[])
initHook = return HNil

authHook :: GuideAction (HVect xs) (HVect (User ': xs))
authHook = do
  oldCtx <- getContext
  maybeUser <- getLoggedInUser
  case maybeUser of
    Nothing   -> Spock.text "Not logged in."
    Just user -> return (user :&: oldCtx)

adminHook :: ListContains n User xs => GuideAction (HVect xs) (HVect (IsAdmin ': xs))
adminHook = do
  oldCtx <- getContext
  let user = findFirst oldCtx
  if userIsAdmin user
    then return (IsAdmin :&: oldCtx)
    else Spock.text "Not authorized."

-- | Redirect the user to a given path if they are logged in.
authRedirect :: Text -> GuideAction ctx a -> GuideAction ctx a
authRedirect path action = do
  user <- getLoggedInUser
  case user of
    Just _ ->
      Spock.redirect path
    Nothing -> action

-- TODO: a function to find all links to Hackage that have version in them

data Quit = CtrlC | ServiceStop
  deriving (Eq, Ord, Show)

instance Exception Quit

-- | Set up a handler that would catch SIGINT (i.e. Ctrl-C) and SIGTERM
-- (i.e. service stop) and throw an exception instead of the signal. This
-- lets us create a checkpoint and close connections on exit.
installTerminationCatcher
  :: ThreadId  -- ^ Thread to kill when the signal comes
  -> IO ()
installTerminationCatcher thread = void $ do
  installHandler sigINT  (\_ -> throwTo thread CtrlC)
  installHandler sigTERM (\_ -> throwTo thread ServiceStop)

-- | Create an admin user (with login “admin”, email “admin@guide.aelve.com”
-- and password specified in the config).
--
-- The user won't be added if it exists already.
createAdminUser :: GuideApp ()
createAdminUser = do
  dbUpdate DeleteAllUsers
  pass <- toUtf8ByteString . adminPassword <$> getConfig
  user <- makeUser "admin" "admin@guide.aelve.com" pass
  void $ dbUpdate $ CreateUser (user & _userIsAdmin .~ True)
