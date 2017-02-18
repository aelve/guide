{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}


{- |
The main module.

  * Run 'main' to actually start the server.
  * Run 'mainWith' to run it with a custom config.
-}
module Guide.Server
(
  main,
  mainWith,
)
where


import Imports

-- Containers
import qualified Data.Map as M
-- Monads and monad transformers
import Control.Monad.Morph
-- Text
import qualified Data.Text.All as T
-- Web
import Web.Spock hiding (head, get, text)
import qualified Web.Spock as Spock
import Web.Spock.Config
import Web.Spock.Lucid
import Lucid hiding (for_)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import qualified Network.HTTP.Types.Status as HTTP
-- Highlighting
import CMark.Highlight (styleToCss, pygments)
-- Monitoring
import qualified System.Remote.Monitoring as EKG
import qualified Network.Wai.Metrics      as EKG
import qualified System.Metrics.Gauge     as EKG.Gauge
-- acid-state
import Data.Acid as Acid
-- IO
import System.IO
import qualified SlaveThread as Slave
-- Catching signals
import System.Posix.Signals
-- Watching the templates directory
import qualified System.FSNotify as FSNotify

import Guide.ServerStuff
import Guide.Handlers
import Guide.Config
import Guide.State
import Guide.Types
import Guide.Views
import Guide.Views.Utils (getJS, getCSS)
import Guide.JS (JS(..), allJSFunctions)
import Guide.Utils
import Guide.Cache
import Guide.Session


{- Note [acid-state]
~~~~~~~~~~~~~~~~~~~~

This application doesn't use a database – instead, it uses
acid-state. Acid-state works as follows:

  * Everything is stored as Haskell values (in particular, all data is stored
    in 'GlobalState').

  * All changes to the state (and all queries) have to be done by using
    'dbUpdate'/'dbQuery' and types (GetItem, SetItemName, etc) from the
    Types.hs module.

  * When doing a 'dbUpdate', don't forget to 'invalidateCache'! Though in
    most cases you'll likely want to use 'uncache' instead.

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

type GuideApp ctx = SpockCtxM ctx () GuideData ServerState ()

-- type GuideAction ctx a = SpockActionCtx ctx () () ServerState a

-- TODO: rename GlobalState to DB, and DB to AcidDB

lucidWithConfig
  :: (MonadIO m, HasSpock (ActionCtxT cxt m),
      SpockState (ActionCtxT cxt m) ~ ServerState)
  => HtmlT (ReaderT Config IO) a -> ActionCtxT cxt m a
lucidWithConfig x = do
  cfg <- getConfig
  lucidIO (hoist (flip runReaderT cfg) x)

----------------------------------------------------------------------------
-- The entry point
----------------------------------------------------------------------------

-- | Start the site.
main :: IO ()
main = do
  config <- readConfig
  mainWith config

-- | Start the site with a specific 'Config'.
mainWith :: Config -> IO ()
mainWith config = do
  -- Emptying the cache is needed because during development (i.e. in REPL)
  -- 'main' can be started many times and if the cache isn't cleared changes
  -- won't be visible
  emptyCache
  startTemplateWatcher
  let emptyState = GlobalState {
        _categories = [],
        _categoriesDeleted = [],
        _actions = [],
        _pendingEdits = [],
        _editIdCounter = 0,
        _sessionStore = M.empty,
        _dirty = True }
  do args <- getArgs
     when (args == ["--dry-run"]) $ do
       db :: DB <- openLocalStateFrom "state/" (error "couldn't load state")
       putStrLn "loaded the database successfully"
       closeAcidState db
       exitSuccess
  -- When we run in GHCi and we exit the main thread, the EKG thread (that
  -- runs the localhost:5050 server which provides statistics) may keep
  -- running. This makes running this in GHCi annoying, because you have to
  -- restart GHCi before every run. So, we kill the thread in the finaliser.
  ekgId <- newIORef Nothing
  -- See Note [acid-state] for the explanation of 'openLocalStateFrom',
  -- 'createCheckpoint', etc
  let prepare = openLocalStateFrom "state/" emptyState
      finalise db = do
        putStrLn "Creating an acid-state checkpoint and closing acid-state"
        createCheckpointAndClose' db
        -- Killing EKG has to be done last, because of
        -- <https://github.com/tibbe/ekg/issues/62>
        putStrLn "Killing EKG"
        mapM_ killThread =<< readIORef ekgId
  bracket prepare finalise $ \db -> do
    installTerminationCatcher =<< myThreadId
    hSetBuffering stdout NoBuffering
    -- Create a checkpoint every six hours. Note: if nothing was changed, the
    -- checkpoint won't be created, which saves us some space.
    Slave.fork $ forever $ do
      createCheckpoint' db
      threadDelay (1000000 * 3600 * 6)
    -- EKG metrics
    ekg <- EKG.forkServer "localhost" 5050
    writeIORef ekgId (Just (EKG.serverThreadId ekg))
    waiMetrics <- EKG.registerWaiMetrics (EKG.serverMetricStore ekg)
    categoryGauge <- EKG.getGauge "db.categories" ekg
    itemGauge <- EKG.getGauge "db.items" ekg
    Slave.fork $ forever $ do
      globalState <- Acid.query db GetGlobalState
      let allCategories = globalState^.categories
      let allItems = allCategories^..each.items.each
      EKG.Gauge.set categoryGauge (fromIntegral (length allCategories))
      EKG.Gauge.set itemGauge (fromIntegral (length allItems))
      threadDelay (1000000 * 60)
    -- Run the server
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
            sc_emptySession = GuideData (),
            sc_store = store,
            sc_housekeepingInterval = 60 * 10,
            sc_hooks = defaultSessionHooks
          }
      return cfg {
        spc_maxRequestSize = Just (1024*1024),
        spc_csrfProtection = True,
        spc_sessionCfg = sessionCfg }
    when (_prerender config) $ prerenderPages config db
    runSpock 8080 $ spock spockConfig $ guideApp waiMetrics

-- TODO: Fix indentation after rebasing.
guideApp :: EKG.WaiMetrics -> GuideApp ()
guideApp waiMetrics = do
      middleware (EKG.metrics waiMetrics)
      middleware (staticPolicy (addBase "static"))
      -- Javascript
      Spock.get "/js.js" $ do
        setHeader "Content-Type" "application/javascript; charset=utf-8"
        js <- getJS
        Spock.bytes $ T.encodeUtf8 (fromJS allJSFunctions <> js)
      -- CSS
      Spock.get "/highlight.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        Spock.bytes $ T.encodeUtf8 (T.pack (styleToCss pygments))
      Spock.get "/css.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        css <- getCSS
        Spock.bytes $ T.encodeUtf8 css
      Spock.get "/admin.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        css <- getCSS
        admincss <- liftIO $ T.readFile "static/admin.css"
        Spock.bytes $ T.encodeUtf8 (css <> admincss)

      -- Main page
      Spock.get root $
        lucidWithConfig $ renderRoot

      -- Admin page
      prehook adminHook $ do
        Spock.get "admin" $ do
          s <- dbQuery GetGlobalState
          lucidIO $ renderAdmin s
        adminMethods

      -- Donation page
      Spock.get "donate" $
        lucidWithConfig $ renderDonate

      -- Static pages
      Spock.get "unwritten-rules" $ lucidWithConfig $
        renderStaticMd "Unwritten rules" "unwritten-rules.md"
      Spock.get "markdown" $ lucidWithConfig $
        renderStaticMd "Markdown" "markdown.md"
      Spock.get "license" $ lucidWithConfig $
        renderStaticMd "License" "license.md"

      -- Haskell
      Spock.subcomponent "haskell" $ do
        Spock.get root $ do
          s <- dbQuery GetGlobalState
          q <- param "q"
          (time, mbIP, mbReferrer, mbUA) <- getRequestDetails
          let act = case q of
                Nothing -> Action'MainPageVisit
                Just x  -> Action'Search x
          baseUrl <- _baseUrl <$> getConfig
          dbUpdate (RegisterAction act mbIP time baseUrl mbReferrer mbUA)
          lucidWithConfig $ renderHaskellRoot s q
        -- Category pages
        Spock.get var $ \path -> do
          -- The links look like /parsers-gao238b1 (because it's nice when
          -- you can find out where a link leads just by looking at it)
          let (_, catId) = T.breakOnEnd "-" path
          when (T.null catId) $
            Spock.jumpNext
          mbCategory <- dbQuery (GetCategoryMaybe (Uid catId))
          case mbCategory of
            Nothing -> Spock.jumpNext
            Just category -> do
              (time, mbIP, mbReferrer, mbUA) <- getRequestDetails
              baseUrl <- _baseUrl <$> getConfig
              dbUpdate $ RegisterAction (Action'CategoryVisit (Uid catId))
                           mbIP time baseUrl mbReferrer mbUA
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
      
      Spock.subcomponent "auth" $ do
        Spock.get "login" $ lucidWithConfig renderLogin
        
        Spock.get "register" $ lucidWithConfig renderRegister

adminHook :: ActionCtxT ctx (WebStateM () GuideData ServerState) ()
adminHook = do
  adminPassword <- _adminPassword <$> getConfig
  unless (adminPassword == "") $ do
    let check user pass =
          unless (user == "admin" && pass == adminPassword) $ do
            Spock.setStatus HTTP.status401
            Spock.text "Wrong password!"
    Spock.requireBasicAuth "Authenticate (login = admin)" check return

-- TODO: a function to find all links to Hackage that have version in them

-- | During development you need to see the changes whenever you change
-- anything. This function starts a thread that watches for changes in
-- templates and clears the cache whenever a change occurs, so that you
-- wouldn't see cached pages.
startTemplateWatcher :: IO ()
startTemplateWatcher = void $ do
  Slave.fork $ FSNotify.withManager $ \mgr -> do
    FSNotify.watchTree mgr "templates/" (const True) $ \_ -> do
      emptyCache
    forever $ threadDelay 1000000

-- | Render all pages and put them into the cache, so that (unlucky) users
-- wouldn't see delays after a restart of the site.
--
-- Well, actually instead unlucky users would see an error after a restart of
-- the site until prerendering completes, which is probably worse.
--
-- TODO: make prerendering asynchronous.
prerenderPages :: Config -> DB -> IO ()
prerenderPages config db = do
  putStr "Prerendering pages to be cached... "
  globalState <- Acid.query db GetGlobalState
  for_ (globalState^.categories) $ \cat -> do
    putStr "|"
    evaluate . force =<<
      renderBST (hoist (flip runReaderT config) (renderCategoryPage cat))
  putStrLn " done"

data Quit = CtrlC | ServiceStop
  deriving (Eq, Ord, Show)

instance Exception Quit

{- | Set up a handler that would catch SIGINT (i.e. Ctrl-C) and SIGTERM
(i.e. service stop) and throw an exception instead of them. This lets us
create a checkpoint and close connections on exit.
-}
installTerminationCatcher :: ThreadId -> IO ()
installTerminationCatcher thread = void $ do
  installHandler sigINT  (CatchOnce (throwTo thread CtrlC))       Nothing
  installHandler sigTERM (CatchOnce (throwTo thread ServiceStop)) Nothing
