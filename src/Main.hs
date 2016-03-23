{-# LANGUAGE
OverloadedStrings,
ScopedTypeVariables,
TypeFamilies,
DataKinds,
MultiWayIf,
FlexibleContexts,
NoImplicitPrelude
  #-}


module Main (main) where


-- General
import BasePrelude hiding (Category)
-- Monads and monad transformers
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Morph
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Containers
import qualified Data.Map as M
-- Text
import Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
-- Paths
import System.FilePath ((</>))
-- Web
import Web.Spock hiding (head, get, text)
import qualified Web.Spock as Spock
import Web.Spock.Lucid
import Lucid
import Network.Wai.Middleware.Static
import qualified Network.HTTP.Types.Status as HTTP
-- Feeds
import qualified Text.Feed.Types as Feed
import qualified Text.Feed.Util  as Feed
import qualified Text.Atom.Feed  as Atom
-- Highlighting
import Cheapskate.Highlight
-- Monitoring
import qualified System.Remote.Monitoring as EKG
import qualified Network.Wai.Metrics      as EKG
import qualified System.Metrics.Gauge     as EKG.Gauge
import Data.Generics.Uniplate.Data
-- acid-state
import Data.Acid as Acid
-- Time
import Data.Time

-- Local
import Config
import Types
import View
import JS (JS(..), allJSFunctions)
import Markdown
import Utils


------------------------------------------------------------------------------
-- working with global state via acid-state
------------------------------------------------------------------------------

type DB = AcidState GlobalState

dbUpdate :: (MonadIO m, HasSpock m, SpockState m ~ ServerState,
             EventState event ~ GlobalState, UpdateEvent event)
         => event -> m (EventResult event)
dbUpdate x = do
  db <- _db <$> Spock.getState
  liftIO $ Acid.update db x

dbQuery :: (MonadIO m, HasSpock m, SpockState m ~ ServerState,
            EventState event ~ GlobalState, QueryEvent event)
        => event -> m (EventResult event)
dbQuery x = do
  db <- _db <$> Spock.getState
  liftIO $ Acid.query db x

------------------------------------------------------------------------------
-- Server state
------------------------------------------------------------------------------

data ServerState = ServerState {
  _config :: Config,
  _db     :: DB }

getConfig :: (Monad m, HasSpock m, SpockState m ~ ServerState)
          => m Config
getConfig = _config <$> Spock.getState

itemVar :: Path '[Uid]
itemVar = "item" <//> var

categoryVar :: Path '[Uid]
categoryVar = "category" <//> var

traitVar :: Path '[Uid]
traitVar = "trait" <//> var

renderMethods :: SpockM () () ServerState ()
renderMethods = Spock.subcomponent "render" $ do
  -- Title of a category
  Spock.get (categoryVar <//> "title") $ \catId -> do
    category <- dbQuery (GetCategory catId)
    lucidIO $ renderCategoryTitle category
  -- Notes for a category
  Spock.get (categoryVar <//> "notes") $ \catId -> do
    category <- dbQuery (GetCategory catId)
    lucidIO $ renderCategoryNotes category
  -- Item colors
  Spock.get (itemVar <//> "colors") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    let hue = getItemHue category item
    json $ M.fromList [("light" :: Text, hueToLightColor hue),
                       ("dark" :: Text, hueToDarkColor hue)]
  -- Item info
  Spock.get (itemVar <//> "info") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucidIO $ renderItemInfo category item
  -- Item description
  Spock.get (itemVar <//> "description") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    lucidIO $ renderItemDescription item
  -- Item ecosystem
  Spock.get (itemVar <//> "ecosystem") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    lucidIO $ renderItemEcosystem item
  -- Item notes
  Spock.get (itemVar <//> "notes") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    lucidIO $ renderItemNotes item

setMethods :: SpockM () () ServerState ()
setMethods = Spock.subcomponent "set" $ do
  -- Title of a category
  Spock.post (categoryVar <//> "title") $ \catId -> do
    content' <- param' "content"
    category <- dbUpdate (SetCategoryTitle catId content')
    lucidIO $ renderCategoryTitle category
  -- Notes for a category
  Spock.post (categoryVar <//> "notes") $ \catId -> do
    content' <- param' "content"
    category <- dbUpdate (SetCategoryNotes catId content')
    lucidIO $ renderCategoryNotes category
  -- Item info
  Spock.post (itemVar <//> "info") $ \itemId -> do
    -- TODO: [easy] add a cross-link saying where the form is handled in the
    -- code and other notes saying where stuff is rendered, etc
    name' <- T.strip <$> param' "name"
    link' <- T.strip <$> param' "link"
    kind' <- do
      kindName :: Text <- param' "kind"
      hackageName' <- (\x -> if T.null x then Nothing else Just x) <$>
                      param' "hackage-name"
      return $ case kindName of
        "library" -> Library hackageName'
        "tool"    -> Tool hackageName'
        _         -> Other
    group' <- do
      groupField <- param' "group"
      customGroupField <- param' "custom-group"
      if | groupField == "-"           -> return Nothing
         | groupField == newGroupValue -> return (Just customGroupField)
         | otherwise                   -> return (Just groupField)
    -- Modify the item
    -- TODO: actually validate the form and report errors
    unless (T.null name') $ void $
      dbUpdate (SetItemName itemId name')
    case (T.null link', sanitiseUrl link') of
      (True, _)   -> void $ dbUpdate (SetItemLink itemId Nothing)
      (_, Just l) -> void $ dbUpdate (SetItemLink itemId (Just l))
      _otherwise  -> return ()
    dbUpdate (SetItemKind itemId kind')
    -- This does all the work of assigning new colors, etc. automatically
    dbUpdate (SetItemGroup itemId group')
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucidIO $ renderItemInfo category item
  -- Item description
  Spock.post (itemVar <//> "description") $ \itemId -> do
    content' <- param' "content"
    item <- dbUpdate (SetItemDescription itemId content')
    lucidIO $ renderItemDescription item
  -- Item ecosystem
  Spock.post (itemVar <//> "ecosystem") $ \itemId -> do
    content' <- param' "content"
    item <- dbUpdate (SetItemEcosystem itemId content')
    lucidIO $ renderItemEcosystem item
  -- Item notes
  Spock.post (itemVar <//> "notes") $ \itemId -> do
    content' <- param' "content"
    item <- dbUpdate (SetItemNotes itemId content')
    lucidIO $ renderItemNotes item
  -- Trait
  Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
    content' <- param' "content"
    trait <- dbUpdate (SetTraitContent itemId traitId content')
    lucidIO $ renderTrait itemId trait

addMethods :: SpockM () () ServerState ()
addMethods = Spock.subcomponent "add" $ do
  -- New category
  Spock.post "category" $ do
    title' <- param' "content"
    catId <- randomShortUid
    time <- liftIO getCurrentTime
    newCategory <- dbUpdate (AddCategory catId title' time)
    lucidIO $ renderCategory newCategory
  -- New item in a category
  Spock.post (categoryVar <//> "item") $ \catId -> do
    name' <- param' "name"
    -- TODO: do something if the category doesn't exist (e.g. has been
    -- already deleted)
    itemId <- randomShortUid
    -- If the item name looks like a Hackage library, assume it's a Hackage
    -- library.
    time <- liftIO getCurrentTime
    newItem <- if T.all (\c -> isAscii c && (isAlphaNum c || c == '-')) name'
      then dbUpdate (AddItem catId itemId name' time (Library (Just name')))
      else dbUpdate (AddItem catId itemId name' time Other)
    category <- dbQuery (GetCategory catId)
    lucidIO $ renderItem category newItem
  -- Pro (argument in favor of an item)
  Spock.post (itemVar <//> "pro") $ \itemId -> do
    content' <- param' "content"
    traitId <- randomLongUid
    newTrait <- dbUpdate (AddPro itemId traitId content')
    lucidIO $ renderTrait itemId newTrait
  -- Con (argument against an item)
  Spock.post (itemVar <//> "con") $ \itemId -> do
    content' <- param' "content"
    traitId <- randomLongUid
    newTrait <- dbUpdate (AddCon itemId traitId content')
    lucidIO $ renderTrait itemId newTrait

otherMethods :: SpockM () () ServerState ()
otherMethods = do
  -- Moving things
  Spock.subcomponent "move" $ do
    -- Move item
    Spock.post itemVar $ \itemId -> do
      direction :: Text <- param' "direction"
      dbUpdate (MoveItem itemId (direction == "up"))
    -- Move trait
    Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
      direction :: Text <- param' "direction"
      dbUpdate (MoveTrait itemId traitId (direction == "up"))

  -- Deleting things
  Spock.subcomponent "delete" $ do
    -- Delete item
    Spock.post itemVar $ \itemId -> do
      dbUpdate (DeleteItem itemId)
    -- Delete trait
    Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
      dbUpdate (DeleteTrait itemId traitId)

  -- Feeds
  -- TODO: this link shouldn't be absolute [absolute-links]
  baseUrl <- (</> "haskell") . T.unpack . _baseUrl <$> getConfig
  Spock.subcomponent "feed" $ do
    -- Feed for items in a category
    Spock.get categoryVar $ \catId -> do
      category <- dbQuery (GetCategory catId)
      let sortedItems = reverse $ sortBy cmp (category^.items)
            where cmp = comparing (^.created) <> comparing (^.uid)
      let route = "feed" <//> categoryVar
      let feedUrl = baseUrl </> T.unpack (renderRoute route (category^.uid))
          feedTitle = Atom.TextString (T.unpack (category^.title) ++
                                       " – Aelve Guide")
          feedLastUpdate = case sortedItems of
            (item:_) -> Feed.toFeedDateStringUTC Feed.AtomKind (item^.created)
            _        -> ""
      let feedBase = Atom.nullFeed feedUrl feedTitle feedLastUpdate
      atomFeed $ feedBase {
        Atom.feedEntries = map (itemToFeedEntry baseUrl category) sortedItems,
        Atom.feedLinks   = [Atom.nullLink feedUrl] }

itemToFeedEntry :: String -> Category -> Item -> Atom.Entry
itemToFeedEntry baseUrl category item =
  entryBase {
    Atom.entryLinks = [Atom.nullLink entryLink],
    Atom.entryContent = Just (Atom.HTMLContent (TL.unpack entryContent)) }
  where
    entryLink = baseUrl </>
                T.unpack (format "{}#item-{}"
                                 (categorySlug category, item^.uid))
    entryContent = Lucid.renderText (renderItemForFeed item)
    entryBase = Atom.nullEntry
      (T.unpack (uidToText (item^.uid)))
      (Atom.TextString (T.unpack (item^.name)))
      (Feed.toFeedDateStringUTC Feed.AtomKind (item^.created))

-- TODO: rename GlobalState to DB, and DB to AcidDB

lucidWithConfig
  :: (MonadIO m, HasSpock (ActionCtxT cxt m),
      SpockState (ActionCtxT cxt m) ~ ServerState)
  => HtmlT (ReaderT Config IO) a -> ActionCtxT cxt m a
lucidWithConfig x = do
  cfg <- getConfig
  lucidIO (hoist (flip runReaderT cfg) x)

main :: IO ()
main = do
  config <- readConfig
  let emptyState = GlobalState mempty
  -- When we run in GHCi and we exit the main thread, the EKG thread (that
  -- runs the localhost:5050 server which provides statistics) may keep
  -- running. This makes running this in GHCi annoying, because you have to
  -- restart GHCi before every run. So, we kill the thread in the finaliser.
  ekgId <- newIORef Nothing
  let prepare = openLocalStateFrom "state/" emptyState
      finalise db = do
        createCheckpoint db
        closeAcidState db
        mapM_ killThread =<< readIORef ekgId
  bracket prepare finalise $ \db -> do
    -- Create a checkpoint every hour. Note: if nothing was changed,
    -- acid-state overwrites the previous checkpoint, which saves us some
    -- space.
    forkOS $ forever $ do
      createCheckpoint db
      threadDelay (1000000 * 3600)
    -- EKG metrics
    ekg <- EKG.forkServer "localhost" 5050
    writeIORef ekgId (Just (EKG.serverThreadId ekg))
    waiMetrics <- EKG.registerWaiMetrics (EKG.serverMetricStore ekg)
    categoryGauge <- EKG.getGauge "db.categories" ekg
    itemGauge <- EKG.getGauge "db.items" ekg
    textGauge <- EKG.getGauge "db.text_length" ekg
    forkOS $ forever $ do
      globalState <- Acid.query db GetGlobalState
      let allCategories = globalState^.categories
      let allItems = allCategories^.each.items
      -- Count length of all Text values in global state. This actually
      -- doesn't work because for some reason Uniplate doesn't see Text
      -- inside MarkdownInline and MarkdownBlock, so instead we gather all
      -- Markdown values and look into them manually.
      let textLength =
            sum (map (T.length . markdownInlineText) (childrenBi globalState))
           +sum (map (T.length . markdownBlockText)  (childrenBi globalState))
      EKG.Gauge.set categoryGauge (fromIntegral (length allCategories))
      EKG.Gauge.set itemGauge (fromIntegral (length allItems))
      EKG.Gauge.set textGauge (fromIntegral textLength)
      threadDelay (1000000 * 60)
    -- Run the server
    let serverState = ServerState {
          _config = config,
          _db     = db }
    let spockConfig = (defaultSpockCfg () PCNoDatabase serverState) {
          spc_maxRequestSize = Just (1024*1024) }
    runSpock 8080 $ spock spockConfig $ do
      middleware (EKG.metrics waiMetrics)
      middleware (staticPolicy (addBase "static"))
      -- Javascript
      Spock.get "/js.js" $ do
        setHeader "Content-Type" "application/javascript; charset=utf-8"
        Spock.bytes $ T.encodeUtf8 (fromJS allJSFunctions)
      -- CSS
      Spock.get "/highlight.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        Spock.bytes $ T.encodeUtf8 (T.pack (styleToCss pygments))
      -- (css.css is a static file and so isn't handled here)

      -- Main page
      Spock.get root $
        lucidWithConfig $ do
          head_ $ do
            title_ "Aelve Guide"
            includeCSS "/css.css"
            renderTracking
          body_ $ do
            h1_ "Aelve Guide"
            h2_ (a_ [href_ "/haskell"] "Haskell")

      -- Admin page
      prehook adminHook $ do
        Spock.get "admin" $
          lucid $ "You're an admin!"

      -- Donation page
      Spock.get "donate" $
        lucidWithConfig $ renderDonate

      -- Unwritten rules
      Spock.get "unwritten-rules" $ do
        lucidWithConfig $ renderUnwrittenRules

      -- Haskell
      Spock.subcomponent "haskell" $ do
        Spock.get root $ do
          s <- dbQuery GetGlobalState
          q <- param "q"
          lucidWithConfig $ renderRoot s q
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
        renderMethods
        setMethods
        addMethods
        otherMethods

adminHook :: ActionCtxT ctx (WebStateM () () ServerState) ()
adminHook = do
  adminPassword <- _adminPassword <$> getConfig
  unless (adminPassword == "") $ do
    let check user pass =
          unless (user == "admin" && pass == adminPassword) $ do
            Spock.setStatus HTTP.status401
            Spock.text "Wrong password!"
    Spock.requireBasicAuth "Authenticate (login = admin)" check return

-- TODO: when a category with the same name exists, show an error message and
-- redirect to that other category

-- TODO: a function to find all links to Hackage that have version in them

-- TODO: why not compare Haskellers too? e.g. for April Fools' we could ask
-- people to list their pros and cons

-- TODO: is it indexable by Google? <given that we're hiding text and
-- Googlebot can execute Javascript>
