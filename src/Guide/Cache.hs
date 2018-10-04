{- |
HTML cache for pages and parts of pages (such as rendered items, traits,
etc). Be careful – the cache is a top-level global variable.
-}
module Guide.Cache
(
  CacheKey(..),
  invalidateCache,
  emptyCache,
  cached,
)
where


import Imports

-- Lucid
import Lucid.Base

import Guide.State
import Guide.Types
import Guide.Utils

-- Concurrent map
import qualified Focus
import qualified STMContainers.Map as STMMap

{- | The cache is a (concurrent) map of rendered HTML for various pages and
pieces of pages.

* 'Left' means that someone has started rendering the piece but hasn't
  finished yet; the 'Unique' specifies who started the rendering.

* 'Right' gives you the result of a complete render.
-}
type Cache = STMMap.Map CacheKey (Either Unique LByteString)

-- | The actual global cache.
cache :: Cache
{-# NOINLINE cache #-}
cache = unsafePerformIO STMMap.newIO

-- | 'CacheKey' specifies what exactly we're caching. Currently we only cache
-- pieces of pages, never pages themselves.
data CacheKey
    -- | List of categories on the main page
  = CacheCategoryList

    -- | Whole category (but not the wrapper page)
  | CacheCategory (Uid Category)
    -- | The header with category name + the edit form + possibly status banner
  | CacheCategoryInfo (Uid Category)
    -- | Category description
  | CacheCategoryNotes (Uid Category)

    -- | Whole item (but not the wrapper page)
  | CacheItem (Uid Item)
    -- | Item header
  | CacheItemInfo (Uid Item)
    -- | Item summary
  | CacheItemDescription (Uid Item)
    -- | Item ecosystem
  | CacheItemEcosystem (Uid Item)
    -- | Item “traits” section
  | CacheItemTraits (Uid Item)
    -- | Item notes
  | CacheItemNotes (Uid Item)
  deriving (Show, Eq, Generic)

instance Hashable CacheKey

-- | Determine which parts should be re-rendered when a given part
-- updates. (Including the given part itself.)
cacheDepends :: GlobalState -> CacheKey -> [CacheKey]
cacheDepends gs key = case key of
  CacheCategoryList    -> [key]
  CacheCategory _      -> [key, CacheCategoryList]
  -- If the category's prosConsEnabled/ecosystemEnabled have changed, we'd
  -- have to render *all* items differently (and CacheCategoryInfo includes
  -- prosConsEnabled/ecosystemEnabled). See Note [enabled sections].
  CacheCategoryInfo x  ->
    [key, CacheCategory x, CacheCategoryList] ++
    -- A convoluted way to say “find category with uid x”
    map CacheItem (gs ^.. categories.each.filtered(hasUid x).items.each.uid)
  CacheCategoryNotes x -> [key, CacheCategory x, CacheCategoryList]
  -- If the item's group has been changed, it can influence how other items
  -- in the same category are rendered (specifically, their lists of groups
  -- in iteminfo will change)
  CacheItem x ->
    let cat = findCategoryByItem x gs
    in  CacheCategory (cat^.uid) :
        -- This will cover (CacheItem x) so we don't need to prepend it
        -- manually like we did before with including 'key' into lists
        map CacheItem     (cat^..items.each.uid) ++
        map CacheItemInfo (cat^..items.each.uid)
  -- Be careful – if the presentation of the category list ever changes
  -- (e.g. to include lists of items in categories, or their counts, or
  -- something), we'd have to invalidate CacheCategoryList here too.
  CacheItemInfo x ->
    let cat = findCategoryByItem x gs
    in  CacheCategory (cat^.uid) :
        map CacheItem     (cat^..items.each.uid) ++
        map CacheItemInfo (cat^..items.each.uid)
  CacheItemDescription x ->
    let cat = findCategoryByItem x gs
    in  [key, CacheItem x, CacheCategory (cat^.uid)]
  CacheItemEcosystem x ->
    let cat = findCategoryByItem x gs
    in  [key, CacheItem x, CacheCategory (cat^.uid)]
  CacheItemTraits x ->
    let cat = findCategoryByItem x gs
    in  [key, CacheItem x, CacheCategory (cat^.uid)]
  CacheItemNotes x ->
    let cat = findCategoryByItem x gs
    in  [key, CacheItem x, CacheCategory (cat^.uid)]

{- | Remove a part from the cache, together with all parts that depend on it.

Note that cache invalidation has to happen before the actual action is
performed, because otherwise bad things can happen (e.g. if we're deleting an
item we have to invalidate the cache for its category, but if we've already
deleted the item we can't look up its category in the 'GlobalState').
-}
invalidateCache :: MonadIO m => GlobalState -> CacheKey -> m ()
invalidateCache gs key = liftIO $ atomically $ do
  for_ (cacheDepends gs key) $ \k ->
    STMMap.delete k cache

-- | Empty the whole cache.
emptyCache :: MonadIO m => m ()
emptyCache = liftIO $ atomically $ STMMap.deleteAll cache

-- | Either take the rendered item from the cache, or render it and put it
-- into the cache if it's not there.
cached :: MonadIO m => CacheKey -> HtmlT IO () -> HtmlT m ()
cached key gen = do
  -- If the item isn't in the cache, we'll have to render it, so we insert a
  -- unique mark if the item wasn't found, and we do it in the same STM
  -- transaction (so that nobody would be able to interfere)
  uniq <- liftIO newUnique
  mbRes <- liftSTM $ do                 -- liftSTM uses 'atomically'
    r <- STMMap.lookup key cache
    when (isNothing r) $
      STMMap.insert (Left uniq) key cache
    return r
  case mbRes of
    -- If the item was in the cache already, we just return it
    Just (Right res) -> toHtmlRaw res
    -- If someone else is already rendering this, we'll use their result when
    -- it's ready (so we wait a bit and then retry)
    Just (Left _) -> do
      liftIO (threadDelay 1000)
      cached key gen
    -- If the item isn't in the cache, we'll take on rendering it
    Nothing -> do
      -- On exception (i.e. when we have failed to do the render), we delete
      -- our mark (but only if it's really our mark) so that someone else
      -- would be able to try to render it
      bs <- liftIO $ renderBST gen `onException`
                       liftSTM (deleteIfEq key (Left uniq) cache)
      -- If rendering has succeeded, we write the rendered result, but only
      -- if the rendering has been started by us
      liftSTM (replaceIfEq key (Left uniq) (Right bs) cache)
      toHtmlRaw bs

-- | Delete the value, but only if it's equal to the one we expect.
deleteIfEq :: Eq v => STMMap.Key k => k -> v -> STMMap.Map k v -> STM ()
deleteIfEq k v m = STMMap.focus (Focus.updateM upd) k m
  where upd a = return (if a == v then Nothing else Just a)

-- | Replace the value, but only if it's equal to the one we expect.
replaceIfEq :: Eq v => STMMap.Key k => k -> v -> v -> STMMap.Map k v -> STM ()
replaceIfEq k v v' m = STMMap.focus (Focus.adjustM upd) k m
  where upd a = return (if a == v then v' else a)
