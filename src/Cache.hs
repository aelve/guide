{-# LANGUAGE
DeriveGeneric,
NoImplicitPrelude
  #-}


module Cache
(
  CacheKey(..),
  invalidateCache,
  emptyCache,
  cached,
)
where


-- General
import BasePrelude hiding (Category)
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Monads and monad transformers
import Control.Monad.IO.Class
-- ByteString
import qualified Data.ByteString.Lazy as BSL
-- Concurrent map
import qualified STMContainers.Map as STMMap
import qualified Focus
import Data.Hashable
-- Lucid
import Lucid.Base

-- local
import Types
import Utils


-- Left = someone started rendering but haven't finished yet
-- Right = result of the render
type Cache = STMMap.Map CacheKey (Either Unique BSL.ByteString)

cache :: Cache
{-# NOINLINE cache #-}
cache = unsafePerformIO STMMap.newIO

data CacheKey
  = CacheCategoryList
  -- categories
  | CacheCategory      (Uid Category)
  | CacheCategoryInfo  (Uid Category)
  | CacheCategoryNotes (Uid Category)
  -- items
  | CacheItem            (Uid Item)
  | CacheItemInfo        (Uid Item)
  | CacheItemDescription (Uid Item)
  | CacheItemEcosystem   (Uid Item)
  | CacheItemTraits      (Uid Item)
  | CacheItemNotes       (Uid Item)
  deriving (Show, Eq, Generic)

instance Hashable CacheKey

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

-- Note that cache invalidation has to happen before the actual action is
-- performed, because otherwise bad things can happen (e.g. if we're deleting
-- an item we have to invalidate the cache for its category, but if we've
-- already deleted the item we can't look up its category).
invalidateCache :: MonadIO m => GlobalState -> CacheKey -> m ()
invalidateCache gs key = liftIO $ atomically $ do
  for_ (cacheDepends gs key) $ \k ->
    STMMap.delete k cache

emptyCache :: MonadIO m => m ()
emptyCache = liftIO $ atomically $ STMMap.deleteAll cache

cached :: MonadIO m => CacheKey -> HtmlT IO () -> HtmlT m ()
cached key gen = do
  -- If the item isn't in the cache, we'll have to render it, so we insert a
  -- unique mark if the item wasn't found, and we do it in the same STM
  -- transaction (so that nobody would be able to interfere)
  uniq <- liftIO newUnique
  mbRes <- liftSTM $ do
    r <- STMMap.lookup key cache
    when (isNothing r) $
      STMMap.insert (Left uniq) key cache
    return r
  -- Okay, so. What's the result?
  case mbRes of
    -- The item was in the cache, so we just return it
    Just (Right res) -> toHtmlRaw res
    -- Someone else is already rendering this; we'll use their result when
    -- it's ready
    Just (Left _) -> do
      liftIO (threadDelay 1000)
      cached key gen
    -- The item isn't in the cache, so we'll take on rendering it
    Nothing -> do
      -- If rendering doesn't succeed, we delete the mark so that someone
      -- else would be able to render it
      bs <- liftIO $ renderBST gen `onException`
                       liftSTM (deleteIfEq key (Left uniq) cache)
      -- If rendering has succeeded, we write the rendered result
      liftSTM (replaceIfEq key (Left uniq) (Right bs) cache)
      toHtmlRaw bs

-- The *IfEq functions are used for extra safety (we'll never replace a newer
-- render with an older render accidentally)

deleteIfEq :: Eq v => STMMap.Key k => k -> v -> STMMap.Map k v -> STM ()
deleteIfEq k v m = STMMap.focus (Focus.updateM upd) k m
  where upd a = return (if a == v then Nothing else Just a)

replaceIfEq :: Eq v => STMMap.Key k => k -> v -> v -> STMMap.Map k v -> STM ()
replaceIfEq k v v' m = STMMap.focus (Focus.adjustM upd) k m
  where upd a = return (if a == v then v' else a)

liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically
