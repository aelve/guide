{-# LANGUAGE
DeriveGeneric,
NoImplicitPrelude
  #-}


module Cache
(
  CacheKey(..),
  invalidateCache,
  cached,
)
where


-- General
import BasePrelude hiding (Category)
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Monads and monad transformers
import Control.Monad.IO.Class
import Control.Monad.Trans
-- ByteString
import qualified Data.ByteString.Lazy as BSL
-- Concurrent map
import qualified STMContainers.Map as STMMap
import Data.Hashable
-- Lucid
import Lucid.Base

-- local
import Types
import Utils


type Cache = STMMap.Map CacheKey BSL.ByteString

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
  CacheCategoryInfo x  -> [key, CacheCategory x, CacheCategoryList]
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

cached :: MonadIO m => CacheKey -> HtmlT m () -> HtmlT m ()
cached key gen = do
  mbRes <- liftIO . atomically $ STMMap.lookup key cache
  case mbRes of
    Just res -> toHtmlRaw res
    Nothing  -> do
      bs <- lift $ renderBST gen
      -- TODO: this bad situation is *maybe* possible:
      --
      --   item is changed in request A
      --   request A invalidates cache
      --   request A starts re-rendering the item
      --   'cached' started by A sees that the cache is empty
      --       item is changed in request B
      --       request B starts re-rendering the item
      --       request B fills the cache
      --   'cached' started by A finishes rendering
      --   'cached' started by A fills the cache with an outdated render
      --
      -- It's unlikely, but still probably possible. Ideally we'd like to run
      -- lookup *and* insert inside the same transaction, but I don't know
      -- how to do it.
      liftIO . atomically $ STMMap.insert bs key cache
      toHtmlRaw bs
