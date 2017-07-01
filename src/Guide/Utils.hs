{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


{- |
All utility functions and types go here.
-}
module Guide.Utils
(
  -- * Lists
  moveUp,
  moveDown,
  deleteFirst,
  insertOrAppend,
  ordNub,

  -- * 'Eq'
  equating,

  -- * URLs
  Url,
  sanitiseUrl,
  makeSlug,
  (//),

  -- * IP
  sockAddrToIP,

  -- * UID
  Uid(..),
  Node,
  randomShortUid,
  randomLongUid,
  uid_,

  -- * Lucid
  includeJS,
  includeCSS,

  -- * Spock
  atomFeed,
  getRequestDetails,

  -- * Template Haskell
  dumpSplices,

  -- * STM
  liftSTM,

  -- * Instances
  -- ** 'MonadThrow' for 'HtmlT'
)
where


import Imports

-- Monads and monad transformers
import Control.Monad.Catch
-- Containers
import qualified Data.Set as S
-- Randomness
import System.Random
-- Text
import qualified Data.Text.All as T
-- JSON
import qualified Data.Aeson as A
-- Network
import qualified Network.Socket as Network
import Data.IP
-- Web
import Lucid hiding (for_)
import Web.Spock as Spock
import Text.HTML.SanitizeXSS (sanitaryURI)
import Web.HttpApiData
import qualified Network.Wai as Wai
-- Feeds
import qualified Text.Atom.Feed        as Atom
import qualified Text.Atom.Feed.Export as Atom
import qualified Text.XML.Light.Output as XML
-- acid-state
import Data.SafeCopy
-- Template Haskell
import Language.Haskell.TH
-- needed for 'sanitiseUrl'
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Network.URI as URI


----------------------------------------------------------------------------
-- Lists
----------------------------------------------------------------------------

-- | Move the -1st element that satisfies the predicate- up.
moveUp :: (a -> Bool) -> [a] -> [a]
moveUp p (x:y:xs) = if p y then y : x : xs else x : moveUp p (y:xs)
moveUp _ xs = xs

-- | Move the -1st element that satisfies the predicate- down.
moveDown :: (a -> Bool) -> [a] -> [a]
moveDown p (x:y:xs) = if p x then y : x : xs else x : moveDown p (y:xs)
moveDown _ xs = xs

-- | Delete the first element that satisfies the predicate (if such an
-- element is present).
deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst _   []   = []
deleteFirst f (x:xs) = if f x then xs else x : deleteFirst f xs

-- | Insert given element into the list, or append it to the list if the
-- position is outside the list bounds.
insertOrAppend
  :: Int          -- ^ Preferred position
  -> a            -- ^ Element to insert
  -> [a]
  -> [a]
insertOrAppend _ a   []   = [a]
insertOrAppend 0 a   xs   = a:xs
insertOrAppend n a (x:xs) = x : insertOrAppend (n-1) a xs

-- | A version of 'works in @O(n log n)@ instead of @O(n^2)@.
ordNub :: Ord a => [a] -> [a]
ordNub = go mempty
  where
    go _ [] = []
    go s (x:xs) | x `S.member` s = go s xs
                | otherwise      = x : go (S.insert x s) xs

----------------------------------------------------------------------------
-- Eq
----------------------------------------------------------------------------

-- | Like 'comparing', but for 'Eq' for 'Eq' comparison instead of 'Ord'. Can
-- be used with e.g. 'grourison instead of 'Ord'. Can be usedpBy'. with e.g.
-- 'groupBy'.
equating :: Eq b => (a -> b) -> (a -> a -> Bool)
equating f = (==) `on` f

----------------------------------------------------------------------------
-- Urls
----------------------------------------------------------------------------

-- | A type for URLs.
type Url = Text

-- | Return 'Nothing' if the URL is unsafe (e.g. a @javascript:@ or @data:@
-- URL). Otherwise return the original URL, possibly adding @http://@ to it
-- if it doesn't have a scheme.
sanitiseUrl :: Url -> Maybe Url
sanitiseUrl u
  | not (sanitaryURI u) = Nothing
  | otherwise =
      Just $ case URI.uriScheme <$> parse (T.toString u) of
        Nothing -> "http://" <> u
        Just "" -> "http://" <> u
        _       -> u
  where
    -- code taken from implementation of 'sanitaryURI'
    parse  = URI.parseURIReference . escape
    escape = URI.escapeURIString URI.isAllowedInURI .
             UTF8.encodeString

-- | Make text suitable for inclusion into an URL (by turning spaces into
-- hyphens and so on).
makeSlug :: Text -> Text
makeSlug =
  T.intercalate "-" . T.words .
  T.filter (\c -> isLetter c || isDigit c || c == ' ' || c == '-') .
  T.toLower .
  T.map (\x -> if x == '_' || x == '/' then '-' else x)

{- |
Add a path element to an URL:

>>> "https://guide.aelve.com" // "haskell"
"https://guide.aelve.com/haskell"

If slashes are already present, it strips them:

>>> "https://guide.aelve.com/" // "/haskell"
"https://guide.aelve.com/haskell"

Note that ('</>') from "System.FilePath" shouldn't be used, as on Windows it
appends backslashes (@\@) and not slashes (@/@).
-}
(//) :: Url -> Text -> Url
(//) x y = fromMaybe x (T.stripSuffix "/" x) <> "/" <>
           fromMaybe y (T.stripPrefix "/" y)

----------------------------------------------------------------------------
-- IP
----------------------------------------------------------------------------

deriveSafeCopySimple 0 'base ''IPv4
deriveSafeCopySimple 0 'base ''IPv6
deriveSafeCopySimple 0 'base ''IP

sockAddrToIP :: Network.SockAddr -> Maybe IP
sockAddrToIP (Network.SockAddrInet  _   x)   = Just (IPv4 (fromHostAddress x))
sockAddrToIP (Network.SockAddrInet6 _ _ x _) = Just (IPv6 (fromHostAddress6 x))
sockAddrToIP _ = Nothing

----------------------------------------------------------------------------
-- Uid
----------------------------------------------------------------------------

-- | Unique id, used for many things – categories, items, and anchor ids.
newtype Uid a = Uid {uidToText :: Text}
  deriving (Eq, Ord, Show,
            ToHttpApiData, FromHttpApiData,
            T.Buildable, Hashable, A.ToJSON)

-- This instance is written manually because otherwise it produces a warning:
--     • Redundant constraint: SafeCopy a
--     • In the instance declaration for ‘SafeCopy (Uid a)’
instance SafeCopy (Uid a) where
  putCopy = contain . safePut . uidToText
  getCopy = contain (Uid <$> safeGet)
  version = 2
  kind = base

instance IsString (Uid a) where
  fromString = Uid . T.pack

-- | Generate a random text of given length from characters @a-z@ and digits.
randomText :: MonadIO m => Int -> m Text
randomText n = liftIO $ do
  -- We don't want the 1st char to be a digit. Just in case (I don't really
  -- have a good reason). Maybe to prevent Javascript from doing automatic
  -- conversions or something (though it should never happen).
  x <- randomRIO ('a', 'z')
  let randomChar = do
        i <- randomRIO (0, 35)
        return $ if i < 10 then toEnum (fromEnum '0' + i)
                           else toEnum (fromEnum 'a' + i - 10)
  xs <- replicateM (n-1) randomChar
  return (T.pack (x:xs))

-- For probability tables, see
-- https://en.wikipedia.org/wiki/Birthday_problem#Probability_table

-- | Generate a random UID of length 12.
--
-- Probability of collision for
--
--   * a million UIDs: approximately 1e-6
--   * a billion UIDs: approximately 0.25
--
randomLongUid :: MonadIO m => m (Uid a)
randomLongUid = Uid <$> randomText 12

-- | Generate a random UID of length 8.
--
-- These UIDs are only used for items and categories (because their uids can
-- occur in links and so they should look a bit nicer).
--
-- Probability of collision for
--
--   * a hundred thousand UIDs: approximately 0.5%
--   * a million UIDs: approximately 40%
--
randomShortUid :: MonadIO m => m (Uid a)
randomShortUid = Uid <$> randomText 8

-- | A marker for Uids that would be used with HTML nodes
data Node

-- | Generate a HTML @id@ attribute from an 'Uid'.
uid_ :: Uid Node -> Attribute
uid_ = id_ . uidToText

----------------------------------------------------------------------------
-- Lucid
----------------------------------------------------------------------------

-- | Include Javascript into page by creating a @<script>@ tag.
includeJS :: Monad m => Url -> HtmlT m ()
includeJS url = with (script_ "") [src_ url]

-- | Include CSS into page.
includeCSS :: Monad m => Url -> HtmlT m ()
includeCSS url = link_ [rel_ "stylesheet", type_ "text/css", href_ url]

----------------------------------------------------------------------------
-- Spock
----------------------------------------------------------------------------

-- | Serve an Atom feed.
atomFeed :: MonadIO m => Atom.Feed -> ActionCtxT ctx m ()
atomFeed feed = do
  setHeader "Content-Type" "application/atom+xml; charset=utf-8"
  bytes $ T.encodeUtf8 (T.pack (XML.ppElement (Atom.xmlFeed feed)))

-- | Get details of the request:
--
-- @(time, IP, referrer, user-agent)@
getRequestDetails
  :: (MonadIO m, HasSpock (ActionCtxT ctx m))
  => ActionCtxT ctx m (UTCTime, Maybe IP, Maybe Text, Maybe Text)
getRequestDetails = do
  time <- liftIO $ getCurrentTime
  mbForwardedFor <- liftA2 (<|>) (Spock.header "Forwarded-For")
                                 (Spock.header "X-Forwarded-For")
  mbIP <- case mbForwardedFor of
    Nothing -> sockAddrToIP . Wai.remoteHost <$> Spock.request
    Just ff -> case readMaybe (T.unpack ip) of
      Nothing -> error ("couldn't read Forwarded-For address: " ++
                        show ip ++ " (full header: " ++
                        show ff ++ ")")
      Just i  -> return (Just i)
      where
        addr = T.strip . snd . T.breakOnEnd "," $ ff
        ip -- [IPv6]:port
           | T.take 1 addr == "[" =
               T.drop 1 (T.takeWhile (/= ']') addr)
           -- IPv4 or IPv4:port
           | T.any (== '.') addr =
               T.takeWhile (/= ':') addr
           -- IPv6 without port
           | otherwise =
               addr
  mbReferrer <- Spock.header "Referer"
  mbUA       <- Spock.header "User-Agent"
  return (time, mbIP, mbReferrer, mbUA)

----------------------------------------------------------------------------
-- Template Haskell
----------------------------------------------------------------------------

-- | Print splices generated by a TH splice (the printing will happen during
-- compilation, as a GHC warning). Useful for debugging.
--
-- For instance, you can dump splices generated with 'makeLenses' by
-- replacing a top-level invocation of 'makeLenses' in your code with:
--
-- @dumpSplices $ makeLenses ''Foo@
--
dumpSplices :: DecsQ -> DecsQ
dumpSplices x = do
  ds <- x
  let code = lines (pprint ds)
  reportWarning ("\n" ++ unlines (map ("    " ++) code))
  return ds

----------------------------------------------------------------------------
-- STM
----------------------------------------------------------------------------

-- | Lift an 'STM' action to any IO-supporting monad.
liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

instance MonadThrow m => MonadThrow (HtmlT m) where
  throwM e = lift $ throwM e
