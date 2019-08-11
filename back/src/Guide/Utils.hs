{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | All utility functions and types go here.
module Guide.Utils
(
  -- * Lists
  Direction (..),
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

  -- * Referrers
  ReferrerView (..),
  toReferrerView,

  -- * IP
  sockAddrToIP,

  -- * UID
  Uid(..),
  Node,
  randomShortUid,
  randomLongUid,
  uid_,

  -- * JSON
  fromJson,
  fromJsonWith,
  toJson,
  toJsonPretty,

  -- * Lucid
  includeJS,
  includeCSS,

  -- * Spock
  atomFeed,
  getRequestDetails,

  -- * Template Haskell
  dumpSplices,
  fields,
  fieldsPrefixed,
  makeClassWithLenses,

  -- * STM
  liftSTM,

  -- * Instances
  -- ** 'MonadThrow' for 'HtmlT'
)
where


import Imports

-- Monads and monad transformers
import Control.Monad.Catch
-- Randomness
import System.Random
-- Network
import Data.IP
-- Web
import Lucid hiding (for_)
import Text.HTML.SanitizeXSS (sanitaryURI)
import Web.HttpApiData
import Web.Spock as Spock
-- Feeds
-- acid-state
import Data.SafeCopy
-- Template Haskell
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
-- needed for parsing urls
import Network.HTTP.Types (Query, parseQuery)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Internal as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.XML.Types as XML
import qualified Network.Socket as Network
import qualified Network.Wai as Wai
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Atom
import qualified Text.XML as XMLC

-- needed for 'sanitiseUrl'
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Network.URI as URI

----------------------------------------------------------------------------
-- Lists
----------------------------------------------------------------------------

-- | Datatype is to point the direction for either items or traits
data Direction = MoveUp | MoveDown
  deriving Eq

-- | Move the -1st element that satisfies the predicate- up.
moveUp :: (a -> Bool) -> [a] -> [a]
moveUp p (x:y:xs) = if p y then y : x : xs else x : moveUp p (y:xs)
moveUp _ xs       = xs

-- | Move the -1st element that satisfies the predicate- down.
moveDown :: (a -> Bool) -> [a] -> [a]
moveDown p (x:y:xs) = if p x then y : x : xs else x : moveDown p (y:xs)
moveDown _ xs       = xs

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
      Just $ case URI.uriScheme <$> parse (toString u) of
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

-- | Add a path element to an URL:
--
-- >>> "https://guide.aelve.com" // "haskell"
-- "https://guide.aelve.com/haskell"
--
-- If slashes are already present, it strips them:
--
-- >>> "https://guide.aelve.com/" // "/haskell"
-- "https://guide.aelve.com/haskell"
--
-- Note that ('</>') from "System.FilePath" shouldn't be used, as on Windows
-- it appends backslashes (@\@) and not slashes (@/@).
(//) :: Url -> Text -> Url
(//) x y = fromMaybe x (T.stripSuffix "/" x) <> "/" <>
           fromMaybe y (T.stripPrefix "/" y)

----------------------------------------------------------------------------
-- ReferrerView
----------------------------------------------------------------------------

data SearchEngine
  = Google
  | Yandex
  | Yahoo
  | Bing
  | Ecosia
  | DuckDuckGo
  deriving (Show, Eq, Ord)

-- | Check whether a domain is one of known search engines.
--
-- TODO: this gives some false positives, e.g. @google.wordpress.com@ or
-- @blog.google@ will be erroneously detected as search engines.
toSearchEngine
  :: Text                 -- ^ Domain
  -> Maybe SearchEngine
toSearchEngine t
  | "google" `elem` lst = Just Google
  | "yandex" `elem` lst = Just Yandex
  | "yahoo"  `elem` lst = Just Yahoo
  | "bing"   `elem` lst = Just Bing
  | "ecosia" `elem` lst = Just Ecosia
  | "duckduckgo" `elem` lst = Just DuckDuckGo
  | otherwise = Nothing
  where lst = T.splitOn "." t

-- | A (lossy) representation of referrers that is better for analytics.
data ReferrerView
  = RefSearchEngine { searchEngine :: SearchEngine
                    , keyword      :: Text }  -- No keyword = empty keyword
  | RefUrl Url
  deriving (Eq, Ord)

instance Show ReferrerView where
  show (RefSearchEngine searchEngine keyword)
    = show searchEngine <> showKeyword keyword
  show (RefUrl url) = toString url

showKeyword :: Text -> String
showKeyword "" = ""
showKeyword kw = " (\"" <> toString kw <> "\")"

extractQuery :: Url -> Maybe Query
extractQuery url = getQuery <$> parse url
  where
    getQuery = parseQuery . toByteString . URI.uriQuery
    parse = URI.parseURI . toString

-- TODO: different search engines have different parameters, we should use
-- right ones instead of just trying “whatever fits”
extractKeyword :: Url -> Maybe Text
extractKeyword url
  = case extractQuery url of
      Just query -> toText <$> lookupQuery query
      Nothing    -> Nothing
  where
    lookupQuery :: [(ByteString, Maybe ByteString)] -> Maybe ByteString
    lookupQuery query = join $
      lookup "q"    query <|>     -- Google, Bing, Ecosia, DDG
      lookup "p"    query <|>     -- Yahoo
      lookup "text" query         -- Yandex

toReferrerView :: Url -> ReferrerView
toReferrerView url
  = case toSearchEngine =<< domain of
      Just se -> RefSearchEngine se (fromMaybe "" keyword)
      Nothing -> RefUrl url
  where
    uri = URI.parseURI $ toString url
    uriAuth = URI.uriAuthority =<< uri
    domain = toText . URI.uriRegName <$> uriAuth
    keyword = extractKeyword url

----------------------------------------------------------------------------
-- IP
----------------------------------------------------------------------------

deriveSafeCopySimple 0 'base ''IPv4
deriveSafeCopySimple 0 'base ''IPv6
deriveSafeCopySimple 0 'base ''IP

sockAddrToIP :: Network.SockAddr -> Maybe IP
sockAddrToIP (Network.SockAddrInet  _   x)   = Just (IPv4 (fromHostAddress x))
sockAddrToIP (Network.SockAddrInet6 _ _ x _) = Just (IPv6 (fromHostAddress6 x))
sockAddrToIP _                               = Nothing

----------------------------------------------------------------------------
-- Uid
----------------------------------------------------------------------------

-- | Unique id, used for many things – categories, items, and anchor ids.
newtype Uid a = Uid {uidToText :: Text}
  deriving (Generic, Eq, Ord, Data,
            ToHttpApiData, FromHttpApiData,
            Buildable, Hashable)

instance Show (Uid a) where
  show (Uid a) = show a

instance Aeson.ToJSON (Uid a) where
  toJSON = Aeson.toJSON . uidToText

instance Aeson.FromJSON (Uid a) where
  parseJSON a = Uid <$> Aeson.parseJSON a

-- This instance is written manually because otherwise it produces a warning:
--     • Redundant constraint: SafeCopy a
--     • In the instance declaration for ‘SafeCopy (Uid a)’
instance SafeCopy (Uid a) where
  putCopy = contain . safePut . uidToText
  getCopy = contain (Uid <$> safeGet)
  version = 2
  kind = base

instance IsString (Uid a) where
  fromString = Uid . toText

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
  return (toText (x:xs))

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
-- JSON
----------------------------------------------------------------------------

class AsJson s where
  -- | Parse JSON using the default JSON instance.
  fromJson :: Aeson.FromJSON a => s -> Either String a
  fromJson = fromJsonWith Aeson.parseJSON

  -- | Parse JSON using a custom parser.
  fromJsonWith :: (Aeson.Value -> Aeson.Parser a) -> s -> Either String a
  fromJsonWith p s = do
    v <- fromJson s
    case Aeson.iparse p v of
      Aeson.IError path err -> Left (Aeson.formatError path err)
      Aeson.ISuccess res    -> Right res

  -- | Convert a value to JSON.
  toJson :: Aeson.ToJSON a => a -> s

  -- | Convert a value to pretty-printed JSON.
  toJsonPretty :: Aeson.ToJSON a => a -> s

instance AsJson ByteString where
  fromJson = Aeson.eitherDecodeStrict
  toJson = toByteString . Aeson.encode
  toJsonPretty = toByteString . Aeson.encodePretty

instance AsJson LByteString where
  fromJson = Aeson.eitherDecode
  toJson = Aeson.encode
  toJsonPretty = Aeson.encodePretty

instance AsJson Text where
  fromJson = Aeson.eitherDecode . toLByteString
  toJson = toText . Aeson.encodeToLazyText
  toJsonPretty = toText . Aeson.encodePrettyToTextBuilder

instance AsJson LText where
  fromJson = Aeson.eitherDecode . toLByteString
  toJson = Aeson.encodeToLazyText
  toJsonPretty = toLText . Aeson.encodePrettyToTextBuilder

instance AsJson Aeson.Value where
  fromJsonWith p v = case Aeson.iparse p v of
    Aeson.IError path err -> Left (Aeson.formatError path err)
    Aeson.ISuccess res    -> Right res
  toJson = Aeson.toJSON
  toJsonPretty = Aeson.toJSON

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
  lazyBytes $ either (const "") (XMLC.renderLBS XMLC.def) $ XMLC.fromXMLDocument $
    XML.Document (XML.Prologue [] Nothing []) (Atom.xmlFeed feed) []

-- | Get details of the request:
--
-- @(time, IP, referrer, user-agent)@
getRequestDetails
  :: (MonadIO m, HasSpock (ActionCtxT ctx m))
  => ActionCtxT ctx m (UTCTime, Maybe IP, Maybe Text, Maybe Text)
getRequestDetails = do
  time <- liftIO getCurrentTime
  mbForwardedFor <- liftA2 (<|>) (Spock.header "Forwarded-For")
                                 (Spock.header "X-Forwarded-For")
  mbIP <- case mbForwardedFor of
    Nothing -> sockAddrToIP . Wai.remoteHost <$> Spock.request
    Just ff -> case readMaybe (toString ip) of
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

-- | Put all fields of a record constructor into scope.
--
-- @f $(fields 'Foo) = ...@ is equivalent to @f Foo{..}@, but the compiler
-- will warn on all unused fields. Thus 'fields' brings safety whenever you
-- want to guarantee that a certain function uses all fields of @Foo@.
--
-- To explicitly ignore a field, match it against @_@:
--
-- @
-- f $(fields 'Foo) = ...
--   where
--     -- Ignored fields
--     _ = (fooUselessField1, fooUselessField2)
-- @
--
-- Usage examples include @ToJSON@ instances and various encoders in
-- general:
--
-- @
-- instance ToJSON Foo where
--   toJSON $(fields 'Foo) = ...
-- @
fields :: Name -> PatQ
fields recordConstructor = do
  cons <- reifyConstructor recordConstructor
  case constructorVariant cons of
    RecordConstructor recordFields ->
      conP recordConstructor (map (varP . mkName . nameBase) recordFields)
    _ -> fail $
      "Expected " ++ show recordConstructor ++ " to be a record constructor"

-- | Like 'fields', but prefixes all fields with the given prefix.
--
-- Useful if you need to put fields from more than one record into scope.
fieldsPrefixed :: String -> Name -> PatQ
fieldsPrefixed prefix recordConstructor = do
  cons <- reifyConstructor recordConstructor
  case constructorVariant cons of
    RecordConstructor recordFields ->
      conP recordConstructor (map (varP . mkName . (prefix <>) . nameBase) recordFields)
    _ -> fail $
      "Expected " ++ show recordConstructor ++ " to be a record constructor"

-- | Make a class with lenses for a record.
--
-- This works almost like 'Control.Lens.makeClassy_', but names the class
-- and the "main" lens differently. It's convenient when you want to export
-- all lenses for a type.
--
-- For example, assume the following data type:
--
-- @
-- data User = User
--   { name :: Text
--   , age :: Int }
--
-- makeClassWithLenses ''User
-- @
--
-- This will generate a class called @UserLenses@ containing lenses for all
-- fields. Conveniently, you will be able to export those lenses by
-- exporting just @UserLenses (..)@.
--
-- For reference, the generated class will look as follows:
--
-- @
-- class UserLenses c where
--   \_User :: Lens' c User
--
--   \_name :: Lens' c Text
--   \_name = \_User . \_name
--
--   \_age :: Lens' c Int
--   \_age = \_User . \_age
--
-- instance UserLenses User where
--   \_User = id
--   \_name f (User name age) = (\name' -> User name' age) \<$\> f name
--   \_age f (User name age) = (\age' -> User name age') \<$\> f age
-- @
makeClassWithLenses :: Name -> DecsQ
makeClassWithLenses = makeLensesWith
  (classyRules
     & lensField .~ (\_ _ n -> [TopName (mkName ('_':nameBase n))])
     & lensClass .~ (\n -> Just ( mkName (nameBase n ++ "Lenses")
                                , mkName ('_':nameBase n))))

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
