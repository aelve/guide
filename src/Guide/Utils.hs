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

  -- * Lucid
  includeJS,
  includeCSS,

  -- * Spock
  atomFeed,
  getRequestDetails,

  -- * Template Haskell
  hs,
  dumpSplices,
  bangNotStrict,

  -- * Safecopy
  Change(..),
  TypeVersion(..),
  changelog,
  GenConstructor(..),
  genVer,
  MigrateConstructor(..),
  migrateVer,

  -- * STM
  liftSTM,

  -- * Instances
  -- ** 'MonadThrow' for 'HtmlT'
)
where


import Imports

-- Lists
import Data.List.Extra (stripSuffix)
-- Monads
import Control.Monad.Extra
-- Monads and monad transformers
import Control.Monad.Catch
-- Containers
import qualified Data.Set as S
import qualified Data.Map as M
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
import qualified Language.Haskell.TH.Syntax as TH (lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.Meta (parseExp)
import Data.Generics.Uniplate.Data (transform)
-- needed for 'sanitiseUrl'
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Network.URI as URI
-- needed for parsing urls
import Network.HTTP.Types (Query, parseQuery)

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
  = RefSearchEngine {searchEngine :: SearchEngine, keyword :: Maybe Text}
  | RefUrl Url
  deriving (Eq, Ord)

instance Show ReferrerView where
  show (RefSearchEngine searchEngine keyword)
    = show searchEngine <> showKeyword keyword
  show (RefUrl url) = T.toString url

showKeyword :: Maybe Text -> String
showKeyword (Just "") = ""
showKeyword (Just keyword) = " (\"" <> T.toString keyword <> "\")"
showKeyword _ = ""

extractQuery :: Url -> Maybe Query
extractQuery url = getQuery <$> parse url
  where
    getQuery = parseQuery . T.toByteString . URI.uriQuery
    parse = URI.parseURI . T.toString

-- TODO: different search engines have different parameters, we should use
-- right ones instead of just trying “whatever fits”
extractKeyword :: Url -> Maybe Text
extractKeyword url
  = case extractQuery url of
      Just query -> T.toStrict <$> lookupQuery query
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
      Just se -> RefSearchEngine se keyword
      Nothing -> RefUrl url
  where
    uri = URI.parseURI $ T.toString url
    uriAuth = URI.uriAuthority =<< uri
    domain = T.toStrict . URI.uriRegName <$> uriAuth
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
  fromString = Uid . T.toStrict

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
  return (T.toStrict (x:xs))

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
  bytes $ T.toByteString (XML.ppElement (Atom.xmlFeed feed))

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

-- | Parse a Haskell expression with haskell-src-meta. The difference between
-- @[|exp|]@ and @[hs|exp|]@ is the the former requires all variables in
-- @exp@ to be present in scope at the moment of generation, but the latter
-- doesn't. This makes 'hs' useful for 'changelog'.
hs :: QuasiQuoter
hs = QuasiQuoter {
  quoteExp  = either fail TH.lift . parseExp,
  quotePat  = fail "hs: can't parse patterns",
  quoteType = fail "hs: can't parse types",
  quoteDec  = fail "hs: can't parse declarations" }

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

bangNotStrict :: Q Bang
bangNotStrict = bang noSourceUnpackedness noSourceStrictness

----------------------------------------------------------------------------
-- SafeCopy
----------------------------------------------------------------------------

{- |
A change from one version of a record (one constructor, several fields) to
another version. We only record the latest version, so we have to be able to
reconstruct the previous version knowing the current version and a list of
'Change's.
-}
data Change
  -- | A field with a particular name and type was removed
  = Removed String (Q Type)
  -- | A field with a particular name and default value was added. We don't
  -- have to record the type since it's already known (remember, we know what
  -- the final version of the record is)
  | Added String Exp

-- | An ADT for versions. Only used in invocations of 'changelog'.
data TypeVersion = Current Int | Past Int
  deriving (Show)

{- |
Generate previous version of the type.

Assume that the new type and the changelog are, respectively:

    -- version 4
    data Foo = FooRec {
      b :: Bool,
      c :: Int }

    changelog ''Foo (Current 4, Past 3) [
      Removed "a" [t|String|],
      Added "c" [|if null a then 0 else 1|] ]

Then we will generate a type called Foo_v3:

    data Foo_v3 = FooRec_v3 {
      a_v3 :: String,
      b_v3 :: Bool }

We'll also generate a migration instance:

    instance Migrate Foo where
      type MigrateFrom Foo = Foo_v3
      migrate old = FooRec {
        b = b_v3 old,
        c = if null (a_v3 old) then 0 else 1 }

Note that you must use 'deriveSafeCopySorted' for types that use 'changelog'
because otherwise fields will be parsed in the wrong order. Specifically,
imagine that you have created a type with fields “b” and “a” and then removed
“b”. 'changelog' has no way of knowing from “the current version has field
“a”” and “the previous version also had field “b”” that the previous version
had fields “b, a” and not “a, b”. Usual 'deriveSafeCopy' or
'deriveSafeCopySimple' care about field order and thus will treat “b, a” and
“a, b” as different types.
-}
changelog
  :: Name                        -- ^ Type (without version suffix)
  -> (TypeVersion, TypeVersion)  -- ^ New version, old version
  -> [Change]                    -- ^ List of changes between this version
                                 --   and previous one
  -> DecsQ
changelog _ (_newVer, Current _) _ =
  -- We could've just changed the second element of the tuple to be 'Int'
  -- instead of 'TypeVersion' but that would lead to worse-looking changelogs
  fail "changelog: old version can't be 'Current'"
changelog bareTyName (newVer, Past oldVer) changes = do
  -- ------------------------------------------------------------------------
  -- Name and version business
  -- ------------------------------------------------------------------------
  -- First, we can define functions for removing a new-version prefix and for
  -- adding a new/old-version prefix to a bare name. We'll be working with
  -- bare names everywhere.
  let mkBare :: Name -> String
      mkBare n = case newVer of
        Current _ -> nameBase n
        Past v ->
          let suff = ("_v" ++ show v)
          in case stripSuffix suff (nameBase n) of
               Just n' -> n'
               Nothing -> error $
                 printf "changelog: %s doesn't have suffix %s"
                        (show n) (show suff)
  let mkOld, mkNew :: String -> Name
      mkOld n = mkName (n ++ "_v" ++ show oldVer)
      mkNew n = case newVer of
        Current _ -> mkName n
        Past v -> mkName (n ++ "_v" ++ show v)
  -- We know the “base” name (tyName) of the type and we know the
  -- versions. From this we can get actual new/old names:
  let newTyName = mkNew (nameBase bareTyName)
  let oldTyName = mkOld (nameBase bareTyName)
  -- We should also check that the new version exists and that the old one
  -- doesn't.
  whenM (isNothing <$> lookupTypeName (nameBase newTyName)) $
    fail (printf "changelog: %s not found" (show newTyName))
  whenM (isJust <$> lookupTypeName (nameBase oldTyName)) $
    fail (printf "changelog: %s is already present" (show oldTyName))

  -- -----------------------------------------------------------------------
  -- Process the changelog
  -- -----------------------------------------------------------------------
  -- Make separate lists of added and removed fields
  let added :: Map String Exp
      added = M.fromList [(n, e) | Added n e <- changes]
  let removed :: Map String (Q Type)
      removed = M.fromList [(n, t) | Removed n t <- changes]

  -- -----------------------------------------------------------------------
  -- Get information about the new version of the datatype
  -- -----------------------------------------------------------------------
  -- First, 'reify' it. See documentation for 'reify' to understand why we
  -- use 'lookupValueName' here (if we just do @reify newTyName@, we might
  -- get the constructor instead).
  TyConI (DataD _cxt _name _vars _kind cons _deriving) <- do
    mbReallyTyName <- lookupTypeName (nameBase newTyName)
    case mbReallyTyName of
      Just reallyTyName -> reify reallyTyName
      Nothing -> fail $ printf "changelog: type %s not found" (show newTyName)
  -- Do some checks first – we only have to handle simple types for now, but
  -- if/when we need to handle more complex ones, we want to be warned.
  unless (null _cxt) $
    fail "changelog: can't yet work with types with context"
  unless (null _vars) $
    fail "changelog: can't yet work with types with variables"
  unless (isNothing _kind) $
    fail "changelog: can't yet work with types with kinds"
  -- We assume that the type is a single-constructor record.
  con <- case cons of
    [x] -> return x
    []  -> fail "changelog: the type has to have at least one constructor"
    _   -> fail "changelog: the type has to have only one constructor"
  -- Check that the type is actually a record and that there are no strict
  -- fields (which we cannot handle yet); when done, make a list of fields
  -- that is easier to work with. We strip names to their bare form.
  let normalBang = Bang NoSourceUnpackedness NoSourceStrictness
  (recName :: String, fields :: [(String, Type)]) <- case con of
    RecC cn fs
      | all (== normalBang) (fs^..each._2) ->
          return (mkBare cn, [(mkBare n, t) | (n,_,t) <- fs])
      | otherwise -> fail "changelog: can't work with strict/unpacked fields"
    _             -> fail "changelog: the type must be a record"
  -- Check that all 'Added' fields are actually present in the new type
  -- and that all 'Removed' fields aren't there
  for_ (M.keys added) $ \n ->
    unless (n `elem` map fst fields) $ fail $
      printf "changelog: field %s isn't present in %s"
             (show (mkNew n)) (show newTyName)
  for_ (M.keys removed) $ \n ->
    when (n `elem` map fst fields) $ fail $
      printf "changelog: field %s is present in %s \
             \but was supposed to be removed"
             (show (mkNew n)) (show newTyName)

  -- -----------------------------------------------------------------------
  -- Generate the old type
  -- -----------------------------------------------------------------------
  -- Now we can generate the old type based on the new type and the
  -- changelog. First we determine the list of fields (and types) we'll have
  -- by taking 'fields' from the new type, adding 'Removed' fields and
  -- removing 'Added' fields. We still use bare names everywhere.
  let oldFields :: Map String (Q Type)
      oldFields = fmap return (M.fromList fields)
                    `M.union` removed
                    `M.difference` added

  -- Then we construct the record constructor:
  --   FooRec_v3 { a_v3 :: String, b_v3 :: Bool }
  let oldRec = recC (mkOld recName)
                    [varBangType (mkOld fName)
                                 (bangType bangNotStrict fType)
                    | (fName, fType) <- M.toList oldFields]
  -- And the data type:
  --   data Foo_v3 = FooRec_v3 {...}
  let oldTypeDecl = dataD (cxt [])      -- no context
                          oldTyName     -- name of old type
                          []            -- no variables
                          Nothing       -- no explicit kind
                          [oldRec]      -- one constructor
                          (cxt [])      -- not deriving anything

  -- Next we generate the migration instance. It has two inner declarations.
  -- First declaration – “type MigrateFrom Foo = Foo_v3”:
  let migrateFromDecl =
        tySynInstD ''MigrateFrom (tySynEqn [conT newTyName] (conT oldTyName))
  -- Second declaration:
  --   migrate old = FooRec {
  --     b = b_v3 old,
  --     c = if null (a_v3 old) then 0 else 1 }
  migrateArg <- newName "old"
  -- This function replaces accessors in an expression – “a” turns into
  -- “(a_vN old)” if 'a' is one of the fields in the old type
  let replaceAccessors = transform f
        where f (VarE x) | nameBase x `elem` M.keys oldFields =
                AppE (VarE (mkOld (nameBase x))) (VarE migrateArg)
              f x = x
  let migrateDecl = funD 'migrate [
        clause [varP migrateArg]
          (normalB $ recConE (mkNew recName) $ do
             (field, _) <- fields
             let content = case M.lookup field added of
                   -- the field was present in old type
                   Nothing -> appE (varE (mkOld field)) (varE migrateArg)
                   -- wasn't
                   Just e  -> return (replaceAccessors e)
             return $ (mkNew field,) <$> content)
          []
        ]

  let migrateInstanceDecl =
        instanceD
          (cxt [])                        -- no context
          [t|Migrate $(conT newTyName)|]  -- Migrate Foo
          [migrateFromDecl, migrateDecl]  -- associated type & migration func

  -- Return everything
  sequence [oldTypeDecl, migrateInstanceDecl]

-- | A type for specifying what constructors existed in an old version of a
-- sum datatype.
data GenConstructor
  = Copy Name                         -- ^ Just reuse the constructor
                                      --   existing now.
  | Custom String [(String, Q Type)]  -- ^ The previous version had a
                                      --   constructor with such-and-such
                                      --   name and such-and-such fields.

-- | Generate an old version of a sum type (used for 'SafeCopy').
genVer
  :: Name                  -- ^ Name of type to generate old version for
  -> Int                   -- ^ Version to generate
  -> [GenConstructor]      -- ^ List of constructors in the version we're
                           --   generating
  -> Q [Dec]
genVer tyName ver constructors = do
  -- Get information about the new version of the datatype
  TyConI (DataD _cxt _name _vars _kind cons _deriving) <- reify tyName
  -- Let's do some checks first
  unless (null _cxt) $
    fail "genVer: can't yet work with types with context"
  unless (null _vars) $
    fail "genVer: can't yet work with types with variables"
  unless (isNothing _kind) $
    fail "genVer: can't yet work with types with kinds"

  let oldName n = mkName (nameBase n ++ "_v" ++ show ver)

  let copyConstructor conName =
        case [c | c@(RecC n _) <- cons, n == conName] of
          [] -> fail ("genVer: couldn't find a record constructor " ++
                      show conName)
          [RecC _ fields] ->
            recC (oldName conName)
                 (map return (fields & each._1 %~ oldName))
          other -> fail ("genVer: copyConstructor: got " ++ show other)

  let customConstructor conName fields =
        recC (oldName (mkName conName))
             [varBangType (oldName (mkName fName))
                          (bangType bangNotStrict fType)
               | (fName, fType) <- fields]

  cons' <- for constructors $ \genCons ->
    case genCons of
      Copy conName -> copyConstructor conName
      Custom conName fields -> customConstructor conName fields

  decl <- dataD
    -- no context
    (cxt [])
    -- name of our type (e.g. SomeType_v3 if the previous version was 3)
    (oldName tyName)
    -- no variables
    []
    -- no explicit kind
    Nothing
    -- constructors
    (map return cons')
    -- not deriving anything
    (cxt [])
  return [decl]

-- | A type for migrating constructors from an old version of a sum datatype.
data MigrateConstructor
  = CopyM Name             -- ^ Copy constructor without changes
  | CustomM String ExpQ    -- ^ The old constructor with such-and-such name
                           --   should be turned into a value of the new type
                           --   (i.e. type of current version) using
                           --   such-and-such code.

-- | Generate 'SafeCopy' migration code for a sum datatype.
--
-- See @instance Migrate Edit@ for an example.
migrateVer
  :: Name                  -- ^ Type we're migrating to
  -> Int                   -- ^ Version we're migrating from
  -> [MigrateConstructor]  -- ^ For each constructor existing in the (old
                           --   version of) type, a specification of how to
                           --   migrate it.
  -> Q Exp
migrateVer tyName ver constructors = do
  -- Get information about the new version of the datatype
  TyConI (DataD _cxt _name _vars _kind cons _deriving) <- reify tyName
  -- Let's do some checks first
  unless (null _cxt) $
    fail "migrateVer: can't yet work with types with context"
  unless (null _vars) $
    fail "migrateVer: can't yet work with types with variables"
  unless (isNothing _kind) $
    fail "migrateVer: can't yet work with types with kinds"

  let oldName n = mkName (nameBase n ++ "_v" ++ show ver)

  arg <- newName "x"

  let copyConstructor conName =
        case [c | c@(RecC n _) <- cons, n == conName] of
          [] -> fail ("migrateVer: couldn't find a record constructor " ++
                      show conName)
          [RecC _ fields] -> do
            -- SomeConstr_v3{} -> SomeConstr (field1 x) (field2 x) ...
            let getField f = varE (oldName (f ^. _1)) `appE` varE arg
            match (recP (oldName conName) [])
                  (normalB (appsE (conE conName : map getField fields)))
                  []
          other -> fail ("migrateVer: copyConstructor: got " ++ show other)

  let customConstructor conName res =
        match (recP (oldName (mkName conName)) [])
              (normalB (res `appE` varE arg))
              []

  branches' <- for constructors $ \genCons ->
    case genCons of
      CopyM conName -> copyConstructor conName
      CustomM conName res -> customConstructor conName res

  lam1E (varP arg) (caseE (varE arg) (map return branches'))

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
