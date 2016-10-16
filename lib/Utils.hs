{-# LANGUAGE
ScopedTypeVariables,
QuasiQuotes,
OverloadedStrings,
GeneralizedNewtypeDeriving,
FlexibleContexts,
FlexibleInstances,
TypeFamilies,
NoImplicitPrelude
  #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


module Utils
(
  -- * Lists
  moveUp,
  moveDown,
  deleteFirst,
  insertAtGuaranteed,
  ordNub,

  -- * 'Eq'
  equating,

  -- * URLs
  Url,
  sanitiseUrl,
  makeSlug,

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

  -- * Template Haskell
  hs,
  dumpSplices,

  -- * Safecopy
  Change(..),
  changelog,
  GenConstructor(..),
  genVer,
  MigrateConstructor(..),
  migrateVer,

  -- * Instances
  -- ** 'MonadThrow' for 'HtmlT'
)
where


import BasePrelude
-- Lists
import Data.List.Extra (stripSuffix)
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Monads and monad transformers
import Control.Monad.Trans
import Control.Monad.Catch
-- Containers
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map (Map)
-- Hashable (needed for Uid)
import Data.Hashable
-- Randomness
import System.Random
-- Text
import Data.Text.All (Text)
import qualified Data.Text.All as T
-- JSON
import qualified Data.Aeson as A
-- Network
import qualified Network.Socket as Network
import Data.IP
-- Web
import Lucid hiding (for_)
import Web.Spock
import Text.HTML.SanitizeXSS (sanitaryURI)
import Web.PathPieces
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


-- | Move the -1st element that satisfies the predicate- up.
moveUp :: (a -> Bool) -> [a] -> [a]
moveUp p (x:y:xs) = if p y then (y:x:xs) else x : moveUp p (y:xs)
moveUp _ xs = xs

-- | Move the -1st element that satisfies the predicate- down.
moveDown :: (a -> Bool) -> [a] -> [a]
moveDown p (x:y:xs) = if p x then (y:x:xs) else x : moveDown p (y:xs)
moveDown _ xs = xs

deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst _   []   = []
deleteFirst f (x:xs) = if f x then xs else x : deleteFirst f xs

insertAtGuaranteed :: Int -> a -> [a] -> [a]
insertAtGuaranteed _ a   []   = [a]
insertAtGuaranteed 0 a   xs   = a:xs
insertAtGuaranteed n a (x:xs) = x : insertAtGuaranteed (n-1) a xs

ordNub :: Ord a => [a] -> [a]
ordNub = go mempty
  where
    go _ [] = []
    go s (x:xs) | x `S.member` s = go s xs
                | otherwise      = x : go (S.insert x s) xs

equating :: Eq b => (a -> b) -> (a -> a -> Bool)
equating f = (==) `on` f

type Url = Text

sanitiseUrl :: Url -> Maybe Url
sanitiseUrl u
  | not (sanitaryURI u)       = Nothing
  | "http:" `T.isPrefixOf` u  = Just u
  | "https:" `T.isPrefixOf` u = Just u
  | otherwise                 = Just ("http://" <> u)

-- | Make text suitable for inclusion into an URL (by turning spaces into
-- hyphens and so on)
makeSlug :: Text -> Text
makeSlug =
  T.intercalate "-" . T.words .
  T.filter (\c -> isLetter c || isDigit c || c == ' ' || c == '-') .
  T.toLower .
  T.map (\x -> if x == '_' || x == '/' then '-' else x)

deriveSafeCopySimple 0 'base ''IPv4
deriveSafeCopySimple 0 'base ''IPv6
deriveSafeCopySimple 0 'base ''IP

sockAddrToIP :: Network.SockAddr -> Maybe IP
sockAddrToIP (Network.SockAddrInet  _   x)   = Just (IPv4 (fromHostAddress x))
sockAddrToIP (Network.SockAddrInet6 _ _ x _) = Just (IPv6 (fromHostAddress6 x))
sockAddrToIP _ = Nothing

-- | Unique id, used for many things – categories, items, and anchor ids.
newtype Uid a = Uid {uidToText :: Text}
  deriving (Eq, Ord, Show, PathPiece, T.Buildable, Hashable, A.ToJSON)

-- See Note [acid-state]
deriveSafeCopySimple 2 'extension ''Uid

newtype Uid_v1 a = Uid_v1 {uidToText_v1 :: Text}

-- TODO: at the next migration change this to deriveSafeCopySimple!
deriveSafeCopy 1 'base ''Uid_v1

instance SafeCopy a => Migrate (Uid a) where
  type MigrateFrom (Uid a) = Uid_v1 a
  migrate Uid_v1{..} = Uid {
    uidToText = uidToText_v1 }

instance IsString (Uid a) where
  fromString = Uid . T.pack

randomText :: MonadIO m => Int -> m Text
randomText n = liftIO $ do
  -- We don't want the 1st char to be a digit. Just in case (I don't really
  -- have a good reason). Maybe to prevent Javascript from doing automatic
  -- conversions or something (tho it should never happen).
  x <- randomRIO ('a', 'z')
  let randomChar = do
        i <- randomRIO (0, 35)
        return $ if i < 10 then toEnum (fromEnum '0' + i)
                           else toEnum (fromEnum 'a' + i - 10)
  xs <- replicateM (n-1) randomChar
  return (T.pack (x:xs))

randomLongUid :: MonadIO m => m (Uid a)
randomLongUid = Uid <$> randomText 12

-- These are only used for items and categories (because their uids can occur
-- in links and so they should look a bit nicer).
randomShortUid :: MonadIO m => m (Uid a)
randomShortUid = Uid <$> randomText 8

-- | A marker for Uids that would be used with HTML nodes
data Node

uid_ :: Uid Node -> Attribute
uid_ = id_ . uidToText

includeJS :: Monad m => Url -> HtmlT m ()
includeJS url = with (script_ "") [src_ url]

includeCSS :: Monad m => Url -> HtmlT m ()
includeCSS url = link_ [rel_ "stylesheet", type_ "text/css", href_ url]

atomFeed :: MonadIO m => Atom.Feed -> ActionCtxT ctx m ()
atomFeed feed = do
  setHeader "Content-Type" "application/atom+xml; charset=utf-8"
  bytes $ T.encodeUtf8 (T.pack (XML.ppElement (Atom.xmlFeed feed)))

hs :: QuasiQuoter
hs = QuasiQuoter {
  quoteExp  = either fail TH.lift . parseExp,
  quotePat  = fail "hs: can't parse patterns",
  quoteType = fail "hs: can't parse types",
  quoteDec  = fail "hs: can't parse declarations" }

dumpSplices :: DecsQ -> DecsQ
dumpSplices x = do
  ds <- x
  -- “reportWarning (pprint ds)” doesn't work in Emacs because of
  -- haskell-mode's parsing of compiler messages
  mapM_ (reportWarning . pprint) ds
  return ds

{- |
A change from one version of a record (one constructor, several fields) to another version. We only record the latest version, so we have to be able to reconstruct the previous version knowing the current version and a list of 'Change's.
-}
data Change
  -- | A field with a particular name and type was removed
  = Removed String (Q Type)
  -- | A field with a particular name and default value was added. We don't
  -- have to record the type since it's already known (remember, we know what
  -- the final version of the record is)
  | Added String Exp

{- |
Generate previous version of the type.

Assume that the new type and the changelog are, respectively:

    -- version 4
    data Foo = FooRec {
      b :: Bool,
      c :: Int }

    changelog ''Foo 4 [
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

Note that you must use 'deriveSafeCopySorted' for types that use 'changelog' because otherwise fields will be parsed in the wrong order. Specifically, imagine that you have created a type with fields “b” and “a” and then removed “b”. 'changelog' has no way of knowing from “the current version has field “a”” and “the previous version also had field “b”” that the previous version had fields “b, a” and not “a, b”. Usual 'deriveSafeCopy' or 'deriveSafeCopySimple' care about field order and thus will treat “b, a” and “a, b” as different types.
-}
changelog
  :: Name           -- ^ Type (without version suffix)
  -> Int            -- ^ New version
  -> [Change]       -- ^ List of changes between this version and previous one
  -> DecsQ
changelog tyName newVer changes = do
  -- Get information about the new version of the datatype
  TyConI (DataD _cxt _name _vars cons _deriving) <- reify tyName
  -- Do some checks first – we only have to handle simple types for now, but
  -- if/when we need to handle more complex ones, we want to be warned
  unless (null _cxt) $
    fail "changelog: can't yet work with types with context"
  unless (null _vars) $
    fail "changelog: can't yet work with types with variables"
  con <- case cons of
    [x] -> return x
    []  -> fail "changelog: the type has to have at least one constructor"
    _   -> fail "changelog: the type has to have only one constructor"
  -- Check that the type is a record and that there are no strict fields
  -- (which we cannot handle yet); when done, make a list of fields that is
  -- easier to work with
  (recName :: Name, fields :: [(Name, Type)]) <- case con of
    RecC cn fs
      | all (== NotStrict) (fs^..each._2) ->
          return (cn, [(n, t) | (n,_,t) <- fs])
      | otherwise -> fail "changelog: can't work with strict/unpacked fields"
    _             -> fail "changelog: the type must be a record"
  -- This will only be needed on newer GHC:
  --     let normalBang = Bang NoSourceUnpackedness NoSourceStrictness
  -- Check that all 'Added' fields are actually present in the new type
  for_ (M.keys added) $ \n -> do
    unless (n `elem` map (nameBase . fst) fields) $ fail $
      printf "changelog: field %s isn't present in %s" (show n) (show tyName)

  -- Now we can generate the old type based on the new type and the
  -- changelog. First we determine the list of fields (and types) we'll have
  -- by taking 'fields' from the new type, adding 'Removed' fields and
  -- removing 'Added' fields:
  let oldFields :: Map Name (Q Type)
      oldFields = let f (Added x _) = M.delete x
                      f (Removed n t) = M.insert n t
                      m = M.fromList [(nameBase n, return t)
                                       | (n,t) <- fields]
                  in M.mapKeys (old . mkName) $ foldr f m changes
  -- Then we construct the record constructor:
  --   FooRec_v3 { a_v3 :: String, b_v3 :: Bool }
  let oldRec = recC (old recName)
                    [varStrictType fName
                                   (strictType notStrict fType)
                    | (fName, fType) <- M.toList oldFields]
  -- And the data type:
  --   data Foo_v3 = FooRec_v3 {...}
  let oldTypeDecl = dataD (cxt [])       -- no context
                          (old tyName)   -- name of old type
                          []             -- no variables
                          [oldRec]       -- one constructor
                          []             -- not deriving anything

  -- Next we generate the migration instance. It has two inner declarations.
  -- First declaration – “type MigrateFrom Foo = Foo_v3”:
  let migrateFromDecl =
        tySynInstD ''MigrateFrom
                   (tySynEqn [conT tyName] (conT (old tyName)))
  -- Second declaration:
  --   migrate old = FooRec {
  --     b = b_v3 old,
  --     c = if null (a_v3 old) then 0 else 1 }
  migrateArg <- newName "old"
  -- This function replaces accessors in an expression – “a” turns into
  -- “(a_vN old)” if 'a' is one of the fields in the old type
  let replaceAccessors = transform f
        where f (VarE x) | old x `elem` M.keys oldFields =
                AppE (VarE (old x)) (VarE migrateArg)
              f x = x
  let migrateDecl = funD 'migrate [
        clause [varP migrateArg]
          (normalB $ recConE recName $ do
             (newField, _) <- fields
             let content = case M.lookup (nameBase newField) added of
                   Nothing -> appE (varE (old newField)) (varE migrateArg)
                   Just e  -> return (replaceAccessors e)
             return $ (newField,) <$> content)
          []
        ]

  let migrateInstanceDecl =
        instanceD
          (cxt [])                        -- no context
          [t|Migrate $(conT tyName)|]     -- Migrate Foo
          [migrateFromDecl, migrateDecl]  -- associated type & migration func

  -- Return everything
  sequence [oldTypeDecl, migrateInstanceDecl]

  where
    oldVer = newVer - 1
    -- Convert a new name to an old name by adding “_vN” to it or replacing
    -- it if the name already has a “_v” suffix
    old :: Name -> Name
    old (nameBase -> n) =
      let suffixless = fromMaybe n $ stripSuffix ("_v" ++ show newVer) n
      in mkName (suffixless ++ "_v" ++ show oldVer)
    -- List of 'Added' fields
    added :: Map String Exp
    added = M.fromList [(n, e) | Added n e <- changes]

data GenConstructor = Copy Name | Custom String [(String, Name)]

genVer :: Name -> Int -> [GenConstructor] -> Q [Dec]
genVer tyName ver constructors = do
  -- Get information about the new version of the datatype
  TyConI (DataD _cxt _name _vars cons _deriving) <- reify tyName
  -- Let's do some checks first
  unless (null _cxt) $
    fail "genVer: can't yet work with types with context"
  unless (null _vars) $
    fail "genVer: can't yet work with types with variables"

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
             [varStrictType (oldName (mkName fName))
                            (strictType notStrict (conT fType))
               | (fName, fType) <- fields]

  cons' <- for constructors $ \genCons -> do
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
    -- constructors
    (map return cons')
    -- not deriving anything
    []
  return [decl]

data MigrateConstructor = CopyM Name | CustomM Name ExpQ

migrateVer :: Name -> Int -> [MigrateConstructor] -> Q Exp
migrateVer tyName ver constructors = do
  -- Get information about the new version of the datatype
  TyConI (DataD _cxt _name _vars cons _deriving) <- reify tyName
  -- Let's do some checks first
  unless (null _cxt) $
    fail "migrateVer: can't yet work with types with context"
  unless (null _vars) $
    fail "migrateVer: can't yet work with types with variables"

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
        match (recP (oldName conName) [])
              (normalB res)
              []

  branches' <- for constructors $ \genCons -> do
    case genCons of
      CopyM conName -> copyConstructor conName
      CustomM conName res -> customConstructor conName res

  lam1E (varP arg) (caseE (varE arg) (map return branches'))

instance MonadThrow m => MonadThrow (HtmlT m) where
  throwM e = lift $ throwM e
