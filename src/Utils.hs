{-# LANGUAGE
OverloadedStrings,
TemplateHaskell,
GeneralizedNewtypeDeriving,
NoImplicitPrelude
  #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


module Utils
(
  -- * Text
  format,
  tshow,

  -- * Lists
  moveUp,
  moveDown,
  deleteFirst,
  deleteAt,

  -- * URLs
  Url,
  sanitiseUrl,
  makeSlug,

  -- * IP
  IP(..),
  sockAddrToIP,
  
  -- * UID
  Uid(..),
  randomShortUid,
  randomLongUid,
  uid_,

  -- * Lucid
  includeJS,
  includeCSS,

  -- * Spock
  atomFeed,
)
where


-- General
import BasePrelude
-- Monads and monad transformers
import Control.Monad.IO.Class
-- Random
import System.Random
-- Text
import Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
-- Formatting
import           Data.Text.Format hiding (format)
import qualified Data.Text.Format        as Format
import qualified Data.Text.Format.Params as Format
import qualified Data.Text.Buildable     as Format
-- Network
import qualified Network.Socket as Network
import qualified Network.Info   as Network
-- Web
import Lucid
import Web.Spock
import Text.HTML.SanitizeXSS (sanitaryURI)
import Web.PathPieces
-- Feeds
import qualified Text.Atom.Feed        as Atom
import qualified Text.Atom.Feed.Export as Atom
import qualified Text.XML.Light.Output as XML
-- acid-state
import Data.SafeCopy


-- | Format a string (a bit like 'Text.Printf.printf' but with different
-- syntax). The version in "Data.Text.Format" returns lazy text, but we
-- use strict text everywhere.
format :: Format.Params ps => Format -> ps -> Text
format f ps = TL.toStrict (Format.format f ps)

tshow :: Show a => a -> Text
tshow = T.pack . show

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

deleteAt :: Int -> [a] -> [a]
deleteAt _   []   = []
deleteAt 0 (_:xs) = xs
deleteAt i (x:xs) = x : deleteAt (i-1) xs

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
  T.map toLower .
  T.filter (\c -> isLetter c || isDigit c || c == ' ' || c == '-') .
  T.map (\x -> if x == '_' then '-' else x)

data IP = IPv4 Network.IPv4 | IPv6 Network.IPv6
  deriving (Eq)

deriveSafeCopy 0 'base ''Network.IPv4
deriveSafeCopy 0 'base ''Network.IPv6

deriveSafeCopy 0 'base ''IP 

instance Show IP where
  show (IPv4 x) = show x
  show (IPv6 x) = show x

sockAddrToIP :: Network.SockAddr -> Maybe IP
sockAddrToIP (Network.SockAddrInet _ addr) =
  Just (IPv4 (Network.IPv4 addr))
sockAddrToIP (Network.SockAddrInet6 _ _ (a,b,c,d) _) =
  Just (IPv6 (Network.IPv6 a b c d))
sockAddrToIP _ = Nothing

-- | Unique id, used for many things – categories, items, and anchor ids.
newtype Uid = Uid {uidToText :: Text}
  deriving (Eq, Ord, Show, PathPiece, Format.Buildable)

-- See Note [acid-state]
deriveSafeCopy 0 'base ''Uid

instance IsString Uid where
  fromString = Uid . T.pack

randomText :: Int -> IO Text
randomText n = do
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

randomLongUid :: MonadIO m => m Uid
randomLongUid = liftIO $ Uid <$> randomText 12

-- These are only used for items and categories (because their uids can occur
-- in links and so they should look a bit nicer).
randomShortUid :: MonadIO m => m Uid
randomShortUid = liftIO $ Uid <$> randomText 8

uid_ :: Uid -> Attribute
uid_ = id_ . uidToText

includeJS :: Monad m => Url -> HtmlT m ()
includeJS url = with (script_ "") [src_ url]

includeCSS :: Monad m => Url -> HtmlT m ()
includeCSS url = link_ [rel_ "stylesheet", type_ "text/css", href_ url]

atomFeed :: MonadIO m => Atom.Feed -> ActionCtxT ctx m ()
atomFeed feed = do
  setHeader "Content-Type" "application/atom+xml; charset=utf-8"
  bytes $ T.encodeUtf8 (T.pack (XML.ppElement (Atom.xmlFeed feed)))
