{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


{- |
Various HTML utils, Mustache utils, etc.
-}
module Guide.Views.Utils
(
  -- * Script utils
  onPageLoad,
  onEnter,
  onCtrlEnter,
  onEscape,
  onFormSubmit,
  inputValue,
  clearInput,

  -- * HTML utils
  emptySpan,
  mkLink,
  selectedIf,
  checkedIf,
  hiddenIf,
  categoryLink,
  itemLink,

  -- * HTML components
  button,
  textButton,
  imgButton,
  textInput,
  markdownEditor,
  smallMarkdownEditor,

  -- * Node identifiers
  thisNode,
  itemNodeId,
  categoryNodeId,

  -- * Sections system
  shown,
  noScriptShown,
  section,
  sectionSpan,

  -- * Widget system & templates
  mustache,
  readWidgets,
  getJS,
  getCSS,
)
where


import Imports

-- Lists
import Data.List.Split
-- Containers
import qualified Data.Map as M
-- import Data.Tree
-- Text
import qualified Data.Text.All as T
import qualified Data.Text.Lazy.All as TL
-- import NeatInterpolation
-- Web
import Lucid hiding (for_)
-- Files
import qualified System.FilePath.Find as F
-- -- Network
-- import Data.IP
-- -- Time
-- import Data.Time.Format.Human
-- -- Markdown
-- import qualified CMark as MD
-- Mustache (templates)
import Text.Mustache.Plus
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Semigroup as Semigroup
import qualified Data.List.NonEmpty as NonEmpty
import Text.Megaparsec
import Text.Megaparsec.Text

-- import Guide.Config
-- import Guide.State
import Guide.Types
import Guide.Utils
import Guide.JS (JS(..), JQuerySelector)
import qualified Guide.JS as JS
import Guide.Markdown
-- import Guide.Cache

-- | Add a script that does something on page load.
onPageLoad :: Monad m => JS -> HtmlT m ()
onPageLoad js = script_ $
  "$(document).ready(function(){"%<js>%"});"

-- | Add some empty space.
emptySpan :: Monad m => Text -> HtmlT m ()
emptySpan w = span_ [style_ ("margin-left:" <> w)] mempty

-- Use inputValue to get the value (works with input_ and textarea_)
onEnter :: JS -> Attribute
onEnter handler = onkeydown_ $
  "if (event.keyCode == 13 || event.keyCode == 10) {"
      %<handler>%" return false;}\n"

onCtrlEnter :: JS -> Attribute
onCtrlEnter handler = onkeydown_ $
  "if ((event.keyCode == 13 || event.keyCode == 10) && " <>
      "(event.metaKey || event.ctrlKey)) {"
      %<handler>%" return false;}\n"

onEscape :: JS -> Attribute
onEscape handler = onkeydown_ $
  "if (event.keyCode == 27) {"
      %<handler>%" return false;}\n"

textInput :: Monad m => [Attribute] -> HtmlT m ()
textInput attrs = input_ (type_ "text" : attrs)

inputValue :: JS
inputValue = JS "this.value"

clearInput :: JS
clearInput = JS "this.value = '';"

onFormSubmit :: (JS -> JS) -> Attribute
onFormSubmit f = onsubmit_ $ format "{} return false;" [f (JS "this")]

button :: Monad m => Text -> [Attribute] -> JS -> HtmlT m ()
button value attrs handler =
  input_ (type_ "button" : value_ value : onclick_ handler' : attrs)
  where
    handler' = fromJS handler

-- A text button looks like “[cancel]”
textButton
  :: Monad m
  => Text         -- ^ Button text
  -> JS           -- ^ Onclick handler
  -> HtmlT m ()
textButton caption (JS handler) =
  span_ [class_ "text-button"] $
    -- “#” is used instead of javascript:void(0) because the latter is slow
    -- in Firefox (at least for me – tested with Firefox 43 on Arch Linux)
    a_ [href_ "#", onclick_ (handler <> "return false;")]
       (toHtml caption)

-- So far all icons used here have been from <https://useiconic.com/open/>
imgButton :: Monad m => Text -> Url -> [Attribute] -> JS -> HtmlT m ()
imgButton alt src attrs (JS handler) =
  a_ [href_ "#", onclick_ (handler <> "return false;")]
     (img_ (src_ src : alt_ alt : title_ alt : attrs))

mkLink :: Monad m => HtmlT m a -> Url -> HtmlT m a
mkLink x src = a_ [href_ src] x

selectedIf :: With w => Bool -> w -> w
selectedIf p x = if p then with x [selected_ "selected"] else x

checkedIf :: With w => Bool -> w -> w
checkedIf p x = if p then with x [checked_] else x

hiddenIf :: With w => Bool -> w -> w
hiddenIf p x = if p then with x [style_ "display:none;"] else x

markdownEditor
  :: MonadIO m
  => [Attribute]
  -> MarkdownBlock  -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving the contents of the editor
  -> JS             -- ^ “Cancel” handler
  -> Text           -- ^ Instruction (e.g. “press Ctrl+Enter to save”)
  -> HtmlT m ()
markdownEditor attr (view mdText -> s) submit cancel instr = do
  textareaUid <- randomLongUid
  let val = JS $ "document.getElementById(\""%<textareaUid>%"\").value"
  -- Autocomplete has to be turned off thanks to
  -- <http://stackoverflow.com/q/8311455>.
  textarea_ ([uid_ textareaUid,
              autocomplete_ "off",
              class_ "big fullwidth",
              onCtrlEnter (submit val),
              onEscape (JS.assign val s <> cancel) ]
             ++ attr) $
    toHtml s
  button "Save" [class_ " save "] $
    submit val
  emptySpan "6px"
  button "Cancel" [class_ " cancel "] $
    JS.assign val s <>
    cancel
  emptySpan "6px"
  span_ [class_ "edit-field-instruction"] (toHtml instr)
  a_ [href_ "/markdown", target_ "_blank"] $
    img_ [src_ "/markdown.svg", alt_ "markdown supported",
          class_ " markdown-supported "]

smallMarkdownEditor
  :: MonadIO m
  => [Attribute]
  -> MarkdownInline -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving the contents of the editor
  -> Maybe JS       -- ^ “Cancel” handler (if “Cancel” is needed)
  -> Text           -- ^ Instruction (e.g. “press Enter to add”)
  -> HtmlT m ()
smallMarkdownEditor attr (view mdText -> s) submit mbCancel instr = do
  textareaId <- randomLongUid
  let val = JS $ "document.getElementById(\""%<textareaId>%"\").value"
  textarea_ ([class_ "fullwidth", uid_ textareaId, autocomplete_ "off"] ++
             [onEnter (submit val)] ++
             [onEscape cancel | Just cancel <- [mbCancel]] ++
             attr) $
    toHtml s
  br_ []
  for_ mbCancel $ \cancel -> do
    textButton "cancel" $
      JS.assign val s <>
      cancel
  span_ [style_ "float:right"] $ do
    span_ [class_ "edit-field-instruction"] (toHtml instr)
    a_ [href_ "/markdown", target_ "_blank"] $
      img_ [src_ "/markdown.svg", alt_ "markdown supported",
            class_ " markdown-supported "]

thisNode :: MonadIO m => HtmlT m JQuerySelector
thisNode = do
  uid' <- randomLongUid
  -- If the class name ever changes, fix 'JS.moveNodeUp' and
  -- 'JS.moveNodeDown'.
  span_ [uid_ uid', class_ "dummy"] mempty
  return (JS.selectParent (JS.selectUid uid'))

itemNodeId :: Item -> Text
itemNodeId item = format "item-{}" [item^.uid]

categoryNodeId :: Category -> Text
categoryNodeId category = format "category-{}" [category^.uid]

-- TODO: another absolute link to get rid of [absolute-links]
categoryLink :: Category -> Url
categoryLink category = format "/haskell/{}" [categorySlug category]

itemLink :: Category -> Item -> Url
itemLink category item =
  format "/haskell/{}#{}" (categorySlug category, itemNodeId item)

-- See Note [show-hide]; wheh changing these, also look at 'JS.switchSection'.
shown, noScriptShown :: Attribute
shown          = class_ " shown "
noScriptShown  = class_ " noscript-shown "

-- See Note [show-hide]
section
  :: Monad m
  => Text           -- ^ Section name (or names)
  -> [Attribute]    -- ^ Additional attributes
  -> HtmlT m ()     -- ^ Content of the section
  -> HtmlT m ()
section t attrs = div_ (class_ (t <> " section ") : attrs)

-- See Note [show-hide]
sectionSpan
  :: Monad m
  => Text           -- ^ Section name (or names)
  -> [Attribute]    -- ^ Additional attributes
  -> HtmlT m ()     -- ^ Content of the section
  -> HtmlT m ()
sectionSpan t attrs = span_ (class_ (t <> " section ") : attrs)

{-
TODO: warn about how one shouldn't write @foo("{{bar}}")@ in templates,
because a newline in 'bar' directly after the quote will mess things
up. Write @foo({{{%js bar}}})@ instead.
-}
mustache :: MonadIO m => PName -> A.Value -> HtmlT m ()
mustache f v = do
  let functions = M.fromList [
        ("selectIf", \[x] -> if x == A.Bool True
            then return (A.String "selected")
            else return A.Null),
        ("js", \[x] -> return $
            A.String . T.toStrict . TL.decodeUtf8 . A.encode $ x),
        ("trace", \xs -> do
            mapM_ (BS.putStrLn . A.encodePretty) xs
            return A.Null) ]
  widgets <- readWidgets
  let templates = [(tname, t) | (HTML_ tname, t) <- widgets]
  when (null templates) $
    error "View.mustache: no HTML templates found in templates/"
  parsed <- for templates $ \(tname, t) -> do
    let pname = fromString (T.unpack tname)
    case compileMustacheText pname (T.toLazy t) of
      Left e -> error $ printf "View.mustache: when parsing %s: %s"
                               tname (parseErrorPretty e)
      Right template -> return template
  let combined = (Semigroup.sconcat (NonEmpty.fromList parsed)) {
                   templateActual = f }
  (rendered, warnings) <- liftIO $ renderMustacheM functions combined v
  when (not (null warnings)) $
    error $ printf "View.mustache: warnings when rendering %s:\n%s"
                   (unPName f) (unlines warnings)
  toHtmlRaw rendered

data SectionType
  = HTML_ Text | JS_ | CSS_ | Description_ | Note_ Text

-- | Used to turn collected section lines back into a section.
--
-- * Trims surrounding blank lines
-- * Doesn't append a newline when there's only one line
--   (useful for inline partials)
unlinesSection :: [Text] -> Text
unlinesSection = unlines' . dropWhile T.null . dropWhileEnd T.null
  where
    unlines' []  = ""
    unlines' [x] = x
    unlines' xs  = T.unlines xs

readWidget :: MonadIO m => FilePath -> m [(SectionType, Text)]
readWidget fp = liftIO $ do
  s <- T.readFile fp
  let isDivide line = (T.all (== '=') line || T.all (== '-') line) &&
                      T.length line >= 20
  let go (x:y:[]) = [(T.strip (last x), unlinesSection y)]
      go (x:y:xs) = (T.strip (last x), unlinesSection (init y)) : go (y : xs)
      go _ = error $ "View.readWidget: couldn't read " ++ fp
  let sections = go (splitWhen isDivide (T.lines s))
  let sectionTypeP :: Parser SectionType
      sectionTypeP = choice [
        do string "HTML"
           HTML_ <$> choice [
             string ": " >> (T.pack <$> some anyChar),
             return (T.pack (takeBaseName fp)) ],
        string "JS" $> JS_,
        string "CSS" $> CSS_,
        string "Description" $> Description_,
        do string "Note ["
           Note_ . T.pack <$> someTill anyChar (char ']') ]
  let parseSectionType t = case parse (sectionTypeP <* eof) fp t of
        Right x -> x
        Left e -> error $ printf "invalid section name: '%s'\n%s"
                          t (parseErrorPretty e)
  return $ over (each._1) parseSectionType sections

readWidgets :: MonadIO m => m [(SectionType, Text)]
readWidgets = liftIO $ do
  let isWidget = F.extension F.==? ".widget"
  files <- F.find F.always isWidget "templates/"
  concat <$> mapM readWidget files

getJS :: MonadIO m => m Text
getJS = do
  widgets <- readWidgets
  let js = [t | (JS_, t) <- widgets]
  return (T.concat js)

getCSS :: MonadIO m => m Text
getCSS = do
  widgets <- readWidgets
  let css = [t | (CSS_, t) <- widgets]
  return (T.concat css)
