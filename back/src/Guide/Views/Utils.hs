{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Various HTML utils, Mustache utils, etc.
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

  protectForm,
  getCsrfHeader,

  module Guide.Views.Utils.Input
)
where


import Imports hiding (some)

-- Web
import Web.Spock
import Web.Spock.Config
-- Lists
import Data.List.Split
-- digestive-functors
import Text.Digestive (View)
-- import NeatInterpolation
-- Web
import Lucid hiding (for_)
import Lucid.Base (makeAttribute)
-- Files
-- -- Network
-- import Data.IP
-- -- Time
-- import Data.Time.Format.Human
-- -- Markdown
-- import qualified CMark as MD
-- Mustache (templates)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Mustache.Plus

import Guide.App
import Guide.JS (JQuerySelector, JS (..))
import Guide.Markdown
import Guide.Types
import Guide.Utils
-- import Guide.Config
-- import Guide.State
-- import Guide.Cache

import Guide.Views.Utils.Input

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.FilePath.Find as F

import qualified Guide.JS as JS


-- | Add a script that does something on page load.
onPageLoad :: Monad m => JS -> HtmlT m ()
onPageLoad js = script_ $
  "$(document).ready(function(){"+|js|+"});"

-- | Add some empty space.
emptySpan :: Monad m => Text -> HtmlT m ()
emptySpan w = span_ [style_ ("margin-left:" <> w)] mempty

-- Use inputValue to get the value (works with input_ and textarea_)
onEnter :: JS -> Attribute
onEnter handler = onkeydown_ $
  "if (event.keyCode == 13 || event.keyCode == 10) {"
      +|handler|+" return false;}\n"

onCtrlEnter :: JS -> Attribute
onCtrlEnter handler = onkeydown_ $
  "if ((event.keyCode == 13 || event.keyCode == 10) && " <>
      "(event.metaKey || event.ctrlKey)) {"
      +|handler|+" return false;}\n"

onEscape :: JS -> Attribute
onEscape handler = onkeydown_ $
  "if (event.keyCode == 27) {"
      +|handler|+" return false;}\n"

textInput :: Monad m => [Attribute] -> HtmlT m ()
textInput attrs = input_ (type_ "text" : attrs)

inputValue :: JS
inputValue = JS "this.value"

clearInput :: JS
clearInput = JS "this.value = '';"

onFormSubmit :: (JS -> JS) -> Attribute
onFormSubmit f = onsubmit_ $ format "{} return false;" (f (JS "this"))

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

-- | @v-bind@ from Vue.js
vBind :: JS.ToJS a => Text -> a -> Attribute
vBind x val = makeAttribute (":" <> x) (fromJS (JS.toJS val))

-- | @v-on@ from Vue.js
--
-- You can access the event payload with @$event@.
vOn :: Text -> JS -> Attribute
vOn x js = makeAttribute ("@" <> x) (fromJS js)

markdownEditor
  :: MonadIO m
  => Int            -- ^ How many rows the editor should have
  -> MarkdownBlock  -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving a variable with the
                    --   contents of the editor
  -> JS             -- ^ “Cancel” handler
  -> Text           -- ^ Instruction (e.g. “press Ctrl+Enter to save”)
  -> HtmlT m ()
markdownEditor rows (view mdSource -> src) submit cancel instr = do
  editorUid <- randomLongUid
  term "a-editor" [uid_ editorUid,
                   vBind "init-content" src,
                   vBind "instruction" instr,
                   vBind "rows" rows,
                   vOn "submit-edit" (submit (JS "$event")),
                   vOn "cancel-edit" cancel]
    (pure ())
  script_ (format "new Vue({el: '#{}'});" editorUid)

smallMarkdownEditor
  :: MonadIO m
  => Int            -- ^ How many rows the editor should have
  -> MarkdownInline -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving a variable with the
                    --   contents of the editor
  -> Maybe JS       -- ^ “Cancel” handler (if “Cancel” is needed)
  -> Text           -- ^ Instruction (e.g. “press Enter to add”)
  -> Maybe Text     -- ^ Placeholder
  -> HtmlT m ()
smallMarkdownEditor rows (view mdSource -> src) submit mbCancel instr mbPlaceholder = do
  editorUid <- randomLongUid
  term "a-editor-mini" ([uid_ editorUid,
                         vBind "init-content" src,
                         vBind "instruction" instr,
                         vBind "rows" rows] ++
                         map (vBind "placeholder") (maybeToList mbPlaceholder) ++
                         [vOn "submit-edit" (submit (JS "$event"))] ++
                         case mbCancel of {
                           Nothing     -> [vBind "allow-cancel" False];
                           Just cancel -> [vOn "cancel-edit" cancel]; })
    (pure ())
  script_ (format "new Vue({el: '#{}'});" editorUid)

thisNode :: MonadIO m => HtmlT m JQuerySelector
thisNode = do
  uid' <- randomLongUid
  -- If the class name ever changes, fix 'JS.moveNodeUp' and
  -- 'JS.moveNodeDown'.
  span_ [uid_ uid', class_ "dummy"] mempty
  return (JS.selectParent (JS.selectUid uid'))

itemNodeId :: Item -> Text
itemNodeId item = format "item-{}" (item^.uid)

categoryNodeId :: Category -> Text
categoryNodeId category = format "category-{}" (category^.uid)

-- TODO: another absolute link to get rid of [absolute-links]
categoryLink :: Category -> Url
categoryLink category = format "/haskell/{}" (categorySlug category)

itemLink :: Category -> Item -> Url
itemLink category item =
  format "/haskell/{}#{}" (categorySlug category) (itemNodeId item)

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
        ("js", \[x] -> return $ A.String (toJson x)),
        ("trace", \xs -> do
            mapM_ (BS.putStrLn . toJsonPretty) xs
            return A.Null) ]
  widgets <- readWidgets
  let templates = [(tname, t) | (HTML_ tname, t) <- widgets]
  when (null templates) $
    error "View.mustache: no HTML templates found in templates/"
  parsed <- for templates $ \(tname, t) -> do
    let pname = fromString (toString tname)
    case compileMustacheText pname t of
      Left e -> error $ printf "View.mustache: when parsing %s: %s"
                               tname (parseErrorPretty e)
      Right template -> return template
  let combined = (Semigroup.sconcat (NonEmpty.fromList parsed)) {
                   templateActual = f }
  (rendered, warnings) <- liftIO $ renderMustacheM functions combined v
  unless (null warnings) $
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
  let go [x,y]    = [(T.strip (last x), unlinesSection y)]
      go (x:y:xs) = (T.strip (last x), unlinesSection (init y)) : go (y : xs)
      go _        = error $ "View.readWidget: couldn't read " ++ fp
  let sections = go (splitWhen isDivide (T.lines s))
  let sectionTypeP :: Parsec Void Text SectionType
      sectionTypeP = choice [
        do
           _ <- string "HTML"
           HTML_ <$> choice [
             string ": " >> (toText <$> some anyChar),
             return (toText (takeBaseName fp)) ],
        string "JS" $> JS_,
        string "CSS" $> CSS_,
        string "Description" $> Description_,
        do
           _ <- string "Note ["
           Note_ . toText <$> someTill anyChar (char ']') ]
  let parseSectionType t = case parse (sectionTypeP <* eof) fp t of
        Right x -> x
        Left e -> error $ printf "invalid section name: '%s'\n%s"
                          t (parseErrorPretty e)
  return $ over (each._1) parseSectionType sections

readWidgets :: MonadIO m => m [(SectionType, Text)]
readWidgets = liftIO $ do
  let isWidget = F.extension F.==? ".widget"
  files' <- F.find F.always isWidget "templates/"
  concat <$> mapM readWidget files'

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

-- | 'protectForm' renders a set of input fields within a CSRF-protected form.
--
-- This sets the method (POST) of submission and includes a server-generated
-- token to help prevent cross-site request forgery (CSRF) attacks.
--
-- Briefly: this is necessary to prevent third party sites from impersonating
-- logged in users, because a POST to the right URL is not sufficient to
-- submit the form and perform an action. The CSRF token is only displayed
-- when viewing the page.
protectForm :: MonadIO m
  => (View (HtmlT m ()) -> HtmlT m ())
  -> View (HtmlT m ())
  -> GuideAction ctx (HtmlT m ())
protectForm render formView = do
  (name', value) <- getCsrfTokenPair
  return $ form formView "" [id_ "login-form"] $ do
    input_ [ type_ "hidden", name_ name', value_ value ]
    render formView

getCsrfTokenPair :: GuideAction ctx (Text, Text)
getCsrfTokenPair = do
  csrfTokenName <- spc_csrfPostName <$> getSpockCfg
  csrfTokenValue <- getCsrfToken
  return (csrfTokenName, csrfTokenValue)

getCsrfHeader :: GuideAction ctx (Text, Text)
getCsrfHeader = do
  csrfTokenName <- spc_csrfHeaderName <$> getSpockCfg
  csrfTokenValue <- getCsrfToken
  return (csrfTokenName, csrfTokenValue)
