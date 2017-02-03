{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Guide.Views.Page
(
  Page (..),
    title,
    name,
    headerUrl,
    subtitle,
    scripts,
    styles,
    headTag,
    header,
    content,
    footer,
  pageDef,
  renderPage
)
where

import Imports

-- Text
import qualified Data.Text.All as T
import NeatInterpolation
-- Web
import Lucid hiding (for_)

import Guide.Config
-- import Guide.State
-- import Guide.Types
import Guide.Utils
import Guide.JS (JS(..))
import qualified Guide.JS as JS
-- import Guide.Markdown
-- import Guide.Cache

import Guide.Views.Utils

-- |Record for the parts of a page, for rendering in a standard template.
data Page m = Page {
    -- |Title for the root of a site.
    -- By default displays before the page name in the <title> and header of page, e.g.: "Aelve Guide"
    _pageTitle :: Text,
    -- |Name of a particular page.
    -- Displays after the title, e.g.: "Haskell" or "Login"
    _pageName :: Maybe Text,
    -- |Url accessed by clicking header text.
    _pageHeaderUrl :: Url,
    -- |Subtitle element.
    -- By default displays immediately below the "Title | Name" header.
    _pageSubtitle :: Page m -> HtmlT m (),
    -- |Scripts to load in the <head> tag.
    _pageScripts :: [Url],
    -- |Stylesheets to load in the <head> tag. 
    _pageStyles :: [Url],
    -- |Contents of <head> tag
    _pageHeadTag :: Page m -> HtmlT m (),
    -- |Header element to display at the top of the body element.
    _pageHeader :: Page m -> HtmlT m (),
    -- |Content element to display in the center of the template.
    _pageContent :: HtmlT m (),
    -- |Footer element to display at bottom of the body element.
    _pageFooter :: Page m -> HtmlT m ()
  }

makeFields ''Page

pageDef :: (MonadIO m, MonadReader Config m) => Page m
pageDef = Page {
    _pageTitle = "Aelve Guide",
    _pageName = Nothing,
    _pageHeaderUrl = "/",
    _pageSubtitle = subtitleDef,
    _pageStyles =
      [ "/magnific-popup.css"
      , "/highlight.css"
      , "/markup.css"
      , "/css.css"
      , "/loader.css"
      ],
    _pageScripts =
      [ "/jquery.js"
      , "/magnific-popup.js"
      , "/autosize.js"
      , "/js.js"
      ],
    _pageHeadTag = headTagDef,
    _pageHeader = headerDef,
    _pageContent = mempty,
    _pageFooter = footerDef
  }

subtitleDef 
  :: (MonadIO m, MonadReader Config m)
  => Page m
  -> HtmlT m ()
subtitleDef _page = do
  div_ [class_ "subtitle"] $ do
    "alpha version • don't share yet"
    lift (asks _discussLink) >>= \case
      Nothing -> return ()
      Just l  -> " • " >> mkLink "discuss the site" l

headTagDef 
  :: (MonadIO m, MonadReader Config m)
  => Page m
  -> HtmlT m ()
headTagDef page = do
  let titleText = case _pageName page of
        Just pageName -> _pageTitle page <> " | " <> pageName
        Nothing -> _pageTitle page
  title_ $ toHtml titleText
  meta_ [name_ "viewport",
          content_ "width=device-width, initial-scale=1.0, user-scalable=yes"]
  link_ [rel_ "icon", href_ "/favicon.ico"]
  googleToken <- _googleToken <$> lift ask
  unless (T.null googleToken) $
    meta_ [name_ "google-site-verification", content_ googleToken]
  -- Report all Javascript errors with alerts
  script_ [text|
    window.onerror = function (msg, url, lineNo, columnNo, error) {
      alert("Error in "+url+" at "+lineNo+":"+columnNo+": "+msg+
            "\n\n"+
            "========== Please report it! =========="+
            "\n\n"+
            "https://github.com/aelve/guide/issues");
      return false; };
    |]
  mapM_ includeJS $ _pageScripts page
  mapM_ includeCSS $ _pageStyles page
  onPageLoad (JS "autosize($('textarea'));")
  -- CSS that makes 'shown' and 'noScriptShown' work;
  -- see Note [show-hide]
  noscript_ $ style_ [text|
    .section:not(.noscript-shown) {display:none;}
    |]
  script_ [text|
    var sheet = document.createElement('style');
    sheet.innerHTML = '.section:not(.shown) {display:none;}';
    // “head” instead of “body” because body isn't loaded yet
    document.head.appendChild(sheet);
    |]

headerDef 
  :: MonadIO m
  => Page m
  -> HtmlT m ()
headerDef page = do
  let nameHtml = 
        case _pageName page of
          Just pageName -> span_ (" | " >> toHtml pageName)
          Nothing -> mempty
  h1_ $ mkLink (toHtml (_pageTitle page) >> nameHtml) (_pageHeaderUrl page)
  (_pageSubtitle page) page


footerDef 
  :: MonadIO m
  => Page m
  -> HtmlT m ()
footerDef _page = do
  mapM_ (div_ [class_ "footer-item"]) $
    [ do 
        "made by "
        mkLink "Artyom" "https://artyom.me"
    , do 
        mkLink "source" "https://github.com/aelve/guide"
        "/"
        mkLink "issue tracker" "https://github.com/aelve/guide/issues"
    , mkLink "rules" "/unwritten-rules"
    , mkLink "donate" "/donate"
    , do 
        "licensed under "
        mkLink "CC+ BY-SA 4.0" "/license"
    ]


renderPage
  :: MonadIO m
  => Page m
  -> HtmlT m ()
renderPage page = doctypehtml_ $ do
  head_ $ (_pageHeadTag page) page
  body_ $ do
    script_ $ fromJS $ JS.createAjaxIndicator ()
    div_ [id_ "header"] $ (_pageHeader page) page
    div_ [id_ "main"] $ _pageContent page
    div_ [id_ "footer"] $ (_pageFooter page) page
