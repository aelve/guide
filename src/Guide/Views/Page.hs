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
    content,
    head,
    scripts,
    styles,
  pageDef,
  renderPage
)
where

import Imports

-- Text
import qualified Data.Text.All as T
-- JSON
-- import qualified Data.Aeson as A
-- import qualified Data.Aeson.Types as A
-- -- acid-state
-- import Data.SafeCopy hiding (kind)

-- import Guide.SafeCopy
-- -- import Guide.Markdown
-- -- import Guide.Utils
-- import Guide.Types.Hue

-- Lists
-- import Data.List.Split
-- -- Containers
-- import qualified Data.Map as M
-- import Data.Tree
-- Text
-- import qualified Data.Text.All as T
-- import qualified Data.Text.Lazy.All as TL
import NeatInterpolation
-- Web
import Lucid hiding (for_)
-- Files
-- import qualified System.FilePath.Find as F
-- Network
-- import Data.IP
-- Time
-- import Data.Time.Format.Human
-- Markdown
-- import qualified CMark as MD
-- Mustache (templates)
-- import Text.Mustache.Plus
-- import qualified Data.Aeson as A
-- import qualified Data.Aeson.Encode.Pretty as A
-- import qualified Data.ByteString.Lazy.Char8 as BS
-- import qualified Data.Semigroup as Semigroup
-- import qualified Data.List.NonEmpty as NonEmpty
-- import Text.Megaparsec
-- import Text.Megaparsec.Text

import Guide.Config
-- import Guide.State
-- import Guide.Types
import Guide.Utils
import Guide.JS (JS(..))
import qualified Guide.JS as JS
-- import Guide.Markdown
-- import Guide.Cache

import Guide.Views.Utils


data Page m = Page {
    _pageTitle :: Text,
    _pageName :: Maybe Text,
    _pageHeaderUrl :: Url,
    _pageSubtitle :: HtmlT m (),
    _pageScripts :: [Url],
    _pageStyles :: [Url],
    _pageContent :: HtmlT m ()
  }

makeFields ''Page

pageDef :: (MonadIO m, MonadReader Config m) => Page m
pageDef = Page {
    _pageTitle = "Aelve",
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
    _pageContent = undefined
  }

subtitleDef :: (MonadReader Config m) => HtmlT m ()
subtitleDef =
  div_ [class_ "subtitle"] $ do
    "alpha version • don't share yet"
    lift (asks _discussLink) >>= \case
      Nothing -> return ()
      Just l  -> " • " >> mkLink "discuss the site" l


renderPage
  :: (MonadIO m, MonadReader Config m)
  => Page m
  -> HtmlT m ()
renderPage page = doctypehtml_ $ do
  head_ $ do
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

  body_ $ do
    script_ $ fromJS $ JS.createAjaxIndicator ()
    div_ [id_ "header"] $ do
      let headerText = 
            case _pageName page of
              Just pageName -> "Aelve Guide" >> span_ (" | " >> toHtml pageName)
              Nothing -> "Aelve Guide"
      h1_ $ mkLink headerText (_pageHeaderUrl page)
      _pageSubtitle page
    div_ [id_ "main"] $
      _pageContent page
    div_ [id_ "footer"] $ do
      mapM_ (div_ [class_ "footer-item"]) $
        [ do "made by "
             mkLink "Artyom" "https://artyom.me"
        , do mkLink "source" "https://github.com/aelve/guide"
             "/"
             mkLink "issue tracker" "https://github.com/aelve/guide/issues"
        , mkLink "rules" "/unwritten-rules"
        , mkLink "donate" "/donate"
        , do "licensed under "
             mkLink "CC+ BY-SA 4.0" "/license"
        ]
