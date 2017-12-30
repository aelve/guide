module Guide.Server.HTMLWrapper where

import Prelude

import CSS.Render (render, renderedSheet)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, (?))
import CSS.Text (textDecoration, underline)
import Data.Maybe (fromMaybe)
import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Pux.Renderer.React (dangerouslySetInnerHTML) as P
import Text.Smolder.HTML (body, div, head, html, link, meta, script, style, title) as S
import Text.Smolder.HTML.Attributes (charset, className, content, href, id, name, rel, src, type') as S
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (empty, text) as S
import Bulma.Common (runClassName) as Bulma
import Bulma.Layout.Layout (container) as Bulma

globalStyle :: CSS
globalStyle = do
  fromString "a:hover" ? do
    textDecoration underline

htmlWrapper :: forall eff . String -> String -> String -> P.HTML eff
htmlWrapper app_html state_json file_name =
  S.html do
    S.head do
      S.meta ! S.charset "UTF-8"
      S.meta ! S.name "viewport" ! S.content "width=device-width, initial-scale=1"
      S.title $ S.text "Aelve-Guide" -- TODO (sectore): Provide title
      S.link ! S.rel "icon" ! S.type' "image/x-icon" ! S.href "/favicon.ico"
      S.link ! S.rel "stylesheet" ! S.href ("/vendor.css")
      -- ^ vendor styles (Bluma & Co.)
      S.style ! P.dangerouslySetInnerHTML (fromMaybe "" (renderedSheet (render globalStyle))) $ S.empty
      -- ^ global styles
    S.body do
      S.div
        ! P.key "guide"
        ! S.id "guide"
        ! S.className (Bulma.runClassName Bulma.container)
        ! P.dangerouslySetInnerHTML app_html
        $ S.empty
      S.script
        ! P.key "initial_state"
        ! S.type' "text/javascript"
        ! P.dangerouslySetInnerHTML state_json
        $ S.empty
      S.script
        ! P.key "js_bundle"
        ! S.type' "text/javascript"
        ! S.src ("/" <> file_name <> ".js")
        $ S.empty
