module Guide.View.HTMLWrapper where

import Prelude

import Data.Monoid (mempty)
import Guide.Config (config)

import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Pux.Renderer.React (dangerouslySetInnerHTML) as P

import Text.Smolder.HTML (body, div, head, html, link, meta, script, title) as S
import Text.Smolder.HTML.Attributes (charset, content, href, id, name, rel, src, type') as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((!))

htmlWrapper :: ∀ ev. String -> String -> P.HTML ev
htmlWrapper app_html state_json =
  S.html do
    S.head do
      S.meta ! S.charset "UTF-8"
      S.meta ! S.name "viewport" ! S.content "width=device-width, initial-scale=1"
      S.title $ S.text config.title
      S.link ! S.rel "icon" ! S.type' "image/x-icon" ! S.href "/favicon.ico"
    S.body do
      S.div ! P.key "app" ! S.id "app" ! P.dangerouslySetInnerHTML app_html $ mempty
      S.script ! P.key "initial_state" ! S.type' "text/javascript" ! P.dangerouslySetInnerHTML state_json $ mempty
      S.script ! P.key "js_bundle" ! S.type' "text/javascript" ! S.src (config.public_path <> "/bundle.js") $ mempty
