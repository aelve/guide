module Guide.Server.HTMLWrapper where

import Prelude

import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Pux.Renderer.React (dangerouslySetInnerHTML) as P

import Text.Smolder.HTML (body, div, head, html, link, meta, script, title, br) as S
import Text.Smolder.HTML.Attributes (charset, content, href, id, name, rel, src, type') as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((!))

htmlWrapper :: forall eff . String -> String -> String -> P.HTML eff
htmlWrapper app_html state_json js_name =
  S.html do
    S.head do
      S.meta ! S.charset "UTF-8"
      S.meta ! S.name "viewport" ! S.content "width=device-width, initial-scale=1"
      S.title $ S.text "Aelve-Guide" -- TODO (sectore): Provide title
      S.link ! S.rel "icon" ! S.type' "image/x-icon" ! S.href "/favicon.ico"
    S.body do
      S.div
        ! P.key "guide"
        ! S.id "guide"
        ! P.dangerouslySetInnerHTML app_html
        $ S.br
      S.script
        ! P.key "initial_state"
        ! S.type' "text/javascript"
        ! P.dangerouslySetInnerHTML state_json
        $ S.br
      S.script
        ! P.key "js_bundle"
        ! S.type' "text/javascript"
        ! S.src ("/" <> js_name <> ".js")
        $ S.br
