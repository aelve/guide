module Guide.View.Homepage where

import Prelude

import Guide.Events (Event(..))
import Guide.Routes (Route(..), toUrl)
import Guide.State (State)
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (a, div, h1, span) as S
import Text.Smolder.HTML.Attributes (href) as S
import Text.Smolder.Markup ((!), (#!))
import Text.Smolder.Markup (text) as S

view :: State -> P.HTML Event
view s =
  S.div do
    S.h1 $ S.text "Aelve Guide"
    S.a ! S.href (toUrl Haskell)
        #! P.onClick (Navigate $ toUrl Haskell)
        $ S.text "Haskell"
    S.span $ S.text " | "
    S.a ! S.href (toUrl Playground)
        #! P.onClick (Navigate $ toUrl Playground)
        $ S.text "Playground"
