module Guide.View.Playground where

import Prelude

import Guide.State (State)
import Guide.Events (Event(..))
import Guide.Routes (Route(Home), toUrl)

import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P

import Text.Smolder.HTML (div, h2, a) as S
import Text.Smolder.HTML.Attributes (href) as S
import Text.Smolder.Markup ((#!), (!))
import Text.Smolder.Markup (text) as S

playgroundView :: State -> P.HTML Event
playgroundView _ =
  S.div $ do
    S.h2 $ S.text "Playground"
    S.a ! S.href (toUrl Home)
        #! P.onClick (Navigate $ toUrl Home)
        $ S.text "Back to Home"
