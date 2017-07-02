module Guide.View.Playground where

import Prelude

import Guide.Events (Event(..))
import Guide.Routes (Route(Home), toUrl)
import Guide.State (State(..))
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, h2, a) as S
import Text.Smolder.HTML.Attributes (href) as S
import Text.Smolder.Markup ((#!), (!))
import Text.Smolder.Markup (text) as S

playgroundView :: State -> P.HTML Event
playgroundView (State st) =
  S.div $ do
    S.h2 $ S.text "Playground"
    S.a ! S.href (toUrl Home)
        #! P.onClick (Navigate $ toUrl Home)
        $ S.text "Back to Home"
    S.div
      #! P.onClick (const RequestGithubUsers)
      $ S.text "Fetch Github users!"
    S.div
      $ S.text $ "Users: " <> st.users
