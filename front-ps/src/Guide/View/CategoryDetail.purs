module Guide.View.CategoryDetail
  ( view
  ) where

import Prelude

import Guide.Events (Event(..))
import Guide.Routes (Route(..), toUrl)
import Guide.State (State(..))
import Guide.Types (CategoryName(..))
import Guide.Api.ClientTypes (CUid(..))
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, h1, a) as S
import Text.Smolder.HTML.Attributes (href) as S
import Text.Smolder.Markup ((#!), (!))
import Text.Smolder.Markup (text) as S

view :: CategoryName -> CUid String -> State -> P.HTML Event
view (CategoryName cName) (CUid catId) (State st) =
  S.div $ do
    S.h1 $ S.text (cName <> " - " <> catId)
    S.a ! S.href (toUrl Home)
        #! P.onClick (Navigate $ toUrl Home)
        $ S.text "Back to Home"
