module Guide.View.Haskell where

import Prelude

import Data.Foldable (for_)
import Guide.Api.ClientTypes (CGrandCategory(..))
import Guide.Events (Event(..))
import Guide.Routes (Route(Home), toUrl)
import Guide.State (State(..))
import Guide.Types (CGrandCategories)
import Network.RemoteData (RemoteData(..))
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, h2, a, ul, li) as S
import Text.Smolder.HTML.Attributes (href) as S
import Text.Smolder.Markup ((#!), (!))
import Text.Smolder.Markup (text) as S

haskellView :: State -> P.HTML Event
haskellView (State st) =
  S.div $ do
    S.h2 $ S.text "Haskell"
    S.a ! S.href (toUrl Home)
        #! P.onClick (Navigate $ toUrl Home)
        $ S.text "Back to Home"
    case st.grandCategories of
      NotAsked -> S.div $ S.text "GrandCategories not asked."
      Loading -> S.div $ S.text "Loading data..."
      Failure error -> S.div $ S.text $ "Error loading data: " <> (show error)
      (Success cats) -> gCatsView cats
      (Refreshing cats) ->
          S.div do
            S.div $ S.text "Refreshing data..."
            gCatsView cats

gCatsView :: CGrandCategories -> P.HTML Event
gCatsView cats =
    S.ul
      $ for_ cats gCatView

gCatView :: CGrandCategory -> P.HTML Event
gCatView (CGrandCategory cat) =
    S.li
      $ S.text cat.cgcTitle
