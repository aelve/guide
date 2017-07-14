module Guide.View.Layout where

import Prelude

import Guide.Events (Event)
import Guide.Routes (Route(..))
import Guide.State (State(..))
import Guide.View.CategoryOverview as CategoryOverview
import Guide.View.CategoryDetail as CategoryDetail
import Guide.View.Homepage as Homepage
import Guide.View.NotFound as NotFound
import Guide.View.Playground (playgroundView)
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div) as S
import Text.Smolder.HTML.Attributes (className) as S
import Text.Smolder.Markup ((!))

view :: State -> P.HTML Event
view state@(State st) =
  S.div ! S.className "app" $ do
    case st.route of
      Home -> Homepage.view state
      (NotFound url) -> NotFound.view state
      (CategoryOverview catName) -> CategoryOverview.view catName state
      (CategoryDetail catName catId) -> CategoryDetail.view catName catId state
      Playground -> playgroundView state
