module Guide.CategoryOverview.View.Layout where

import Prelude

import Guide.CategoryOverview.View.CategoryOverview as CategoryOverview
import Guide.Common.View.Header as Header
import Guide.Common.View.Footer as Footer
import Guide.CategoryOverview.View.NotFound as NotFound
import Guide.CategoryOverview.Routes (Route(..))
import Guide.CategoryOverview.State (State(..))
import Guide.CategoryOverview.Events (Event)
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div) as S
import Text.Smolder.HTML.Attributes (className) as S
import Text.Smolder.Markup ((!))
import Bulma.Common (runClassName) as Bulma
import Bulma.Columns.Columns (column) as Bulma

view :: State -> P.HTML Event
view state@(State st) =
  S.div
    ! S.className (Bulma.runClassName Bulma.column) $ do
      Header.view state
      case st.route of
        (NotFound url) -> NotFound.view state
        (CategoryOverview name)-> CategoryOverview.view state
      Footer.view state
