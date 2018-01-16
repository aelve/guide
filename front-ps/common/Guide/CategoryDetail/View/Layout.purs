module Guide.CategoryDetail.View.Layout where

import Guide.CategoryDetail.View.CategoryDetail as CategoryDetail
import Guide.CategoryDetail.View.NotFound as NotFound
import Guide.CategoryDetail.Routes (Route(..))
import Guide.CategoryDetail.State (State(..))
import Guide.CategoryDetail.Events (Event)
import Data.Function (($))
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    case st.route of
      (NotFound url) -> NotFound.view (State st)
      (CategoryDetail _ _)-> CategoryDetail.view (State st)
