module Guide.CategoryOverview.View.Homepage where

import Prelude

import Guide.CategoryOverview.Events (Event)
import Guide.CategoryOverview.State (State)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, h1) as S
import Text.Smolder.Markup (text)

view :: State -> HTML Event
view s =
  S.div do
    S.h1 $ text "Category Overview"
