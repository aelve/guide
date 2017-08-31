module Guide.CategoryDetail.View.Homepage where

import Prelude

import Guide.CategoryDetail.Events (Event)
import Guide.CategoryDetail.State (State)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, h1) as S
import Text.Smolder.Markup (text)

view :: State -> HTML Event
view s =
  S.div do
    S.h1 $ text "Category Detail"
