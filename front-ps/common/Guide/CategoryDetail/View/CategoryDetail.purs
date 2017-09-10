module Guide.CategoryDetail.View.CategoryDetail where

import Prelude

import Data.Lens ((^.))
import Guide.Api.Types (CCategoryDetail(..), _CUid)
import Guide.CategoryDetail.Events (Event)
import Guide.CategoryDetail.State (State(..))
import Network.RemoteData (RemoteData(..))
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, h1, h2) as S
import Text.Smolder.Markup (text) as S

view :: State -> HTML Event
view (State state) =
  S.div do
    S.h1 $ S.text "Category Detail"
    case state.category of
      NotAsked -> S.div $ S.text "Categories not asked."
      Loading -> S.div $ S.text "Loading data..."
      Failure error -> S.div $ S.text $ "Error loading data: " <> (show error)
      (Success (CCategoryDetail cat)) -> S.h2 $ S.text (cat.ccdTitle <> " - " <> (cat.ccdUid ^. _CUid))
