module Guide.CategoryOverview.View.CategoryOverview where

import Prelude

import Data.Foldable (for_)
import Data.Generic (gShow)
import Guide.Api.Types (CategoryInfo(..))
import Guide.CategoryOverview.Events (Event)
import Guide.CategoryOverview.State (State(..))
import Guide.Common.Types (CCategories)
import Network.RemoteData (RemoteData(..))
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Text.Smolder.HTML (div, h1, h2, h3, a, ul, li) as S
import Text.Smolder.Markup ((#!), (!))
import Text.Smolder.Markup (text) as S

view :: State -> P.HTML Event
view st@(State state) =
  S.div do
    S.h1 $ S.text "Category Overview"
    case state.categories of
      NotAsked -> S.div $ S.text "Categories not asked."
      Loading -> S.div $ S.text "Loading data..."
      Failure error -> S.div $ S.text $ "Error loading data: " <> (show error)
      (Success cats) -> catsView st cats

catsView :: State -> CCategories -> P.HTML Event
catsView st@(State state) cats =
  S.ul
    $ for_ cats (catView st)

catView :: State -> CategoryInfo -> P.HTML Event
catView _ (CategoryInfo cat) =
  S.li
    ! P.key (gShow cat.categoryInfoUid) $ do
    S.h2
      $ S.text cat.categoryInfoTitle
