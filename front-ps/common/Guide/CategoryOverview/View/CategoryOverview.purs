module Guide.CategoryOverview.View.CategoryOverview where

import Prelude

import Data.Foldable (for_)
import Data.Lens ((^.))
import Guide.Api.Types (CategoryInfo(..), _CUid)
import Guide.CategoryOverview.Events (Event)
import Guide.CategoryOverview.State (State(..))
import Guide.Common.Routes (categoryDetailUrl)
import Guide.Common.Types (CCategories)
import Network.RemoteData (RemoteData(..))
import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Text.Smolder.HTML (div, h1, a, ul, li) as S
import Text.Smolder.HTML.Attributes (href) as S
import Text.Smolder.Markup ((!))
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
catView (State state) (CategoryInfo cat) =
  S.li
    ! P.key (cat.categoryInfoUid ^. _CUid) $ do
    S.a
      ! S.href (categoryDetailUrl state.categoryName cat.categoryInfoUid)
      $ S.text cat.categoryInfoTitle
