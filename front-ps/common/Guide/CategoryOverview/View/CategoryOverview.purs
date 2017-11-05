module Guide.CategoryOverview.View.CategoryOverview where

import Prelude

import Data.Foldable (for_)
import Guide.Api.Types (CategoryInfo(..))
import Guide.CategoryOverview.Events (Event)
import Guide.CategoryOverview.State (State(..))
import Guide.Common.Routes (categoryDetailUrl)
import Guide.Common.Types (CCategories, unwrapUid)
import Network.RemoteData (RemoteData(..))
import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Text.Smolder.HTML (div, a) as S
import Text.Smolder.HTML.Attributes (href, className) as S
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as S

view :: State -> P.HTML Event
view st@(State state) =
  S.div
    ! S.className "content"
    $ case state.categories of
        NotAsked -> S.div $ S.text "Categories not asked."
        Loading -> S.div $ S.text "Loading data..."
        Failure error -> S.div $ S.text $ "Error loading data: " <> (show error)
        (Success cats) -> catsView st cats

catsView :: State -> CCategories -> P.HTML Event
catsView st@(State state) cats =
  S.div
    ! S.className "columns is-multiline"
    $ for_ cats (catView st)

catView :: State -> CategoryInfo -> P.HTML Event
catView (State state) (CategoryInfo cat) =
  S.div
    ! S.className "column is-one-third-desktop is-half"
    ! P.key (unwrapUid cat.categoryInfoUid) $ do
    S.a
      ! S.className "subtitle is-4"
      ! S.href (categoryDetailUrl state.categoryName cat.categoryInfoUid)
      $ S.text cat.categoryInfoTitle
