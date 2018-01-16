module Guide.CategoryOverview.View.CategoryOverview where

import Prelude

import Bulma.Columns.Columns (column, columns, isMultiline) as B
import Bulma.Columns.Size (PercentSize(..), isPercentSize, isPercentSizeResponsive) as B
import Bulma.Common (Is(..), Breakpoint(..), runClassNames, runClassName) as B
import Bulma.Elements.Title (title, isSize) as B
import Data.Array (filter, head, null, snoc)
import Data.Foldable (foldl, for_)
import Data.Generic (gEq)
import Data.Lens.Getter ((^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, empty, insert, member, update)
import Data.Symbol (SProxy(..))
import Guide.Api.Types (CategoryInfo(..), _CategoryInfo)
import Guide.CategoryOverview.Events (Event)
import Guide.CategoryOverview.State (State(..))
import Guide.Common.CSS.Global (catColumn, catLink, catLinkWrapper, catHeadline, catSubheadline, catsWip, catsFinished, catsStub) as CSS
import Guide.Common.Routes (categoryDetailUrl)
import Guide.Common.Types (CCategories, unwrapUid)
import Guide.Types.Core (CategoryStatus(..))
import Network.RemoteData (RemoteData(..))
import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Text.Smolder.HTML (a, div, h2, li, p, ul) as S
import Text.Smolder.HTML.Attributes (href, className) as S
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as S
import Type.Data.Boolean (kind Boolean)

view :: State -> P.HTML Event
view st@(State state) =
  S.div
    $ case state.categories of
        NotAsked -> S.div $ S.text "Categories not asked."
        Loading -> S.div $ S.text "Loading data..."
        Failure error -> S.div $ S.text $ "Error loading data: " <> (show error)
        (Success cats) -> catsView st cats

catsView :: State -> CCategories -> P.HTML Event
catsView st@(State state) cats =
  S.div
    ! S.className (B.runClassNames
                    [ B.columns
                    , B.isMultiline
                    ])
    $ for_ (groupCats cats) (catGroupView st)
    where
      groupCats :: CCategories -> StrMap CCategories
      groupCats =
        foldl (\sm cat ->
                  if member (groupName cat) sm
                  then update (\cats' -> Just $ snoc cats' cat) (groupName cat) sm
                  else insert (groupName cat) [cat] sm
              ) empty
        where
          groupName cat = cat ^. (_CategoryInfo <<< prop (SProxy :: SProxy "categoryInfoGroup_"))

catGroupView :: State -> Array CategoryInfo -> P.HTML Event
catGroupView st groupedCats =
  S.div
    ! S.className (B.runClassNames
                    [ B.column
                    , B.isPercentSize B.Half
                    , B.isPercentSizeResponsive B.OneThird B.Desktop
                    , CSS.catColumn
                    ])
    ! P.key (groupKey groupedCats)
    $ do
      -- headline of category
      S.h2
        ! S.className (B.runClassNames
                        [ B.title
                        , B.isSize B.Is3
                        , CSS.catHeadline
                        ]
                      )
        $ S.text (maybe "" getTitle firstCat)
      -- finished categories
      unless (null catsFinished) $
        S.ul
          ! S.className (B.runClassName CSS.catsFinished)
          $ for_ catsFinished (catLinkView st)
      -- wip categories
      unless (null catsWip) $
        S.div
          ! S.className (B.runClassName CSS.catsWip)
          $ do
            S.p
              ! S.className (B.runClassName CSS.catSubheadline)
              $ S.text "In progress"
            S.ul
              $ for_ catsWip (catLinkView st)
      -- stub categories
      unless (null catsStub) $
        S.div
          ! S.className (B.runClassName CSS.catsStub)
          $ do
            S.p
              ! S.className (B.runClassName CSS.catSubheadline)
              $ S.text "To be written"
            S.ul
              $ for_ catsStub (catLinkView st)
      where
        firstCat :: Maybe CategoryInfo
        firstCat = head groupedCats

        getTitle :: CategoryInfo -> String
        getTitle cat =
          cat ^. (_CategoryInfo <<< prop (SProxy :: SProxy "categoryInfoGroup_"))

        -- Fold all UId's of a group to get a unique key (`String`)
        -- which is needed by React/Preact
        groupKey :: Array CategoryInfo -> String
        groupKey =
          foldl (\s (CategoryInfo cat) -> (unwrapUid cat.categoryInfoUid) <> s) ""

        filterCatsByStatus :: Array CategoryInfo -> CategoryStatus -> CCategories
        filterCatsByStatus cats status =
          filter (compareStatus status) cats
          where
            compareStatus :: CategoryStatus -> CategoryInfo -> Boolean
            compareStatus status' (CategoryInfo cat) =
              gEq status' (_.categoryInfoStatus cat)
              -- ^ We don't have an `Eq` instance of generated `CategoryStatus`
              -- However, `CategoryStatus` is derived by `Generic`
              -- so we can compare generically those by using `gEq`

        catsFinished = filterCatsByStatus groupedCats CategoryFinished
        catsWip = filterCatsByStatus groupedCats CategoryWIP
        catsStub = filterCatsByStatus groupedCats CategoryStub

catLinkView :: State -> CategoryInfo -> P.HTML Event
catLinkView (State state) (CategoryInfo cat) =
  S.li
    ! S.className (B.runClassName CSS.catLinkWrapper)
    $ S.a
      ! S.className (B.runClassName CSS.catLink)
      ! S.href (categoryDetailUrl state.categoryName cat.categoryInfoUid)
      $ S.text cat.categoryInfoTitle
