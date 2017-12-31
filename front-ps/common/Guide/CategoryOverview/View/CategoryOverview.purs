module Guide.CategoryOverview.View.CategoryOverview where

import Prelude

import Bulma.Columns.Columns (column, columns, isMultiline) as B
import Bulma.Columns.Size (PercentSize(..), isPercentSize, isPercentSizeResponsive) as B
import Bulma.Common (Is(..), Breakpoint(..), runClassNames, runClassName) as B
import Bulma.Elements.Title (title, isSize) as B
import Data.Foldable (for_)
import Guide.Api.Types (CategoryInfo(..))
import Guide.CategoryOverview.Events (Event)
import Guide.CategoryOverview.State (State(..))
import Guide.Common.CSS.Global (catColumn, catLink, catHeadline, catSubheadline, catsWip, catsFinished, catsStub) as CSS
import Guide.Common.Routes (categoryDetailUrl)
import Guide.Common.Types (CCategories, unwrapUid)
import Network.RemoteData (RemoteData(..))
import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Text.Smolder.HTML (a, div, h2, p) as S
import Text.Smolder.HTML.Attributes (href, className) as S
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as S

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
    $ for_ cats (catView st)

catView :: State -> CategoryInfo -> P.HTML Event
catView (State state) (CategoryInfo cat) =
  S.div
    ! S.className (B.runClassNames
                    [ B.column
                    , B.isPercentSize B.Half
                    , B.isPercentSizeResponsive B.OneThird B.Desktop
                    , CSS.catColumn
                    ])
    ! P.key (unwrapUid cat.categoryInfoUid) $ do
      S.h2
        ! S.className (B.runClassNames
                        [ B.title
                        , B.isSize B.Is3
                        , CSS.catHeadline
                        ]
                      )
        $ S.text "Headline"
      S.div
        ! S.className (B.runClassName CSS.catsFinished)
        $ do
          S.a
            ! S.className (B.runClassName CSS.catLink)
            ! S.href (categoryDetailUrl state.categoryName cat.categoryInfoUid)
            $ S.text cat.categoryInfoTitle
          S.div
            ! S.className (B.runClassName CSS.catsWip)
            $ do
              S.p
                ! S.className (B.runClassNames
                                [ CSS.catSubheadline
                                ]
                              )
                $ S.text "In progress"
              S.p
                ! S.className (B.runClassName CSS.catLink)
                $ S.text "lorem ipsum"
          S.div
            ! S.className (B.runClassName CSS.catsStub)
            $ do
              S.p
                ! S.className (B.runClassNames
                                [ CSS.catSubheadline
                                ]
                              )
                $ S.text "To be written"
              S.p
                ! S.className (B.runClassName CSS.catLink)
                $ S.text "lorem ipsum"
