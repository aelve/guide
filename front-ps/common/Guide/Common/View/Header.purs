module Guide.Common.View.Header where

import Prelude

import Bulma.Columns.Columns (columns, column) as B
import Bulma.Columns.Size (PercentSize(..), isPercentSizeResponsive) as B
import Bulma.Common (Breakpoint(..), Is(..), Size(..), runClassName, runClassNames) as B
import Bulma.Elements.Button (button) as B
import Bulma.Elements.Title (title, subtitle, isSize) as B
import Bulma.Form.Common (isSize) as BF
import Bulma.Form.Input (input) as BF
import Bulma.Layout.Layout (level, levelItem, levelLeft, levelRight) as B
import Bulma.Modifiers.Typography (hasWeight, Weight(..)) as B
import Guide.CategoryOverview.Events (Event)
import Guide.CategoryOverview.State (State(..))
import Guide.Common.CSS.Global (header) as CSS
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, h1, a, input, section, span) as S
import Text.Smolder.HTML.Attributes (href, className, placeholder, type') as S
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (empty, text) as S

view :: State -> P.HTML Event
view st@(State state) =
  S.section
    ! S.className (B.runClassName CSS.header)
    $ do
      S.div
        ! S.className (B.runClassName B.level)
        $ do
          S.div
            ! S.className (B.runClassName B.levelLeft)
            $ S.div
              ! S.className (B.runClassName B.levelItem)
              $ S.h1
                  ! S.className (B.runClassNames
                                  [ B.title
                                  , B.isSize B.Is1
                                  ])
                  $ do
                    S.text "Aelve Guide |"
                    S.span
                      ! S.className (B.runClassNames
                                      [ B.subtitle
                                      , B.isSize B.Is2
                                      , B.hasWeight B.LightWeight
                                      ])
                      $ S.text " Haskell" -- TODO(sectore): Grap title from state
          S.div
            ! S.className (B.runClassName B.levelRight)
            $ S.div
              ! S.className (B.runClassName B.levelItem)
              $ S.a
                ! S.className (B.runClassName B.button)
                ! S.href "./auth"
                $ S.text "login"
      S.div
        $ S.input
            ! S.className (B.runClassNames
                            [ BF.input
                            , BF.isSize B.Large
                            ])
            ! S.placeholder "Search"
            ! S.type' "text"
      S.div
        ! S.className (B.runClassNames [B.columns])
        $ do
          S.div
            ! S.className (B.runClassNames
                          [ B.column
                          , BF.isSize B.Small
                          , B.isPercentSizeResponsive B.Half B.Tablet
                          , B.isPercentSizeResponsive B.OneThird B.Desktop
                          ])
            $ S.input
                ! S.className (B.runClassNames
                              [ BF.input
                              ])
                ! S.placeholder "Add category"
                ! S.type' "text"
          S.div
            ! S.className (B.runClassNames [B.column])
            $ S.empty
