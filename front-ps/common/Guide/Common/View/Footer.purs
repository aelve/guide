module Guide.Common.View.Footer where

import Prelude

import Bulma.Common (Breakpoint(..), runClassName, runClassNames) as B
import Bulma.Components.Navbar (navbar, navbarItem) as B
import Bulma.Layout.Layout (footer) as B
import Bulma.Modifiers.Responsive (isInlineFlexResponsive, isInlineBlockResponsive) as B
import Bulma.Modifiers.Typography (hasAlignment, Alignment(..)) as B
import Guide.CategoryOverview.Events (Event)
import Guide.CategoryOverview.State (State(..))
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, a, ul, li) as S
import Text.Smolder.HTML.Attributes (href, className) as S
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as S
import Guide.Common.CSS.Global (footerContainer) as CSS

view :: State -> P.HTML Event
view st@(State state) =
  S.div
    ! S.className (B.runClassNames
                    [ B.footer
                    , B.hasAlignment B.Centered
                    , CSS.footerContainer
                    ])
    $ S.ul
      ! S.className (B.runClassNames
                      [ B.navbar
                      , B.isInlineFlexResponsive B.Desktop
                      , B.isInlineBlockResponsive B.Mobile
                      ])
      $ do
        S.li
          ! S.className (B.runClassName B.navbarItem)
          $ S.div $ do
            S.text "made by "
            S.a
              ! S.href "https://artyom.me/"
              $ S.text "Artyom"
            S.text " & "
            S.a
              ! S.href "https://github.com/aelve/guide/graphs/contributors"
              $ S.text "contributors"
        S.li
          ! S.className (B.runClassName B.navbarItem)
          $ S.div $ do
            S.a
              ! S.href "https://github.com/aelve/guide"
              $ S.text "source"
            S.text "/"
            S.a
              ! S.href "https://github.com/aelve/guide/issues"
              $ S.text "issue tracker"
        S.li
          ! S.className (B.runClassName B.navbarItem)
          $ S.a
              ! S.href "https://guide.aelve.com/unwritten-rules"
              $ S.text "rules"
        S.li
          ! S.className (B.runClassName B.navbarItem)
          $ S.a
              ! S.href "https://guide.aelve.com/donate"
              $ S.text "donate"
        S.li
          ! S.className (B.runClassName B.navbarItem)
          $ S.div $ do
            S.text "licensed under "
            S.a
              ! S.href "https://guide.aelve.com/license"
              $ S.text "CC+ BY-SA 4.0"
