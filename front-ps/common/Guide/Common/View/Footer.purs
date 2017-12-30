module Guide.Common.View.Footer where

import Prelude

import Guide.CategoryOverview.Events (Event)
import Guide.CategoryOverview.State (State(..))
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, a, ul, li) as S
import Text.Smolder.HTML.Attributes (href, className) as S
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as S
import Bulma.Layout.Layout (footer) as Bulma
import Bulma.Common (runClassName, runClassNames) as Bulma
import Bulma.Modifiers.Typography (hasAlignment, Alignment(..)) as Bulma
import Bulma.Modifiers.Responsive (isInlineFlex) as Bulma
import Bulma.Components.Navbar (navbar, navbarItem) as Bulma

view :: State -> P.HTML Event
view st@(State state) =
  S.div
    ! S.className (Bulma.runClassNames
                    [ Bulma.footer
                    , Bulma.hasAlignment Bulma.Centered
                    ])
    $ S.ul
      ! S.className (Bulma.runClassNames
                      [ Bulma.navbar
                      , Bulma.isInlineFlex
                      ])
      $ do
        S.li
          ! S.className (Bulma.runClassName Bulma.navbarItem)
          $ S.div $ do
            S.text "made by "
            S.a
              ! S.href "https://artyom.me/"
              $ S.text "Artyom"
            S.text " & "
            S.a
              ! S.href "https://github.com/aelve/guide/graphs/contributors"
              $ S.text "others"
        S.li
          ! S.className (Bulma.runClassName Bulma.navbarItem)
          $ S.div $ do
            S.a
              ! S.href "https://github.com/aelve/guide"
              $ S.text "source"
            S.text "/"
            S.a
              ! S.href "https://github.com/aelve/guide/issues"
              $ S.text "issue tracker"
        S.li
          ! S.className (Bulma.runClassName Bulma.navbarItem)
          $ S.a
              ! S.href "https://guide.aelve.com/unwritten-rules"
              $ S.text "rules"
        S.li
          ! S.className (Bulma.runClassName Bulma.navbarItem)
          $ S.a
              ! S.href "https://guide.aelve.com/donate"
              $ S.text "donate"
        S.li
          ! S.className (Bulma.runClassName Bulma.navbarItem)
          $ S.div $ do
            S.text "licensed under "
            S.a
              ! S.href "https://guide.aelve.com/license"
              $ S.text "CC+ BY-SA 4.0"
