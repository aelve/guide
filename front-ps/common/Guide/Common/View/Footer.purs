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
import Bulma.Core (runClazzName, runClazzNames) as Bulma
import Bulma.Modifiers.Typography (alignment, Alignment(..)) as Bulma
import Bulma.Modifiers.Responsive (showInlineFlex') as Bulma
import Bulma.Components.NavBar (navbar, navbarItem) as Bulma

view :: State -> P.HTML Event
view st@(State state) =
  S.div
    ! S.className (Bulma.runClazzNames
                    [ Bulma.footer
                    , Bulma.alignment Bulma.Centered
                    ])
    $ S.ul
      ! S.className (Bulma.runClazzNames
                      [ Bulma.navbar
                      , Bulma.showInlineFlex'
                      ])
      $ do
        S.li
          ! S.className (Bulma.runClazzName Bulma.navbarItem)
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
          ! S.className (Bulma.runClazzName Bulma.navbarItem)
          $ S.div $ do
            S.a
              ! S.href "https://github.com/aelve/guide"
              $ S.text "source"
            S.text "/"
            S.a
              ! S.href "https://github.com/aelve/guide/issues"
              $ S.text "issue tracker"
        S.li
          ! S.className (Bulma.runClazzName Bulma.navbarItem)
          $ S.a
              ! S.href "https://guide.aelve.com/unwritten-rules"
              $ S.text "rules"
        S.li
          ! S.className (Bulma.runClazzName Bulma.navbarItem)
          $ S.a
              ! S.href "https://guide.aelve.com/donate"
              $ S.text "donate"
        S.li
          ! S.className (Bulma.runClazzName Bulma.navbarItem)
          $ S.div $ do
            S.text "licensed under "
            S.a
              ! S.href "https://guide.aelve.com/license"
              $ S.text "CC+ BY-SA 4.0"
