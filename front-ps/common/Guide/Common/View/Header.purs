module Guide.Common.View.Header where

import Prelude

import Guide.CategoryOverview.Events (Event)
import Guide.CategoryOverview.State (State(..))
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, h1, a, section, span) as S
import Text.Smolder.HTML.Attributes (href, className) as S
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as S
import Bulma.Common (Is(..), runClassName, runClassNames) as Bulma
import Bulma.Components.Navbar (navbar, navbarBrand, navbarEnd) as Bulma
import Bulma.Elements.Button (button) as Bulma
import Bulma.Elements.Title (title, subtitle, isSize) as Bulma
import Bulma.Layout.Layout (container, hero, heroBody, level) as Bulma
import Bulma.Modifiers.Typography (hasWeight, Weight(..)) as Bulma

view :: State -> P.HTML Event
view st@(State state) =
  S.section
    ! S.className (Bulma.runClassName Bulma.hero)
    $ S.div
      ! S.className (Bulma.runClassName Bulma.heroBody)
      $ S.div
        ! S.className (Bulma.runClassName Bulma.navbar)
        $ S.div
          ! S.className (Bulma.runClassName Bulma.container) $ do
            S.div
              ! S.className (Bulma.runClassName Bulma.navbarBrand)
              $ S.div
                ! S.className (Bulma.runClassName Bulma.level) $ do
                  S.h1
                    ! S.className (Bulma.runClassNames
                                    [ Bulma.title
                                    , Bulma.isSize Bulma.Is1
                                    ])
                    $ do
                      S.text "Guide |"
                      S.span
                        ! S.className (Bulma.runClassNames
                                        [ Bulma.subtitle
                                        , Bulma.isSize Bulma.Is2
                                        , Bulma.hasWeight Bulma.LightWeight
                                        ])
                        $ S.text " Haskell" -- TODO(sectore): Grap title from state
            S.div
              ! S.className (Bulma.runClassName Bulma.navbarEnd)
              $ S.a
                ! S.className (Bulma.runClassName Bulma.button)
                ! S.href "./auth"
                $ S.text "login"
