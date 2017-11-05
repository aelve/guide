module Guide.Common.View.Header where

import Prelude

import Guide.CategoryOverview.Events (Event)
import Guide.CategoryOverview.State (State(..))
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, h1, a, section, span) as S
import Text.Smolder.HTML.Attributes (href, className) as S
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as S

view :: State -> P.HTML Event
view st@(State state) =
  S.section
    ! S.className "hero"
    $ S.div
      ! S.className "hero-body"
      $ S.div
        ! S.className "navbar"
        $ S.div
          ! S.className "container" $ do
            S.div
              ! S.className "navbar-brand"
              $ S.div
                ! S.className "level" $ do
                  S.h1
                    ! S.className "title is-1" $ do
                      S.text "Guide |"
                      S.span
                        ! S.className "subtitle is-2 has-text-weight-light"
                        $ S.text " Haskell" -- TODO(sectore): Grap title from state
            S.div
              ! S.className "navbar-end"
              $ S.a
                ! S.className "button"
                ! S.href "./auth"
                $ S.text "login"
