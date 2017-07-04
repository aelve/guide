module Guide.View.Playground where

import Prelude

import Data.Foldable (for_)
import Guide.Events (Event(..))
import Guide.Routes (Route(Home), toUrl)
import Guide.State (State(..))
import Guide.Types (User(..), Users(..))
import Network.RemoteData (RemoteData(..))
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (button, div, h2, a, ul, li) as S
import Text.Smolder.HTML.Attributes (href) as S
import Text.Smolder.Markup ((#!), (!))
import Text.Smolder.Markup (text) as S

playgroundView :: State -> P.HTML Event
playgroundView (State st) =
  S.div $ do
    S.h2 $ S.text "Playground"
    S.a ! S.href (toUrl Home)
        #! P.onClick (Navigate $ toUrl Home)
        $ S.text "Back to Home"
    S.button
      #! P.onClick (const RequestUsers)
      $ S.text "Re-load users!"
    case st.users of
      NotAsked -> S.div $ S.text "Users not asked."
      Loading -> S.div $ S.text "Loading users..."
      Failure error -> S.div $ S.text $ "Error loading users: " <> (show error)
      (Success users) -> usersView users
      (Refreshing users) ->
          S.div do
            S.div $ S.text "Refreshing users..."
            usersView users

usersView :: Users -> P.HTML Event
usersView (Users users) =
    S.ul
      $ for_ users userView

userView :: User -> P.HTML Event
userView (User user) =
    S.li
      $ S.text user.name
