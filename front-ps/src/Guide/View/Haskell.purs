module Guide.View.Haskell where

import Prelude

import Data.Array (null)
import Data.Foldable (for_)
import Guide.Api.ClientTypes (CCategoryOverview(..), CGrandCategory(..))
import Guide.Events (Event(..))
import Guide.Routes (Route(..), toUrl)
import Guide.State (State(..))
import Guide.Types (CGrandCategories)
import Guide.Util.DOMUtil (mkKey)
import Network.RemoteData (RemoteData(..))
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Text.Smolder.HTML (div, h1, h2, h3, a, ul, li) as S
import Text.Smolder.HTML.Attributes (href) as S
import Text.Smolder.Markup ((#!), (!))
import Text.Smolder.Markup (text) as S

haskellView :: State -> P.HTML Event
haskellView (State st) =
  S.div $ do
    S.h1 $ S.text "Haskell"
    S.a ! S.href (toUrl Home)
        #! P.onClick (Navigate $ toUrl Home)
        $ S.text "Back to Home"
    case st.grandCategories of
      NotAsked -> S.div $ S.text "GrandCategories not asked."
      Loading -> S.div $ S.text "Loading data..."
      Failure error -> S.div $ S.text $ "Error loading data: " <> (show error)
      (Success cats) -> gCatsView cats
      (Refreshing cats) ->
          S.div do
            S.div $ S.text "Refreshing data..."
            gCatsView cats

gCatsView :: CGrandCategories -> P.HTML Event
gCatsView cats =
  S.ul
    $ for_ cats gCatView

gCatView :: CGrandCategory -> P.HTML Event
gCatView (CGrandCategory cat) =
  S.li
    ! P.key (mkKey cat.cgcTitle) $ do
    S.h2
      $ S.text cat.cgcTitle
    S.ul
      $ for_ cat.cgcFinished catOverviewView
    when (not null cat.cgcWip) $
      S.div $ do
        S.h3
          $ S.text "In progress"
        S.ul
          $ for_ cat.cgcWip catOverviewView
    when (not null cat.cgcStub) $
      S.div $ do
        S.h3
          $ S.text "To be written"
        S.ul
          $ for_ cat.cgcStub catOverviewView

catOverviewView :: CCategoryOverview -> P.HTML Event
catOverviewView (CCategoryOverview cat) =
  S.li
    ! P.key cat.ccoUid
    $ S.a ! S.href cat.ccoLink
            #! P.onClick (Navigate cat.ccoLink)
            $ S.text cat.ccoTitle
