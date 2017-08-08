{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Guide.Routes
  ( apiRoute
  , addRoute
  , adminRoute
  , authRoute
  , deleteRoute
  , haskellRoute
  , feedRoute
  , moveRoute
  , renderRoute
  , setRoute
  ) where

import Web.Spock (Path)
import Web.Routing.Combinators (PathState(Open))

apiRoute :: Path '[] 'Open
apiRoute = "api"

haskellRoute :: Path '[] 'Open
haskellRoute = "haskell"

authRoute :: Path '[] 'Open
authRoute = "auth"

setRoute :: Path '[] 'Open
setRoute = "set"

addRoute :: Path '[] 'Open
addRoute = "add"

moveRoute :: Path '[] 'Open
moveRoute = "move"

deleteRoute :: Path '[] 'Open
deleteRoute = "delete"

feedRoute :: Path '[] 'Open
feedRoute = "feed"

renderRoute :: Path '[] 'Open
renderRoute = "render"

adminRoute :: Path '[] 'Open
adminRoute = "admin"
