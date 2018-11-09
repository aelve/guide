{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}


module Guide.Routes
(
  addRoute,
  adminRoute,
  authRoute,
  deleteRoute,
  haskellRoute,
  feedRoute,
  moveRoute,
  renderRoute,
  setRoute,
) where


import Web.Routing.Combinators (PathState (Open))
import Web.Spock (Path, (<//>))


haskellRoute :: Path '[] 'Open
haskellRoute = "haskell"

authRoute :: Path '[] 'Open
authRoute = "auth"

setRoute :: Path '[] 'Open
setRoute = haskellRoute <//> "set"

addRoute :: Path '[] 'Open
addRoute = haskellRoute <//> "add"

moveRoute :: Path '[] 'Open
moveRoute = haskellRoute <//> "move"

deleteRoute :: Path '[] 'Open
deleteRoute = haskellRoute <//> "delete"

feedRoute :: Path '[] 'Open
feedRoute = haskellRoute <//> "feed"

renderRoute :: Path '[] 'Open
renderRoute = haskellRoute <//> "render"

adminRoute :: Path '[] 'Open
adminRoute = "admin"
