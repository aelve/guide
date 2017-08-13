{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}


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


import Web.Spock (Path, (<//>))
import Web.Routing.Combinators (PathState(Open))


haskellRoute :: Path '[] 'Open
haskellRoute = "haskell"

authRoute :: Path '[] 'Open
authRoute = haskellRoute <//> "auth"

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
adminRoute = haskellRoute <//> "admin"
