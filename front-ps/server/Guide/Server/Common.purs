module Guide.Server.Common where

import Guide.Server.Types (PageConfig)
import Node.Express.Handler (Handler)
import Node.Express.Response (render)


renderPage :: forall eff . PageConfig -> Handler eff
renderPage config =
  -- TODO (sectore) Render a single Pux app for each page
  render "layout" config
