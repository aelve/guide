module Guide.Config where

type Config =
  { title :: String
  , public_path :: String
  , isProduction :: Boolean
  , isServer :: Boolean
  }

foreign import config :: Config
