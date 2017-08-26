module Guide.Server.Types where

newtype PageConfig = PageConfig
  { contentId :: String
  , title :: String
  , catName :: String --
  , catDetailId :: String
  }
