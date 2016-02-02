{-# LANGUAGE
OverloadedStrings,
NoImplicitPrelude
  #-}


module Main (main) where


-- General
import BasePrelude
-- IO
import Control.Monad.IO.Class
-- Text
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
-- Web
import Lucid
import Web.Spock


main :: IO ()
main = runSpock 8080 $ spockT id $ do
  textVar <- liftIO $ newIORef ("" :: Text)
  get root $ do
    t <- liftIO $ readIORef textVar
    lucid $ do
      script_ "window.onload = function() {\
              \  var area = document.getElementsByName('text')[0];\
              \  area.addEventListener('input', function() {\
              \    var client = new XMLHttpRequest();\
              \    client.open('PUT', '/store', true);\
              \    client.setRequestHeader('Content-Type', 'text/plain');\
              \    client.send(area.value);}, false);};"
      with textarea_ [name_ "text"] (toHtml t)
  put "store" $ do
    t <- T.decodeUtf8 <$> body
    liftIO $ writeIORef textVar t

-- Utils

lucid :: Html a -> ActionT IO a
lucid = html . TL.toStrict . renderText
