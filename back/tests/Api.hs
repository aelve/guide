{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Api where

import BasePrelude hiding (Category)
import Data.Aeson
import qualified Data.Text             as T
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import Network.HTTP.Simple
import Control.Monad.Catch
import Network.HTTP.Types.Status

import Guide.Api.Types
import Guide.Types.Core
import Guide.Utils (Uid (..), Url)
import qualified Test.Hspec as H

apiTests = H.describe "api" $ do
  H.it "fail request" $ do
    request <- makeRequest
      (Host "http://localhost/fail")
      (Port 4400)
      (Method "GET")
    Status 404 "Not Found" <- runFailRequest request
    pure ()

  -- category tests
  H.it "get categories request" $ do
    [] <- getCategoriesRequest
    pure ()

  H.it "createCategory" $ void $ postCreateCategory

  H.it "get category by id" $ do
    -- get id of category from DB
    categoryInfo <- head <$> getCategoriesRequest
    let Uid categoryId = cciId categoryInfo
    request <- makeRequest
      (Host $ "http://localhost/category/" <> T.unpack categoryId)
      (Port 4400)
      (Method "GET")
    _ :: (Status, CCategoryFull) <- runRequest request
    pure ()
  H.it "delete category by id" $ do
    categoryInfo <- head <$> getCategoriesRequest
    Just True    <- deleteCategory (cciId categoryInfo)
    []           <- getCategoriesRequest
    pure ()
  H.it "modify notes of category" $ do
    categoryId <- postCreateCategory
    let Uid tCategoryId = categoryId 
    request <- makeRequest
      (Host $ "http://localhost/category/" <> T.unpack tCategoryId <> "/notes")
      (Port 4400)
      (Method "PUT")
    let req = setRequestBodyJSON (makeEditObject "" "string") request
    Status 200 _ <- runRequestNoBody req
    Status 409 _ <- runRequestNoBody req
    void $ deleteCategory categoryId
    Status 404 _ <- runRequestNoBody req
    pure ()
  H.it "modify info of category" $ do
    categoryId <- postCreateCategory
    let Uid tCategoryId = categoryId 
    request <- makeRequest
      (Host $ "http://localhost/category/" <> T.unpack tCategoryId <> "/info")
      (Port 4400)
      (Method "PUT")
    let req = setRequestBodyJSON makeEditCategoryInfo request
    Status 200 _ <- runRequestNoBody req
    void $ deleteCategory categoryId
    Status 404 _ <- runRequestNoBody req
    pure ()

makeEditCategoryInfo :: Value
makeEditCategoryInfo = object
  [ "title"    .= ("oldText" :: String)
  , "group"    .= ("Model" :: String)
  , "status"   .= ("CategoryStub" :: String)
  , "sections" .= [("ItemProsConsSection" :: String)]
  ]

makeEditObject :: String -> String -> Value
makeEditObject oldText newText = object
  [ "original" .= oldText
  , "modified" .= newText
  ]

getCategoriesRequest :: IO [CCategoryInfo]
getCategoriesRequest = do
  request <- makeRequest
    (Host "http://localhost/categories")
    (Port 4400)
    (Method "GET")
  snd <$> runRequest request

postCreateCategory :: IO (Uid Category)
postCreateCategory = do
  request <- makeRequest
    (Host "http://localhost/category?title=NewCategory&group=Model")
    (Port 4400)
    (Method "POST")
  snd <$> runRequest request

deleteCategory :: Uid Category -> IO (Maybe Bool)
deleteCategory (Uid categoryId) = do
  request <- makeRequest
    (Host $ "http://localhost/category/" <> T.unpack categoryId)
    (Port 4400)
    (Method "DELETE")
  res <- runRequestNoBody request
  pure $ case res of
    Status 200 _                    -> Just True
    Status 404 "Category not found" -> Just False
    _                               -> Nothing

runRequestNoBody, runFailRequest :: Request -> IO Status
runRequestNoBody request = getResponseStatus <$> httpNoBody request
runFailRequest           = runRequestNoBody

runRequest :: Yaml.FromJSON a => Request -> IO (Status, a)
runRequest request = do
  response <- httpJSON request
  pure (getResponseStatus response, getResponseBody response)

newtype Host   = Host String
newtype Port   = Port Int
newtype Method = Method S8.ByteString

makeRequest :: MonadThrow m => Host -> Port -> Method -> m Request
makeRequest (Host host) (Port port) (Method method) = do
  initReq <- parseRequest host
  pure $
    setRequestPort port $
    setRequestMethod method initReq