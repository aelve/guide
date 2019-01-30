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
      (Method "GET")
    Status 404 "Not Found" <- runFailRequest request
    pure ()
  H.describe "Categories" $ do
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
        (Method "GET")
      (Status 200 "OK", _ :: CCategoryFull) <- runRequest request
      pure ()

    H.it "delete category by id" $ do
      categoryInfo <- head <$> getCategoriesRequest
      Just True    <- deleteCategory (cciId categoryInfo)
      Just False   <- deleteCategory (cciId categoryInfo)
      []           <- getCategoriesRequest
      pure ()

    H.it "modify notes of category" $ do
      req <- withCategory $ \categoryId -> do
        let Uid tCategoryId = categoryId 
        request <- makeRequest
          (Host $ "http://localhost/category/" <> T.unpack tCategoryId <> "/notes")
          (Method "PUT")
        let req = setRequestBodyJSON (makeEditObject "" "string") request
        Status 200 "OK"                      <- runRequestNoBody req
        Status 409 "Merge conflict occurred" <- runRequestNoBody req
        pure req
      Status 404 "Category not found"      <- runRequestNoBody req
      pure ()

    H.it "modify info of category" $ do
      req <- withCategory $ \categoryId -> do
        let Uid tCategoryId = categoryId 
        request <- makeRequest
          (Host $ "http://localhost/category/" <> T.unpack tCategoryId <> "/info")
          (Method "PUT")
        let req = setRequestBodyJSON makeEditCategoryInfo request
        Status 200 "OK"                 <- runRequestNoBody req
        pure req
      Status 404 "Category not found" <- runRequestNoBody req
      pure ()

  H.describe "Items" $ do
    H.it "create & delete item" $
      withCategory $ \categoryId -> do
        itemId      <- postCreateItem categoryId
        Just True   <- deleteItem itemId
        Just False  <- deleteItem itemId
        pure ()
  
    H.it "get item by id" $ do
      withCategory $ \categoryId -> do
        Uid itemId  <- postCreateItem categoryId
        request     <- makeRequest
          (Host $ "http://localhost/item/" <> T.unpack itemId)
          (Method "GET")
        (Status 200 "OK", _ :: CItemFull) <- runRequest request
        void $ deleteItem (Uid itemId)
        Status 404 "Item not found" <- runFailRequest request
        pure ()

withCategory :: (Uid Category -> IO a) -> IO a
withCategory f = do
  categoryId  <- postCreateCategory
  res <- f categoryId
  void $ deleteCategory categoryId
  pure res

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
    (Method "GET")
  snd <$> runRequest request

postCreateCategory :: IO (Uid Category)
postCreateCategory = do
  request <- makeRequest
    (Host "http://localhost/category?title=NewCategory&group=Model")
    (Method "POST")
  snd <$> runRequest request

deleteCategory :: Uid Category -> IO (Maybe Bool)
deleteCategory (Uid categoryId) = do
  request <- makeRequest
    (Host $ "http://localhost/category/" <> T.unpack categoryId)
    (Method "DELETE")
  res <- runRequestNoBody request
  pure $ case res of
    Status 200 "OK"                 -> Just True
    Status 404 "Category not found" -> Just False
    _                               -> Nothing

deleteItem :: Uid Item -> IO (Maybe Bool)
deleteItem (Uid itemId) = do
  request <- makeRequest
    (Host $ "http://localhost/item/" <> T.unpack itemId)
    (Method "DELETE")
  res <- runRequestNoBody request
  pure $ case res of
    Status 200 "OK"             -> Just True
    Status 404 "Item not found" -> Just False
    _                           -> Nothing

postCreateItem :: Uid Category -> IO (Uid Item)
postCreateItem (Uid categoryId) = do
  request <- makeRequest
    (Host $ "http://localhost/item/" <> T.unpack categoryId <> "?name=testName")
    (Method "POST")
  snd <$> runRequest request

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

makeRequest :: MonadThrow m => Host -> Method -> m Request
makeRequest (Host host) (Method method) = do
  initReq <- parseRequest host
  pure $
    setRequestPort 4400 $
    setRequestMethod method initReq