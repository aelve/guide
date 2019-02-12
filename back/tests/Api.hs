{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Integration tests for new API methods.
module Api (tests) where

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
import Guide.Utils (Uid (..))

import Test.Hspec


tests :: Spec
tests = describe "api" $ do
  it "fail request" $ do
    request <- makeRequest
      (Path "fail")
      (Method "GET")
    Status 404 "Not Found" <- runFailRequest request
    pure ()
  describe "Categories" $ do
    it "get categories request" $ void $ getCategoriesRequest
    it "createCategory"         $ void $ createCategory

    it "get category by id" $ do
      -- get id of category from DB
      categoryInfo <- head <$> getCategoriesRequest
      let Uid categoryId = cciId categoryInfo
      request <- makeRequest
        (Path $ "category/" <> T.unpack categoryId)
        (Method "GET")
      (Status 200 "OK", _ :: CCategoryFull) <- runRequest request
      pure ()

    it "delete category by id" $ do
      categoryInfo <- head <$> getCategoriesRequest
      Just True    <- deleteCategory (cciId categoryInfo)
      Just False   <- deleteCategory (cciId categoryInfo)
      pure ()

    it "modify notes of category" $ do
      req <- withCategory $ \categoryId -> do
        let Uid tCategoryId = categoryId
        request <- makeRequest
          (Path $ "category/" <> T.unpack tCategoryId <> "/notes")
          (Method "PUT")
        let req = setRequestBodyJSON (makeEditObject "" "string") request
        Status 200 "OK"                      <- runRequestNoBody req
        Status 409 "Merge conflict occurred" <- runRequestNoBody req
        pure req
      Status 404 "Category not found"      <- runRequestNoBody req
      pure ()

    it "modify info of category" $ do
      req <- withCategory $ \categoryId -> do
        let Uid tCategoryId = categoryId
        request <- makeRequest
          (Path $ "category/" <> T.unpack tCategoryId <> "/info")
          (Method "PUT")
        let req = setRequestBodyJSON editCategoryInfo request
        Status 200 "OK"                 <- runRequestNoBody req
        pure req
      Status 404 "Category not found" <- runRequestNoBody req
      pure ()

  describe "Items" $ do
    it "create & delete item" $
      withCategory $ \categoryId -> do
        itemId      <- createItem categoryId
        Just True   <- deleteItem itemId
        Just False  <- deleteItem itemId
        pure ()

    it "get item by id" $ do
      req <- withItem $ \(Uid itemId) -> do
        request     <- makeRequest
          (Path $ "item/" <> T.unpack itemId)
          (Method "GET")
        (Status 200 "OK", _ :: CItemFull) <- runRequest request
        pure request
      Status 404 "Item not found" <- runFailRequest req
      pure ()

    it "set item info" $ do
      req <- withItem $ \(Uid itemId) -> do
        request     <- makeRequest
          (Path $ "item/" <> T.unpack itemId <> "/info")
          (Method "PUT")
        let req = setRequestBodyJSON itemInfo request
        Status 200 "OK" <- runRequestNoBody req
        pure req
      Status 404 "Item not found" <- runFailRequest req
      pure ()
    forM_ ["summary", "ecosystem", "notes"] $ \dataType -> do
      it ("set " <> dataType <> " to item") $ setMergebleDataToItem dataType
  describe "Trait" $ do
    it "create & delete trait" $
      withItem $ \itemId -> do
        traitId     <- createTrait itemId
        Just True   <- deleteTrait itemId traitId
        Just False  <- deleteTrait itemId traitId
        pure ()
    it "get trait by id" $ do
          req <- withTrait $ \(Uid itemId) (Uid traitId) -> do
            request     <- makeRequest
              (Path $ "item/" <> T.unpack itemId <> "/trait/" <> T.unpack traitId)
              (Method "GET")
            (Status 200 "OK", _ :: CTrait) <- runRequest request
            pure request
          Status 404 "Item not found" <- runFailRequest req
          pure ()

    it "update trait" $ do
      req <- withTrait $ \(Uid itemId) (Uid traitId) -> do
        request     <- makeRequest
          (Path $ "item/" <> T.unpack itemId <> "/trait/" <> T.unpack traitId)
          (Method "PUT")
        let req = setRequestBodyJSON (makeEditObject "oldText" "newText") request
        Status 200 "OK"                       <- runRequestNoBody req
        Status 409 "Merge conflict occurred"  <- runRequestNoBody req
        pure req
      Status 404 "Item not found" <- runFailRequest req
      pure ()
    it "move trait" $ do
      req <- withTrait $ \(Uid itemId) (Uid traitId) -> do
        request     <- makeRequest
          (Path $ "item/" <> T.unpack itemId <> "/trait/" <> T.unpack traitId <> "/move")
          (Method "POST")
        let req = setRequestBodyJSON (object ["direction" .= ("up" :: String)]) request
        Status 200 "OK" <- runRequestNoBody req
        pure req
      Status 404 "Item not found" <- runFailRequest req
      pure ()

----------------------------------------------------------------------------
-- Category
----------------------------------------------------------------------------

withCategory :: (Uid Category -> IO a) -> IO a
withCategory f = do
  categoryId  <- createCategory
  res         <- f categoryId
  void $ deleteCategory categoryId
  pure res

createCategory :: IO (Uid Category)
createCategory = do
  request <- makeRequest
    (Path "category?title=NewCategory&group=Model")
    (Method "POST")
  snd <$> runRequest request

deleteCategory :: Uid Category -> IO (Maybe Bool)
deleteCategory (Uid categoryId) = do
  request <- makeRequest
    (Path $ "category/" <> T.unpack categoryId)
    (Method "DELETE")
  res <- runRequestNoBody request
  pure $ case res of
    Status 200 "OK"                 -> Just True
    Status 404 "Category not found" -> Just False
    _                               -> Nothing

editCategoryInfo :: Value
editCategoryInfo = object
  [ "title"    .= ("oldText" :: String)
  , "group"    .= ("Model" :: String)
  , "status"   .= ("CategoryStub" :: String)
  , "sections" .= [("ItemProsConsSection" :: String)]
  ]

getCategoriesRequest :: IO [CCategoryInfo]
getCategoriesRequest = do
  request <- makeRequest
    (Path "categories")
    (Method "GET")
  snd <$> runRequest request

----------------------------------------------------------------------------
-- Item
----------------------------------------------------------------------------

setMergebleDataToItem :: String -> IO ()
setMergebleDataToItem dataType = do
  req <- withItem $ \(Uid itemId) -> do
    request     <- makeRequest
      (Path $ "item/" <> T.unpack itemId <> "/" <> dataType)
      (Method "PUT")
    let req = setRequestBodyJSON (makeEditObject "" "text") request
    Status 200 "OK" <- runRequestNoBody req
    Status 409 "Merge conflict occurred" <- runRequestNoBody req
    pure req
  Status 404 "Item not found" <- runFailRequest req
  pure ()

withItem :: (Uid Item -> IO a) -> IO a
withItem f = withCategory $ \categoryId -> do
  itemId  <- createItem categoryId
  res     <- f itemId
  void $ deleteItem itemId
  pure res

createItem :: Uid Category -> IO (Uid Item)
createItem (Uid categoryId) = do
  request <- makeRequest
    (Path $ "item/" <> T.unpack categoryId <> "?name=testName")
    (Method "POST")
  snd <$> runRequest request

deleteItem :: Uid Item -> IO (Maybe Bool)
deleteItem (Uid itemId) = do
  request <- makeRequest
    (Path $ "item/" <> T.unpack itemId)
    (Method "DELETE")
  res <- runRequestNoBody request
  pure $ case res of
    Status 200 "OK"             -> Just True
    Status 404 "Item not found" -> Just False
    _                           -> Nothing

itemInfo :: Value
itemInfo = object
  [ "name"    .= ("exampleName"    :: String)
  , "group"   .= ("exampleGroup"   :: String)
  , "hackage" .= ("string"         :: String)
  , "link"    .= ("http:/link.exp" :: String)
  ]

----------------------------------------------------------------------------
-- Trait
----------------------------------------------------------------------------

createTrait :: Uid Item -> IO (Uid Trait)
createTrait (Uid itemId) = do
  request <- makeRequest
    (Path $ "item/" <> T.unpack itemId <> "/trait")
    (Method "POST")
  snd <$> (runRequest $ setRequestBodyJSON traitBody request)

deleteTrait :: Uid Item -> Uid Trait -> IO (Maybe Bool)
deleteTrait (Uid itemId) (Uid traitId) = do
  request <- makeRequest
    (Path $ "item/" <> T.unpack itemId <> "/trait/" <> T.unpack traitId)
    (Method "DELETE")
  res <- runRequestNoBody request
  pure $ case res of
    Status 200 "OK"               -> Just True
    Status 404 "Item not found"   -> Just False
    Status 404 "Trait not found"  -> Just False
    _                             -> Nothing

withTrait :: (Uid Item -> Uid Trait -> IO a) -> IO a
withTrait f = withItem $ \itemId -> do
  traitId <- createTrait itemId
  res     <- f itemId traitId
  void $ deleteTrait itemId traitId
  pure res

traitBody :: Value
traitBody = object
  [ "type"    .= ("Pro" :: String)
  , "content" .= ("oldText" :: String)
  ]

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

makeEditObject :: String -> String -> Value
makeEditObject oldText newText = object
  [ "original" .= oldText
  , "modified" .= newText
  ]

----------------------------------------------------------------------------
-- Utilities for requests
----------------------------------------------------------------------------

runRequestNoBody, runFailRequest :: Request -> IO Status
runRequestNoBody request = getResponseStatus <$> httpNoBody request
runFailRequest           = runRequestNoBody

runRequest :: Yaml.FromJSON a => Request -> IO (Status, a)
runRequest request = do
  response <- httpJSON request
  pure (getResponseStatus response, getResponseBody response)

newtype Path   = Path String
newtype Method = Method S8.ByteString

makeRequest :: MonadThrow m => Path -> Method -> m Request
makeRequest (Path path) (Method method) = do
  initReq <- parseRequest $ "http://localhost/" ++ path
  pure $
    setRequestPort 4400 $
    setRequestMethod method initReq
