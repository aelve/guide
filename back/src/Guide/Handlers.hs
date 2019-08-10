{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | All rest API handlers.
module Guide.Handlers
(
  methods,
  adminMethods,
  getLoggedInUser,
)
where

-- Shared imports
import Imports
-- Web
import Lucid hiding (for_)
import Web.Spock hiding (get, head, renderRoute, text)
import Web.Spock.Lucid
-- Site
import Guide.App
import Guide.Config
import Guide.Diff (merge)
import Guide.Markdown
import Guide.Routes
import Guide.ServerStuff
import Guide.State
import Guide.Types
import Guide.Utils
import Guide.Views

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Types as Feed
import qualified Text.Feed.Util as Feed
import qualified Web.Spock as Spock

methods :: GuideM ctx ()
methods = do
  renderMethods
  setMethods
  addMethods
  otherMethods

renderMethods :: GuideM ctx ()
renderMethods = do
  -- Notes for a category
  Spock.get (renderRoute <//> categoryVar <//> "notes") $ \catId -> do
    category <- dbQuery (GetCategory catId)
    lucidIO $ renderCategoryNotes category
  -- Item info
  Spock.get (renderRoute <//> itemVar <//> "info") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucidIO $ renderItemInfo category item
  -- Item description
  Spock.get (renderRoute <//> itemVar <//> "description") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    lucidIO $ renderItemDescription item
  -- Item ecosystem
  Spock.get (renderRoute <//> itemVar <//> "ecosystem") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    lucidIO $ renderItemEcosystem item
  -- Item notes
  Spock.get (renderRoute <//> itemVar <//> "notes") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucidIO $ renderItemNotes category item

setMethods :: GuideM ctx ()
setMethods = do
  Spock.post (setRoute <//> categoryVar <//> "info") $ \catId -> do
    -- TODO: [easy] add a cross-link saying where the form is handled in the
    -- code and other notes saying where stuff is rendered, etc
    title' <- T.strip <$> param' "title"
    group' <- T.strip <$> param' "group"
    prosConsEnabled'  <- (Just ("on" :: Text) ==) <$>
                         param "pros-cons-enabled"
    ecosystemEnabled' <- (Just ("on" :: Text) ==) <$>
                         param "ecosystem-enabled"
    notesEnabled'     <- (Just ("on" :: Text) ==) <$>
                         param "notes-enabled"
    status' <- do
      statusName :: Text <- param' "status"
      return $ case statusName of
        "finished" -> CategoryFinished
        "wip"      -> CategoryWIP
        "stub"     -> CategoryStub
        other      -> error ("unknown category status: " ++ show other)
    -- Modify the category
    -- TODO: actually validate the form and report errors
    unless (T.null title') $ do
      (edit, _) <- dbUpdate (SetCategoryTitle catId title')
      addEdit edit
    unless (T.null group') $ do
      (edit, _) <- dbUpdate (SetCategoryGroup catId group')
      addEdit edit
    do (edit, _) <- dbUpdate (SetCategoryStatus catId status')
       addEdit edit
    do oldEnabledSections <- categoryEnabledSections <$> dbQuery (GetCategory catId)
       let newEnabledSections = S.fromList . concat $
             [ [ItemProsConsSection  | prosConsEnabled']
             , [ItemEcosystemSection | ecosystemEnabled']
             , [ItemNotesSection     | notesEnabled'] ]
       (edit, _) <- dbUpdate $
                    ChangeCategoryEnabledSections catId
                      (newEnabledSections S.\\ oldEnabledSections)
                      (oldEnabledSections S.\\ newEnabledSections)
       addEdit edit
    -- After all these edits we can render the category header
    category <- dbQuery (GetCategory catId)
    lucidIO $ renderCategoryInfo category
  -- Notes for a category
  Spock.post (setRoute <//> categoryVar <//> "notes") $ \catId -> do
    original <- param' "original"
    content' <- param' "content"
    modified <- view (_categoryNotes.mdSource) <$> dbQuery (GetCategory catId)
    if modified == original
      then do
        (edit, category) <- dbUpdate (SetCategoryNotes catId content')
        addEdit edit
        lucidIO $ renderCategoryNotes category
      else do
        setStatus HTTP.status409
        json $ M.fromList [
          ("modified" :: Text, modified),
          ("merged" :: Text, merge original content' modified)]
  -- Item info
  Spock.post (setRoute <//> itemVar <//> "info") $ \itemId -> do
    -- TODO: [easy] add a cross-link saying where the form is handled in the
    -- code and other notes saying where stuff is rendered, etc
    name' <- T.strip <$> param' "name"
    link' <- T.strip <$> param' "link"
    hackage' <- (\x -> if T.null x then Nothing else Just x) . T.strip <$>
                param' "hackage"
    -- Modify the item
    -- TODO: actually validate the form and report errors
    unless (T.null name') $ do
      (edit, _) <- dbUpdate (SetItemName itemId name')
      addEdit edit
    case (T.null link', sanitiseUrl link') of
      (True, _) -> do
          (edit, _) <- dbUpdate (SetItemLink itemId Nothing)
          addEdit edit
      (_, Just l) -> do
          (edit, _) <- dbUpdate (SetItemLink itemId (Just l))
          addEdit edit
      _otherwise ->
          return ()
    do (edit, _) <- dbUpdate (SetItemHackage itemId hackage')
       addEdit edit
    -- After all these edits we can render the item
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucidIO $ renderItemInfo category item
  -- Item description
  Spock.post (setRoute <//> itemVar <//> "description") $ \itemId -> do
    original <- param' "original"
    content' <- param' "content"
    modified <- view (_itemSummary.mdSource) <$> dbQuery (GetItem itemId)
    if modified == original
      then do
        (edit, item) <- dbUpdate (SetItemSummary itemId content')
        addEdit edit
        lucidIO $ renderItemDescription item
      else do
        setStatus HTTP.status409
        json $ M.fromList [
          ("modified" :: Text, modified),
          ("merged" :: Text, merge original content' modified)]
  -- Item ecosystem
  Spock.post (setRoute <//> itemVar <//> "ecosystem") $ \itemId -> do
    original <- param' "original"
    content' <- param' "content"
    modified <- view (_itemEcosystem.mdSource) <$> dbQuery (GetItem itemId)
    if modified == original
      then do
        (edit, item) <- dbUpdate (SetItemEcosystem itemId content')
        addEdit edit
        lucidIO $ renderItemEcosystem item
      else do
        setStatus HTTP.status409
        json $ M.fromList [
          ("modified" :: Text, modified),
          ("merged" :: Text, merge original content' modified)]
  -- Item notes
  Spock.post (setRoute <//> itemVar <//> "notes") $ \itemId -> do
    original <- param' "original"
    content' <- param' "content"
    modified <- view (_itemNotes.mdSource) <$> dbQuery (GetItem itemId)
    if modified == original
      then do
        (edit, item) <- dbUpdate (SetItemNotes itemId content')
        addEdit edit
        category <- dbQuery (GetCategoryByItem itemId)
        lucidIO $ renderItemNotes category item
      else do
        setStatus HTTP.status409
        json $ M.fromList [
          ("modified" :: Text, modified),
          ("merged" :: Text, merge original content' modified)]
  -- Trait
  Spock.post (setRoute <//> itemVar <//> traitVar) $ \itemId traitId -> do
    original <- param' "original"
    content' <- param' "content"
    modified <- view (_traitContent.mdSource) <$> dbQuery (GetTrait itemId traitId)
    if modified == original
      then do
        (edit, trait) <- dbUpdate (SetTraitContent itemId traitId content')
        addEdit edit
        lucidIO $ renderTrait itemId trait
      else do
        setStatus HTTP.status409
        json $ M.fromList [
          ("modified" :: Text, modified),
          ("merged" :: Text, merge original content' modified)]

addMethods :: GuideM ctx ()
addMethods = do
  -- New category
  Spock.post (addRoute <//> "category") $ do
    title' <- param' "content"
    catId <- randomShortUid
    time <- liftIO getCurrentTime
    (edit, newCategory) <- dbUpdate (AddCategory catId title' "Miscellaneous" time)
    addEdit edit
    -- Return the URL of the new category
    Spock.text ("/haskell/" <> categorySlug newCategory)

  -- New item in a category
  Spock.post (addRoute <//> categoryVar <//> "item") $ \catId -> do
    name' <- param' "name"
    -- TODO: do something if the category doesn't exist (e.g. has been
    -- already deleted)
    itemId <- randomShortUid
    time <- liftIO getCurrentTime
    (edit, newItem) <- dbUpdate (AddItem catId itemId name' time)
    addEdit edit
    category <- dbQuery (GetCategory catId)
    lucidIO $ renderItem category newItem
  -- Pro (argument in favor of an item)
  Spock.post (addRoute <//> itemVar <//> "pro") $ \itemId -> do
    content' <- param' "content"
    traitId <- randomLongUid
    (edit, newTrait) <- dbUpdate (AddPro itemId traitId content')
    addEdit edit
    lucidIO $ renderTrait itemId newTrait
  -- Con (argument against an item)
  Spock.post (addRoute <//> itemVar <//> "con") $ \itemId -> do
    content' <- param' "content"
    traitId <- randomLongUid
    (edit, newTrait) <- dbUpdate (AddCon itemId traitId content')
    addEdit edit
    lucidIO $ renderTrait itemId newTrait

otherMethods :: GuideM ctx ()
otherMethods = do
  -- # Moving things
  -- Move item
  Spock.post (moveRoute <//> itemVar) $ \itemId -> do
    direction :: Text <- param' "direction"
    edit <- dbUpdate (MoveItem itemId (direction == "up"))
    addEdit edit
  Spock.post (moveRoute <//> itemVar <//> traitVar) $ \itemId traitId -> do
    direction :: Text <- param' "direction"
    edit <- dbUpdate (MoveTrait itemId traitId (direction == "up"))
    addEdit edit

  -- # Deleting things
  -- Delete category
  Spock.post (deleteRoute <//> categoryVar) $ \catId -> do
    mbEdit <- dbUpdate (DeleteCategory catId)
    mapM_ addEdit mbEdit
  -- Delete item
  Spock.post (deleteRoute <//> itemVar) $ \itemId -> do
    mbEdit <- dbUpdate (DeleteItem itemId)
    mapM_ addEdit mbEdit
  -- Delete trait
  Spock.post (deleteRoute <//> itemVar <//> traitVar) $ \itemId traitId ->  do
    mbEdit <- dbUpdate (DeleteTrait itemId traitId)
    mapM_ addEdit mbEdit

  -- # Feeds
  -- TODO: this link shouldn't be absolute [absolute-links]
  baseUrl <- (// "haskell") . _baseUrl <$> getConfig

  -- Feed for items in a category
  Spock.get (feedRoute <//> categoryVar) $ \catId -> do
    category <- dbQuery (GetCategory catId)
    let sortedItems = sortBy (flip cmp) (categoryItems category)
          where cmp = comparing itemCreated <> comparing itemUid
    let route = "feed" <//> categoryVar
    let feedUrl = baseUrl // Spock.renderRoute route (categoryUid category)
        feedTitle = Atom.TextString (categoryTitle category <> " – Haskell – Aelve Guide")
        feedLastUpdate = case sortedItems of
          item:_ -> Feed.toFeedDateStringUTC Feed.AtomKind (itemCreated item)
          _      -> ""
    let feedBase = Atom.nullFeed feedUrl feedTitle (toText feedLastUpdate)
    entries <- liftIO $ mapM (itemToFeedEntry baseUrl category) sortedItems
    atomFeed $ feedBase {
      Atom.feedEntries = entries,
      Atom.feedLinks   = [Atom.nullLink feedUrl] }

adminMethods :: AdminM ctx ()
adminMethods = do
  -- Accept an edit
  Spock.post (adminRoute <//> "edit" <//> var <//> "accept") $ \n -> do
    _ <- dbUpdate (RemovePendingEdit n)
    return ()
  -- Undo an edit
  Spock.post (adminRoute <//> "edit" <//> var <//> "undo") $ \n -> do
    (edit, _) <- dbQuery (GetEdit n)
    res <- undoEdit edit
    case res of
      Left err -> Spock.text (toText err)
      Right () -> do
                     _ <- dbUpdate (RemovePendingEdit n)
                     Spock.text ""
  -- Accept a range of edits
  Spock.post (adminRoute <//> "edits" <//> var <//> var <//> "accept") $ \m n -> do
    dbUpdate (RemovePendingEdits m n)
  -- Undo a range of edits
  Spock.post (adminRoute <//> "edits" <//> var <//> var <//> "undo") $ \m n -> do
    edits <- dbQuery (GetEdits m n)
    s <- dbQuery GetGlobalState
    failed <- fmap catMaybes $ for edits $ \(edit, details) -> do
      res <- undoEdit edit
      case res of
        Left err -> return (Just ((edit, details), Just err))
        Right () -> do
                       _ <- dbUpdate (RemovePendingEdit (editId details))
                       return Nothing
    case failed of
      [] -> Spock.text ""
      _  -> lucidIO $ renderEdits s failed
  -- Create a checkpoint
  Spock.post (adminRoute <//> "create-checkpoint") $ do
    db <- _db <$> Spock.getState
    createCheckpoint' db

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Retrieve the User based on the current session
getLoggedInUser :: GuideAction ctx (Maybe User)
getLoggedInUser = do
  sess <- readSession
  case sess ^. sessionUserID of
    Nothing   -> return Nothing
    Just uid' -> dbQuery $ GetUser uid'

itemToFeedEntry
  :: (MonadIO m)
  => Url -> Category -> Item -> m Atom.Entry
itemToFeedEntry baseUrl category item = do
  entryContent <- Lucid.renderTextT (renderItemForFeed category item)
  return entryBase {
    Atom.entryLinks = [Atom.nullLink entryLink],
    Atom.entryContent = Just (Atom.HTMLContent (toText entryContent)) }
  where
    entryLink = baseUrl //
                format "{}#item-{}" (categorySlug category) (itemUid item)
    entryBase = Atom.nullEntry
      (uidToText (itemUid item))
      (Atom.TextString (itemName item))
      (toText (Feed.toFeedDateStringUTC Feed.AtomKind (itemCreated item)))
