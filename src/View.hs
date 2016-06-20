{-# LANGUAGE
QuasiQuotes,
OverloadedStrings,
FlexibleContexts,
ViewPatterns,
RecordWildCards,
TupleSections,
NoImplicitPrelude
  #-}


module View
(
  -- * Pages
  renderRoot,
  renderAdmin,
  renderEdits,
  renderHaskellRoot,
  renderDonate,
  renderCategoryPage,
  renderStaticMd,
  renderSearchResults,

  -- * Methods
  -- ** Categories
  renderCategoryList,
  renderCategory,
  renderCategoryInfo,
  renderCategoryNotes,
  -- ** Items
  renderItem,
  renderItemInfo,
  renderItemDescription,
  renderItemEcosystem,
  renderItemTraits,
  renderItemNotes,
  -- ** Traits
  renderTrait,

  -- * Rendering for feeds
  renderItemForFeed,

  -- * Miscellaneous
  getItemHue,
  newGroupValue,
)
where


-- General
import BasePrelude hiding (Category)
-- Default
import Data.Default
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Monads and monad transformers
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Random
-- Containers
import qualified Data.Map as M
import Data.Tree
-- Text
import qualified Data.Text.All as T
import Data.Text.All (Text)
import NeatInterpolation
-- Web
import Lucid hiding (for_)
-- Network
import Data.IP
-- Time
import Data.Time
import Data.Time.Format.Human
-- Markdown
import Cheapskate.Lucid
import Cheapskate.Types

-- Local
import Config
import Types
import Utils
import JS (JS(..), JQuerySelector)
import qualified JS
import Markdown
import Cache


{- Note [autosize]
~~~~~~~~~~~~~~~~~~

All textareas on the site are autosized – i.e. they grow when the user is typing. This is done by the autosize.js plugin, which is called on page load:

    autosize($('textarea'));

A slight problem is that it doesn't compute the height of hidden elements correctly – thus, when something is shown and it happens to be a textarea or contain a textarea, we have to call autosize again. This is done in 'JS.switchSection'. So far there are no textboxes that are shown *without* switchSection being involved, and so there's no need to watch for elements being added to the DOM.

It would be nicer if we could watch for elements becoming visible without having to modify switchSection, but there doesn't seem to be an easy way to do this – MutationObserver doesn't let us find out when something becomes visible (i.e. when its clientHeight stops being 0).

In switchSection we use

    autosize($('textarea'));
    autosize.update($('textarea'));

instead of simple

    autosize.update($('textarea'));

– this is done because the textarea could have appeared after the original `autosize($('textarea'));` was called on page load (which could happen if an item was added, for instance).

-}

{- Note [show-hide]
~~~~~~~~~~~~~~~~~~~

A lot of things (notes, etc) can be expanded/collapsed by pressing a button. Similarly, pressing “edit” replaces rendered text with a textbox, or adds buttons to pros/cons. All this is done with sections and show/hide.

A section is something that can be shown or hidden. You define a section by using 'section' (which creates a <div>) or 'sectionSpan' (which creates a <span>).

    section "normal" [shown, noScriptShown] $ do
      renderText
      ...

    section "editing" [] $ do
      renderEditbox
      ...

You can even give 2 names to a section – e.g. "normal editing" if you want the section be visible both in “normal” mode and in “editing” mode.

The list parameter is used to add attributes to the section. 'shown' is an attribute that means that the section is normally visible; 'noScriptShown' means that the section will be visible when Javascipt is disabled. Sections without either attribute will be hidden. (Usually 'shown' and 'noScriptShown' go together, but not always.)

When several sections are in the same container (e.g. a <div>), you can toggle between them with 'JS.switchSection', which shows the section (or several sections) with given name, and hides all sections with other names. The elements that aren't sections are not affected.

Also, there's another function available – 'JS.switchSectionEverywhere' – that switches sections everywhere inside the container, not only among container's direct children. It's useful when you have something like a list of pros/cons and you want to switch them all into the “editable” state.

////////////////////////////////////

And now, here's how it's all implemented.

In 'wrapPage' there's a piece of CSS wrapped in <noscript> that hides everything except for 'noScriptShown' things:

    .section:not(.noscript-shown) {display:none;}

There's also a piece of Javascript that, when executed, will change it to the following CSS:

    .section:not(.shown) {display:none;}

So, if Javascript is disabled we hide all sections except for those that have the 'noScriptShown' attribute, and if it's enabled we hide all sections except for those that have the 'shown' attribute.

After that switching sections is simply done by adding/removing the “shown” class. (Note that we don't have to choose between “noscript-shown” and “shown” because switching sections is *only* possible if Javascript is enabled, and in this case the relevant tag will always be “shown” and not “noscript-shown”.)

-}

renderRoot :: (MonadIO m, MonadRandom m, MonadReader Config m) => HtmlT m ()
renderRoot = do
  wrapPage "Aelve Guide" $ do
    h1_ "Aelve Guide"
    h2_ (mkLink "Haskell" "/haskell")

-- TODO: show a “category not found” page

renderAdmin
  :: (MonadIO m, MonadRandom m) => GlobalState -> HtmlT m ()
renderAdmin globalState = do
  head_ $ do
    includeJS "/js.js"
    includeJS "/jquery.js"
    includeJS "/sorttable.js"
    includeCSS "/markup.css"
    includeCSS "/admin.css"
    includeCSS "/loader.css"
    title_ "admin – Aelve Guide"
    meta_ [name_ "viewport",
           content_ "width=device-width, initial-scale=1.0, user-scalable=yes"]

  body_ $ do
    script_ $ fromJS $ JS.createAjaxIndicator ()
    h1_ "Miscellaneous"
    buttonUid <- randomLongUid
    button "Create checkpoint" [uid_ buttonUid] $
      JS.createCheckpoint [JS.selectUid buttonUid]
    div_ [id_ "stats"] $
      renderStats globalState (globalState ^. actions)
    div_ [id_ "edits"] $
      renderEdits globalState (map (,Nothing) (globalState ^. pendingEdits))

renderStats
  :: (MonadIO m)
  => GlobalState
  -> [(Action, ActionDetails)]
  -> HtmlT m ()
renderStats globalState acts = do
  h1_ "Statistics"
  p_ "All information is for last 31 days."
  now <- liftIO getCurrentTime
  let thisMonth (_, d) = diffUTCTime now (actionDate d) <= 31*86400
      acts' = takeWhile thisMonth acts
  p_ $ do
    "Main page visits: "
    strong_ $ toHtml $ show $ length [() | (Action'MainPageVisit, _) <- acts']
    ". "
    "Edits: "
    strong_ $ toHtml $ show $ length [() | (Action'Edit _, _) <- acts']
    ". "
    "Unique visitors: "
    strong_ $ toHtml $ show $ length $ ordNub $ map (actionIP.snd) acts'
    "."
  let allCategories = globalState^.categories ++
                      globalState^.categoriesDeleted
  -- TODO: move this somewhere else (it's also used in renderEdit)
  let findCategory catId = fromMaybe err (find (hasUid catId) allCategories)
        where
          err = error ("renderStats: couldn't find category with uid = " ++
                       T.unpack (uidToText catId))
  table_ [class_ "sortable"] $ do
    thead_ $ tr_ $ do
      th_ [class_ "sorttable_nosort"] "Category"
      th_ "Visits"
      th_ "Unique visitors"
    tbody_ $ do
      let rawVisits :: [(Uid Category, Maybe IP)]
          rawVisits = [(catId, actionIP d) |
                       (Action'CategoryVisit catId, d) <- acts']
      let visits :: [(Uid Category, (Int, Int))]
          visits = map (over _2 (length &&& length.ordNub)) .
                   map (fst.head &&& map snd) .
                   groupWith fst
                     $ rawVisits
      for_ (reverse $ sortWith (fst.snd) visits) $ \(catId, (n, u)) -> do
        tr_ $ do
          td_ (toHtml (findCategory catId ^. title))
          td_ (toHtml (show n))
          td_ (toHtml (show u))
  table_ [class_ "sortable"] $ do
    thead_ $ tr_ $ do
      th_ [class_ "sorttable_nosort"] "Search"
      th_ "Repetitions"
    tbody_ $ do
      let searches = map (head &&& length) . group $
            [s | (Action'Search s, _) <- acts']
      for_ (reverse $ sortWith snd searches) $ \(s, n) -> do
        tr_ $ do
          td_ (toHtml s)
          td_ (toHtml (show n))
  table_ [class_ "sortable"] $ do
    thead_ $ tr_ $ do
      th_ [class_ "sorttable_nosort"] "Referrer"
      th_ "Visitors"
      th_ "Unique visitors"
    tbody_ $ do
      let rawVisits :: [(Url, Maybe IP)]
          rawVisits = [(r, actionIP d) |
                       (_, d) <- acts',
                       Just (ExternalReferrer r) <- [actionReferrer d]]
      let visits :: [(Url, (Int, Int))]
          visits = map (over _2 (length &&& length.ordNub)) .
                   map (fst.head &&& map snd) .
                   groupWith fst
                     $ rawVisits
      for_ (reverse $ sortWith (fst.snd) visits) $ \(r, (n, u)) -> do
        tr_ $ do
          td_ (toHtml r)
          td_ (toHtml (show n))
          td_ (toHtml (show u))
  table_ $ do
    thead_ $ tr_ $ do
      th_ "Action"
      th_ "Date"
      th_ "IP"
    tbody_ $ do
      -- acts, not acts' (what if there were less than 10 actions in the last
      -- month?)
      for_ (take 10 acts) $ \(a, d) -> tr_ $ do
        td_ $ case a of
          Action'Edit _          -> "Edit"
          Action'MainPageVisit   -> "Main page visit"
          Action'CategoryVisit _ -> "Category visit"
          Action'Search _        -> "Search"
        td_ $ toHtml =<< liftIO (humanReadableTime (actionDate d))
        td_ $ case actionIP d of
          Nothing -> "<unknown IP>"
          Just ip -> toHtml (show ip)

-- TODO: when showing Edit'DeleteCategory, show the amount of items in that
-- category and titles of items themselves

-- | Group edits by IP and render them
renderEdits
  :: (MonadIO m, MonadRandom m)
  => GlobalState
  -> [((Edit, EditDetails), Maybe String)]
  -> HtmlT m ()
renderEdits globalState edits = do
  let getIP = editIP . snd . fst
  -- Unlike 'groupWith', “groupBy . equating” doesn't sort the input.
  let editBlocks = groupBy (equating getIP) edits
  let ipNum = length $ groupWith getIP edits
  h1_ $ toHtml $
    T.format "Pending edits (IPs: {}, blocks: {})" (ipNum, length editBlocks)
  for_ editBlocks $ \editBlock -> div_ $ do
    blockNode <- thisNode
    h2_ $ do
      case editIP (editBlock ^?! _head._1._2) of
        Nothing -> "<unknown IP>"
        Just ip -> toHtml (show ip)
      emptySpan "1em"
      textButton "accept all" $
        JS.acceptBlock (editId (editBlock ^?! _head._1._2),
                        editId (editBlock ^?! _last._1._2),
                        blockNode)
      emptySpan "0.5em"
      textButton "undo all" $
        JS.undoBlock (editId (editBlock ^?! _head._1._2),
                      editId (editBlock ^?! _last._1._2),
                      blockNode)
    for_ editBlock $ \((edit, EditDetails{..}), mbErr) ->
      div_ [class_ "edit"] $ do
        editNode <- thisNode
        p_ [class_ "edit-info"] $ do
          toHtml =<< liftIO (humanReadableTime editDate)
          emptySpan "1em"
          textButton "accept" $
            JS.acceptEdit (editId, editNode)
          emptySpan "0.5em"
          textButton "try to undo" $
            JS.undoEdit (editId, editNode)
        case mbErr of
          Nothing  -> return ()
          Just err -> p_ $ span_ [style_ "background-color:#E57373"] $
            "Can't apply the edit: " >> toHtml err
        renderEdit globalState edit

renderEdit :: Monad m => GlobalState -> Edit -> HtmlT m ()
renderEdit globalState edit = do
  let quote :: Monad m => HtmlT m () -> HtmlT m ()
      quote a = "“" *> a <* "”"
  -- We're searching for everything (items/categories) both in normal lists
  -- and in lists of deleted things. Just in case.
  let allCategories = globalState^.categories ++
                      globalState^.categoriesDeleted
  let findCategory catId = fromMaybe err (find (hasUid catId) allCategories)
        where
          err = error ("renderEdit: couldn't find category with uid = " ++
                       T.unpack (uidToText catId))
  let findItem itemId = (category, item)
        where
          getItems = view (items <> itemsDeleted)
          ourCategory = any (hasUid itemId) . getItems
          err = error ("renderEdit: couldn't find item with uid = " ++
                       T.unpack (uidToText itemId))
          category = fromMaybe err (find ourCategory allCategories)
          item = fromJust (find (hasUid itemId) (getItems category))
  let findTrait itemId traitId = (category, item, trait)
        where
          (category, item) = findItem itemId
          getTraits = view (cons <> consDeleted <> pros <> prosDeleted)
          err = error ("renderEdit: couldn't find trait with uid = " ++
                       T.unpack (uidToText traitId))
          trait = fromMaybe err (find (hasUid traitId) (getTraits item))

  let printCategory catId = do
        let category = findCategory catId
        quote $ toHtml (category ^. title)
  let printItem itemId = do
        let (_, item) = findItem itemId
        quote $ toHtml (item ^. name)

  case edit of
    -- Add
    Edit'AddCategory _catId title' -> p_ $ do
      "added category " >> quote (toHtml title')
    Edit'AddItem catId _itemId name' -> p_ $ do
      "added item " >> quote (toHtml name')
      " to category " >> printCategory catId
    Edit'AddPro itemId _traitId content' -> do
      p_ $ "added pro to item " >> printItem itemId
      blockquote_ $ p_ $ toHtml (renderMarkdownInline content')
    Edit'AddCon itemId _traitId content' -> do
      p_ $ "added con to item " >> printItem itemId
      blockquote_ $ p_ $ toHtml (renderMarkdownInline content')

    -- Change category properties
    Edit'SetCategoryTitle _catId oldTitle newTitle -> p_ $ do
      "changed title of category " >> quote (toHtml oldTitle)
      " to " >> quote (toHtml newTitle)
    Edit'SetCategoryGroup catId oldGroup newGroup -> p_ $ do
      "changed group of category " >> printCategory catId
      " from " >> quote (toHtml oldGroup)
      " to "   >> quote (toHtml newGroup)
    Edit'SetCategoryStatus catId oldStatus newStatus -> p_ $ do
      "changed status of category " >> printCategory catId
      " from " >> quote (toHtml (show oldStatus))
      " to "   >> quote (toHtml (show newStatus))
    Edit'SetCategoryNotes catId oldNotes newNotes -> do
      p_ $ "changed notes of category " >> printCategory catId
      table_ $ tr_ $ do
        td_ $ blockquote_ $ toHtml (renderMarkdownBlock oldNotes)
        td_ $ blockquote_ $ toHtml (renderMarkdownBlock newNotes)
    Edit'SetCategoryProsConsEnabled catId _oldVal newVal -> do
      if newVal == True
        then p_ $ "enabled pros/cons for category " >> printCategory catId
        else p_ $ "disabled pros/cons for category " >> printCategory catId
    Edit'SetCategoryEcosystemEnabled catId _oldVal newVal -> do
      if newVal == True
        then p_ $ "enabled ecosystem for category " >> printCategory catId
        else p_ $ "disabled ecosystem for category " >> printCategory catId

    -- Change item properties
    Edit'SetItemName _itemId oldName newName -> p_ $ do
      "changed name of item " >> quote (toHtml oldName)
      " to " >> quote (toHtml newName)
    Edit'SetItemLink itemId oldLink newLink -> p_ $ do
      "changed link of item " >> printItem itemId
      " from " >> code_ (toHtml (show oldLink))
      " to "   >> code_ (toHtml (show newLink))
    Edit'SetItemGroup itemId oldGroup newGroup -> p_ $ do
      "changed group of item " >> printItem itemId
      " from " >> code_ (toHtml (show oldGroup))
      " to "   >> code_ (toHtml (show newGroup))
    Edit'SetItemKind itemId oldKind newKind -> p_ $ do
      "changed kind of item " >> printItem itemId
      " from " >> code_ (toHtml (show oldKind))
      " to "   >> code_ (toHtml (show newKind))
    Edit'SetItemDescription itemId oldDescr newDescr -> do
      p_ $ "changed description of item " >> printItem itemId
      table_ $ tr_ $ do
        td_ $ blockquote_ $ toHtml (renderMarkdownBlock oldDescr)
        td_ $ blockquote_ $ toHtml (renderMarkdownBlock newDescr)
    Edit'SetItemNotes itemId oldNotes newNotes -> do
      p_ $ "changed notes of item " >> printItem itemId
      table_ $ tr_ $ do
        td_ $ blockquote_ $ toHtml (renderMarkdownBlock oldNotes)
        td_ $ blockquote_ $ toHtml (renderMarkdownBlock newNotes)
    Edit'SetItemEcosystem itemId oldEcosystem newEcosystem -> do
      p_ $ "changed ecosystem of item " >> printItem itemId
      table_ $ tr_ $ do
        td_ $ blockquote_ $ toHtml (renderMarkdownBlock oldEcosystem)
        td_ $ blockquote_ $ toHtml (renderMarkdownBlock newEcosystem)

    -- Change trait properties
    Edit'SetTraitContent itemId _traitId oldContent newContent -> do
      p_ $ "changed trait of item " >> printItem itemId
      table_ $ tr_ $ do
        td_ $ blockquote_ $ p_ (toHtml (renderMarkdownInline oldContent))
        td_ $ blockquote_ $ p_ (toHtml (renderMarkdownInline newContent))

    -- Delete
    Edit'DeleteCategory catId _pos -> p_ $ do
      "deleted category " >> printCategory catId
    Edit'DeleteItem itemId _pos -> p_ $ do
      let (category, item) = findItem itemId
      "deleted item " >> quote (toHtml (item^.name))
      " from category " >> quote (toHtml (category^.title))
    Edit'DeleteTrait itemId traitId _pos -> do
      let (_, item, trait) = findTrait itemId traitId
      p_ $ "deleted trait from item " >> quote (toHtml (item^.name))
      blockquote_ $ p_ $ toHtml (trait^.content)

    -- Other
    Edit'MoveItem itemId direction -> p_ $ do
      "moved item " >> printItem itemId
      if direction then " up" else " down"
    Edit'MoveTrait itemId traitId direction -> do
      let (_, item, trait) = findTrait itemId traitId
      p_ $ "moved trait of item " >> quote (toHtml (item^.name)) >>
           if direction then " up" else " down"
      blockquote_ $ p_ $ toHtml (trait^.content)

-- TODO: use “data Direction = Up | Down” for directions instead of Bool

renderHaskellRoot
  :: (MonadIO m, MonadRandom m, MonadReader Config m)
  => GlobalState -> Maybe Text -> HtmlT m ()
renderHaskellRoot globalState mbSearchQuery =
  wrapPage "Aelve Guide: Haskell" $ do
    onPageLoad $ JS.expandHash ()
    case mbSearchQuery of
      Nothing -> h1_ "Aelve Guide: Haskell"
      -- A search page isn't the main page, so we need a link to the main page
      Just _  -> h1_ (mkLink "Aelve Guide: Haskell" "/haskell")
    renderNoScriptWarning
    renderSearch mbSearchQuery
    textInput [
      placeholder_ "add a category",
      autocomplete_ "off",
      onEnter $ JS.addCategoryAndRedirect [inputValue] ]
    case mbSearchQuery of
      Nothing -> renderCategoryList (globalState^.categories)
      Just query' -> do
        let queryWords = T.words query'
        let rank :: Category -> Int
            rank cat = sum [
              length (queryWords `intersect` (cat^..items.each.name)),
              length (queryWords `intersect` T.words (cat^.title)) ]
        let rankedCategories
              | null queryWords = globalState^.categories
              | otherwise       = filter ((/= 0) . rank) .
                                  reverse . sortOn rank
                                    $ globalState^.categories
        renderSearchResults rankedCategories
    -- TODO: maybe add a button like “give me random category that is
    -- unfinished”

renderCategoryPage
  :: (MonadIO m, MonadRandom m, MonadReader Config m)
  => Category -> HtmlT m ()
renderCategoryPage category = do
  wrapPage (category^.title <> " – Haskell – Aelve Guide") $ do
    onPageLoad $ JS.expandHash ()
    -- TODO: another absolute link [absolute-links]
    h1_ (mkLink "Aelve Guide: Haskell" "/haskell")
    renderNoScriptWarning
    renderSearch Nothing
    renderCategory category

renderNoScriptWarning :: MonadRandom m => HtmlT m ()
renderNoScriptWarning =
  noscript_ $ div_ [id_ "noscript-message"] $
    toHtml $ renderMarkdownBlock [text|
      You have Javascript disabled! This site works fine without
      Javascript, but since all editing needs Javascript to work,
      you won't be able to edit anything.
      |]

renderDonate
  :: (MonadIO m, MonadRandom m, MonadReader Config m) => HtmlT m ()
renderDonate = wrapPage "Donate to Artyom" $ do
  toHtmlRaw =<< liftIO (readFile "static/donate.html")

renderStaticMd
  :: (MonadIO m, MonadRandom m, MonadReader Config m)
  => Text -> String -> HtmlT m ()
renderStaticMd t fn = wrapPage t $
  toHtml . renderMarkdownBlock =<< liftIO (T.readFile ("static/" ++ fn))

-- Include all the necessary things
wrapPage
  :: (MonadIO m, MonadRandom m, MonadReader Config m)
  => Text                              -- ^ Page title
  -> HtmlT m ()
  -> HtmlT m ()
wrapPage pageTitle page = doctypehtml_ $ do
  head_ $ do
    title_ (toHtml pageTitle)
    meta_ [name_ "viewport",
           content_ "width=device-width, initial-scale=1.0, user-scalable=yes"]
    link_ [rel_ "icon", href_ "/favicon.ico"]
    token <- _googleToken <$> lift ask
    unless (T.null token) $
      meta_ [name_ "google-site-verification", content_ token]
    -- Report all Javascript errors with alerts
    script_ [text|
      window.onerror = function (msg, url, lineNo, columnNo, error) {
        alert("Error in "+url+" at "+lineNo+":"+columnNo+": "+msg+
              "\n\n"+
              "========== Please report it! =========="+
              "\n\n"+
              "https://github.com/aelve/guide/issues");
        return false; };
      |]
    includeJS "/jquery.js"
    -- for modal dialogs
    includeJS "/magnific-popup.js"
    includeCSS "/magnific-popup.css"
    -- See Note [autosize]
    includeJS "/autosize.js"
    onPageLoad (JS "autosize($('textarea'));")
    -- The order is important – markup.css overrides some rules from
    -- highlight.css (e.g. div.sourceCode), css.css overrides the rule for
    -- a.anchor from markup.css.
    --
    -- TODO: maybe use !important or something instead?
    includeCSS "/highlight.css"
    includeCSS "/markup.css"
    includeCSS "/css.css"
    includeCSS "/loader.css"
    -- Include definitions of all Javascript functions that we have defined
    -- in this file. (This isn't an actual file, so don't look for it in the
    -- static folder – it's generated and served in 'otherMethods'.)
    includeJS "/js.js"
    -- CSS that makes 'shown' and 'noScriptShown' work;
    -- see Note [show-hide]
    noscript_ $ style_ [text|
      .section:not(.noscript-shown) {display:none;}
      |]
    script_ [text|
      var sheet = document.createElement('style');
      sheet.innerHTML = '.section:not(.shown) {display:none;}';
      // “head” instead of “body” because body isn't loaded yet
      document.head.appendChild(sheet);
      |]

  body_ $ do
    script_ $ fromJS $ JS.createAjaxIndicator ()
    div_ [id_ "main"] $
      page
    div_ [id_ "footer"] $ do
      mapM_ (div_ [class_ "footer-item"]) $
        [ do "made by "
             mkLink "Artyom" "https://artyom.me"
        , do mkLink "source" "https://github.com/aelve/guide"
             "/"
             mkLink "issue tracker" "https://github.com/aelve/guide/issues"
        , mkLink "rules" "/unwritten-rules"
        , do div_ (mkLink "donate" "/donate")
             div_ [class_ "unemployed"] "I don't have a job"
        , do "licensed under "
             mkLink "CC+ BY-SA 4.0" "/license"
        ]

renderSearch :: Monad m => Maybe Text -> HtmlT m ()
renderSearch mbSearchQuery = do
  form_ [action_ "/haskell"] $ do
    input_ [type_ "text", name_ "q", id_ "search", placeholder_ "search",
            value_ (fromMaybe "" mbSearchQuery)]

-- If the presentation of the category list ever changes (e.g. to include
-- lists of items in categories, or their counts, or something), you might
-- have to start invalidating 'CacheCategoryList' in more things in
-- 'Cache.invalidateCache'.
renderCategoryList :: (MonadIO m, MonadRandom m) => [Category] -> HtmlT m ()
renderCategoryList cats = cached CacheCategoryList $ do
  div_ [id_ "categories"] $
    for_ (groupWith (view group_) cats) $ \gr ->
      div_ [class_ "category-group"] $ do
        h2_ $ toHtml (gr^?!_head.group_)
        for_ gr $ \category -> do
          -- TODO: this link shouldn't be absolute [absolute-links]
          let cl = case category^.status of
                CategoryFinished   -> "status-finished"
                CategoryMostlyDone -> "status-mostly-done"
                CategoryWIP        -> "status-wip"
                CategoryStub       -> "status-stub"
          a_ [class_ cl, href_ ("/haskell/" <> categorySlug category)] $
            toHtml (category^.title)
          case category^.status of
            CategoryFinished   -> return ()
            CategoryMostlyDone -> span_ [class_ "status"] "mostly done"
            CategoryWIP        -> span_ [class_ "status"] "work in progress"
            CategoryStub       -> span_ [class_ "status"] "stub"
          br_ []

renderSearchResults :: Monad m => [Category] -> HtmlT m ()
renderSearchResults cats = do
  div_ [id_ "categories"] $
    for_ cats $ \category -> do
      -- TODO: this link shouldn't be absolute [absolute-links]
      a_ [href_ ("/haskell/" <> categorySlug category)] $
        toHtml (category^.title)
      br_ []

renderCategoryInfo :: MonadIO m => Category -> HtmlT m ()
renderCategoryInfo category = cached (CacheCategoryInfo (category^.uid)) $ do
  let thisId = "category-info-" <> uidToText (category^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId, class_ "category-info"] $ do

    section "normal" [shown, noScriptShown] $ h2_ $ do
      -- TODO: this link shouldn't be absolute [absolute-links]
      span_ [class_ "controls"] $
        a_ [href_ ("/haskell/feed/category/" <> uidToText (category^.uid))] $
          img_ [src_ "/rss-alt.svg",
                alt_ "category feed", title_ "category feed"]
      -- TODO: this link shouldn't be absolute [absolute-links]
      a_ [href_ ("/haskell/" <> categorySlug category)] $
        toHtml (category^.title)
      emptySpan "1em"
      span_ [class_ "group"] $
        toHtml (category^.group_)
      emptySpan "1em"
      textButton "edit" $
        JS.switchSection (this, "editing" :: Text)
      emptySpan "1em"
      textButton "delete" $
        JS.deleteCategoryAndRedirect [category^.uid]

    section "editing" [] $ do
      let formSubmitHandler formNode =
            JS.submitCategoryInfo (this, category^.uid, formNode)
      form_ [onFormSubmit formSubmitHandler] $ do
        -- All inputs have "autocomplete = off" thanks to
        -- <http://stackoverflow.com/q/8311455>
        label_ $ do
          "Title" >> br_ []
          input_ [type_ "text", name_ "title",
                  autocomplete_ "off",
                  value_ (category^.title)]
        br_ []
        label_ $ do
          "Group" >> br_ []
          input_ [type_ "text", name_ "group",
                  autocomplete_ "off",
                  value_ (category^.group_)]
        br_ []
        label_ $ do
          "Status" >> br_ []
          select_ [name_ "status", autocomplete_ "off"] $ do
            option_ [value_ "finished"] "Complete"
              & selectedIf (category^.status == CategoryFinished)
            option_ [value_ "mostly-done"] "Mostly done/usable"
              & selectedIf (category^.status == CategoryMostlyDone)
            option_ [value_ "wip"] "Work in progress"
              & selectedIf (category^.status == CategoryWIP)
            option_ [value_ "stub"] "Stub"
              & selectedIf (category^.status == CategoryStub)
        br_ []
        label_ $ do
          input_ [type_ "checkbox", name_ "pros-cons-enabled",
                  autocomplete_ "off"]
            & checkedIf (category^.prosConsEnabled)
          "Pros/cons enabled"
        br_ []
        label_ $ do
          input_ [type_ "checkbox", name_ "ecosystem-enabled",
                  autocomplete_ "off"]
            & checkedIf (category^.ecosystemEnabled)
          "“Ecosystem” field enabled"
        br_ []
        input_ [type_ "submit", value_ "Save"]
        button "Cancel" [] $
          JS.switchSection (this, "normal" :: Text)

renderCategoryNotes :: (MonadIO m, MonadRandom m) => Category -> HtmlT m ()
renderCategoryNotes category = cached (CacheCategoryNotes (category^.uid)) $ do
  let thisId = "category-notes-" <> uidToText (category^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId, class_ "category-notes"] $ do

    section "normal" [shown, noScriptShown] $ do
      div_ [class_ "notes-like"] $ do
        if markdownNull (category^.notes)
          then p_ "write something here!"
          else toHtml (category^.notes)
      textButton "edit description" $
        JS.switchSection (this, "editing" :: Text)

    section "editing" [] $ do
      contents <- if markdownNull (category^.notes)
        then liftIO $ renderMarkdownBlock <$>
               T.readFile "static/category-notes-template.md"
        else return (category^.notes)
      markdownEditor
        [rows_ "10"]
        contents
        (\val -> JS.submitCategoryNotes (this, category^.uid, val))
        (JS.switchSection (this, "normal" :: Text))

renderCategory :: (MonadIO m, MonadRandom m) => Category -> HtmlT m ()
renderCategory category = cached (CacheCategory (category^.uid)) $ do
  div_ [class_ "category", id_ (categoryNodeId category)] $ do
    renderCategoryInfo category
    renderCategoryNotes category
    itemsNode <- div_ [class_ "items"] $ do
      mapM_ (renderItem category) (category^.items)
      thisNode
    textInput [
      placeholder_ "add an item",
      autocomplete_ "off",
      onEnter $ JS.addItem (itemsNode, category^.uid, inputValue) <>
                clearInput ]

getItemHue :: Category -> Item -> Hue
getItemHue category item = case item^.group_ of
  Nothing -> NoHue
  Just s  -> M.findWithDefault NoHue s (category^.groups)

{- Note [enabled sections]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Categories have flags that enable/disable showing some sections of the items (currently pros/cons and ecosystem); this is done because for some items (like books, or people) “ecosystem” might not make any sense, and pros/cons don't make sense for categories that contain diverse items.

When we change those flags (by editing category info), we want to update the way items are shown (without reloading the page). So, if the “show ecosystem” flag has been set and we unset it, we want to hide the ecosystem section in all items belonging to the category. This happens in 'JS.submitCategoryInfo'.

If the category has showing pros/cons (or ecosystem, or both) disabled, we have to render traits and ecosystem as hidden (we can't just not render them at all, because then we wouldn't be able to un-hide them). How could we do it? If we do it in 'renderItemTraits' or 'renderItemEcosystem', this would mean that cached versions of traits/ecosystem would have to be rerendered whenever prosConsEnabled/ecosystemEnabled is changed. So, instead we do a somewhat inelegant thing: we wrap traits/ecosystem into yet another <div>, and set “display:none” on it. 'JS.submitCategoryInfo' operates on those <div>s.
-}

-- TODO: perhaps use jQuery Touch Punch or something to allow dragging items
-- instead of using arrows? Touch Punch works on mobile, too
renderItem :: (MonadIO m, MonadRandom m) => Category -> Item -> HtmlT m ()
renderItem category item = cached (CacheItem (item^.uid)) $ do
  -- The id is used for links in feeds, and for anchor links
  div_ [id_ (itemNodeId item), class_ "item"] $ do
    renderItemInfo category item
    let bg = hueToLightColor $ getItemHue category item
    div_ [class_ "item-body", style_ ("background-color:" <> bg)] $ do
      -- See Note [enabled sections]
      renderItemDescription item
      hiddenIf (not (category^.prosConsEnabled)) $
        div_ [class_ "pros-cons-wrapper"] $
          renderItemTraits item
      hiddenIf (not (category^.ecosystemEnabled)) $
        div_ [class_ "ecosystem-wrapper"] $
          renderItemEcosystem item
      renderItemNotes category item

-- TODO: warn when a library isn't on Hackage but is supposed to be

renderItemTitle :: Monad m => Item -> HtmlT m ()
renderItemTitle item = do
  case item^.link of
    Just l  -> a_ [href_ l] (toHtml (item^.name))
    Nothing -> toHtml (item^.name)
  let hackageLink x = "https://hackage.haskell.org/package/" <> x
  case item ^. kind.hackageName of
    Just x  -> " (" >> a_ [href_ (hackageLink x)] "Hackage" >> ")"
    Nothing -> return ()

-- TODO: give a link to oldest available docs when the new docs aren't there
renderItemInfo :: (MonadIO m, MonadRandom m) => Category -> Item -> HtmlT m ()
renderItemInfo cat item = cached (CacheItemInfo (item^.uid)) $ do
  let bg = hueToDarkColor $ getItemHue cat item
  let thisId = "item-info-" <> uidToText (item^.uid)
      this   = JS.selectId thisId
  let bodyNode = JS.selectChildren (JS.selectParent this)
                                   (JS.selectClass "item-body")
  div_ [id_ thisId, class_ "item-info",
        style_ ("background-color:" <> bg)] $ do

    section "normal" [shown, noScriptShown] $ do
      -- TODO: [very-easy] move this style_ into css.css
      span_ [style_ "font-size:150%"] $ do
        -- TODO: absolute links again [absolute-links]
        a_ [class_ "anchor", href_ (itemLink cat item)] "#"
        renderItemTitle item
      emptySpan "2em"
      toHtml (fromMaybe "other" (item^.group_))
      span_ [class_ "controls"] $ do
        imgButton "move item up" "/arrow-thick-top.svg" [] $
          JS.moveItemUp (item^.uid, itemNode item)
        imgButton "move item down" "/arrow-thick-bottom.svg" [] $
          JS.moveItemDown (item^.uid, itemNode item)
        emptySpan "1.5em"
        imgButton "edit item info" "/pencil.svg" [] $
          JS.switchSection (this, "editing" :: Text)
        emptySpan "0.5em"
        imgButton "delete item" "/x.svg" [] $
          JS.deleteItem (item^.uid, itemNode item)

    section "editing" [] $ do
      -- When the info/header node changes its group (and is hence
      -- recolored), item's body has to be recolored too
      let formSubmitHandler formNode =
            JS.submitItemInfo (this, bodyNode, item^.uid, formNode)
      form_ [onFormSubmit formSubmitHandler] $ do
        -- All inputs have "autocomplete = off" thanks to
        -- <http://stackoverflow.com/q/8311455>
        label_ $ do
          "Name" >> br_ []
          input_ [type_ "text", name_ "name",
                  autocomplete_ "off",
                  value_ (item^.name)]
        br_ []
        label_ $ do
          "Kind" >> br_ []
          select_ [name_ "kind", autocomplete_ "off"] $ do
            option_ [value_ "library"] "Library"
              & selectedIf (case item^.kind of Library{} -> True; _ -> False)
            option_ [value_ "tool"] "Tool"
              & selectedIf (case item^.kind of Tool{} -> True; _ -> False)
            option_ [value_ "other"] "Other"
              & selectedIf (case item^.kind of Other{} -> True; _ -> False)
        br_ []
        label_ $ do
          "Name on Hackage" >> br_ []
          input_ [type_ "text", name_ "hackage-name", autocomplete_ "off",
                  value_ (fromMaybe "" (item^?kind.hackageName._Just))]
        br_ []
        label_ $ do
          "Site (optional)" >> br_ []
          input_ [type_ "text", name_ "link", autocomplete_ "off",
                  value_ (fromMaybe "" (item^.link))]
        br_ []
        newGroupInputId <- randomLongUid
        label_ $ do
          "Group" >> br_ []
          -- When “new group” is selected in the list, we show a field for
          -- entering new group's name
          let selectHandler = [text|
                  if (this.value == "$newGroupValue") {
                    $("#$idText").show();
                    $("#$idText").focus(); }
                  else $("#$idText").hide(); |]
                where idText = uidToText newGroupInputId
          select_ [name_ "group", autocomplete_ "off",
                   onchange_ selectHandler] $ do
            let gs = Nothing : map Just (M.keys (cat^.groups))
            for_ gs $ \group' -> do
              -- Text that will be shown in the list (“-” stands for “no
              -- group”)
              let txt = fromMaybe "-" group'
              -- If the element corresponds to the current group of the
              -- item (or the element is “-”, i.e. Nothing, and the group
              -- is Nothing too), mark it as selected, thus making it the
              -- element that will be chosen by default when the form is
              -- rendered
              option_ [value_ txt] (toHtml txt)
                & selectedIf (group' == item^.group_)
            option_ [value_ newGroupValue] "New group..."
        input_ [uid_ newGroupInputId, type_ "text", autocomplete_ "off",
                name_ "custom-group", hidden_ "hidden"]
        br_ []
        input_ [type_ "submit", value_ "Save"]
        button "Cancel" [] $
          JS.switchSection (this, "normal" :: Text)

-- TODO: just make a synonym for “Html with IO and randomness”

renderItemDescription :: (MonadIO m, MonadRandom m) => Item -> HtmlT m ()
renderItemDescription item = cached (CacheItemDescription (item^.uid)) $ do
  let thisId = "item-description-" <> uidToText (item^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId, class_ "item-description"] $ do

    section "normal" [shown, noScriptShown] $ do
      strong_ "Summary"
      emptySpan "0.5em"
      imgButton "edit summary" "/pencil.svg"
        [style_ "width:12px;opacity:0.5"] $
        JS.switchSection (this, "editing" :: Text)
      div_ [class_ "notes-like"] $ do
        if markdownNull (item^.description)
          then p_ "write something here!"
          else toHtml (item^.description)

    section "editing" [] $ do
      strong_ "Summary"
      emptySpan "0.5em"
      imgButton "quit editing summary" "/pencil.svg"
        [style_ "width:12px;opacity:0.5"] $
        JS.switchSection (this, "normal" :: Text)
      markdownEditor
        [rows_ "10"]
        (item^.description)
        (\val -> JS.submitItemDescription
                   (this, item^.uid, item^.description.mdText, val))
        (JS.switchSection (this, "normal" :: Text))

renderItemEcosystem :: (MonadIO m, MonadRandom m) => Item -> HtmlT m ()
renderItemEcosystem item = cached (CacheItemEcosystem (item^.uid)) $ do
  let thisId = "item-ecosystem-" <> uidToText (item^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId, class_ "item-ecosystem"] $ do

    section "normal" [shown, noScriptShown] $ do
      strong_ "Ecosystem"
      emptySpan "0.5em"
      imgButton "edit ecosystem" "/pencil.svg"
        [style_ "width:12px;opacity:0.5"] $
        JS.switchSection (this, "editing" :: Text)
      unless (markdownNull (item^.ecosystem)) $
        toHtml (item^.ecosystem)

    section "editing" [] $ do
      strong_ "Ecosystem"
      emptySpan "0.5em"
      imgButton "quit editing ecosystem" "/pencil.svg"
        [style_ "width:12px;opacity:0.5"] $
        JS.switchSection (this, "normal" :: Text)
      markdownEditor
        [rows_ "3"]
        (item^.ecosystem)
        (\val -> JS.submitItemEcosystem
                   (this, item^.uid, item^.ecosystem.mdText, val))
        (JS.switchSection (this, "normal" :: Text))

renderItemTraits :: (MonadIO m, MonadRandom m) => Item -> HtmlT m ()
renderItemTraits item = cached (CacheItemTraits (item^.uid)) $ do
  div_ [class_ "item-traits"] $ do
    div_ [class_ "traits-groups-container"] $ do
      div_ [class_ "traits-group"] $ do
        strong_ "Pros"
        this <- thisNode
        emptySpan "0.5em"
        sectionSpan "normal" [shown, noScriptShown] $ do
          imgButton "edit pros" "/pencil.svg"
            [style_ "width:12px;opacity:0.5"] $
            JS.switchSectionsEverywhere (this, "editable" :: Text)
        sectionSpan "editable" [] $ do
          imgButton "quit editing pros" "/pencil.svg"
            [style_ "width:12px;opacity:0.5"] $
            JS.switchSectionsEverywhere (this, "normal" :: Text)
        -- We can't use 'thisNode' inside <ul> because it creates a <span>
        -- and only <li> elements can be children of <ul>
        listUid <- randomLongUid
        ul_ [uid_ listUid] $
          mapM_ (renderTrait (item^.uid)) (item^.pros)
        section "editable" [] $ do
          smallMarkdownEditor
            [rows_ "3", placeholder_ "add pro"]
            (renderMarkdownInline "")
            (\val -> JS.addPro (JS.selectUid listUid, item^.uid, val) <>
                     JS.assign val ("" :: Text))
            Nothing
          textButton "edit off" $
            JS.switchSectionsEverywhere(this, "normal" :: Text)

      div_ [class_ "traits-group"] $ do
        strong_ "Cons"
        this <- thisNode
        emptySpan "0.5em"
        sectionSpan "normal" [shown, noScriptShown] $ do
          imgButton "edit cons" "/pencil.svg"
            [style_ "width:12px;opacity:0.5"] $
            JS.switchSectionsEverywhere (this, "editable" :: Text)
        sectionSpan "editable" [] $ do
          imgButton "quit editing cons" "/pencil.svg"
            [style_ "width:12px;opacity:0.5"] $
            JS.switchSectionsEverywhere (this, "normal" :: Text)
        listUid <- randomLongUid
        ul_ [uid_ listUid] $
          mapM_ (renderTrait (item^.uid)) (item^.cons)
        section "editable" [] $ do
          smallMarkdownEditor
            [rows_ "3", placeholder_ "add con"]
            (renderMarkdownInline "")
            (\val -> JS.addCon (JS.selectUid listUid, item^.uid, val) <>
                     JS.assign val ("" :: Text))
            Nothing
          textButton "edit off" $
            JS.switchSectionsEverywhere(this, "normal" :: Text)

renderTrait :: MonadRandom m => Uid Item -> Trait -> HtmlT m ()
renderTrait itemId trait = do
  let thisId = "trait-" <> uidToText (trait^.uid)
      this   = JS.selectId thisId
  editingSectionUid <- randomLongUid
  li_ [id_ thisId] $ do

    section "normal editable" [shown, noScriptShown] $ do
      toHtml (trait^.content)

    section "editable" [] $ do
      div_ [class_ "trait-controls"] $ do
        imgButton "move trait up" "/arrow-thick-top.svg" [] $
          JS.moveTraitUp (itemId, trait^.uid, this)
        imgButton "move trait down" "/arrow-thick-bottom.svg" [] $
          JS.moveTraitDown (itemId, trait^.uid, this)
        emptySpan "16px"
        textareaUid <- randomLongUid
        imgButton "edit trait" "/pencil.svg" [] $
          -- See Note [dynamic interface]
          JS.makeTraitEditor (this, JS.selectUid editingSectionUid,
                              textareaUid,
                              trait^.content.mdText,
                              itemId, trait^.uid) <>
          JS.switchSection (this, "editing" :: Text) <>
          JS.autosizeTextarea [JS.selectUid textareaUid]
        emptySpan "16px"
        imgButton "delete trait" "/x.svg" [] $
          JS.deleteTrait (itemId, trait^.uid, this)

    section "editing" [uid_ editingSectionUid] $ do
      return ()

-- TODO: automatically provide links to modules in Markdown (and have a
-- database of modules or something)

-- TODO: [very-easy] write about the all-is-text extension
-- TODO: [easy] write that arrows are for arranging stuff, not upvoting

-- TODO: [easy] add Hayoo search, Hoogle search, and Hackage search shortcut
-- boxes

-- TODO: when searching, show links to package, main modules, etc before all
-- categories

-- TODO: attach TODOs (“fix grammar”, etc) to items and categories (or should
-- people instead just write “TODO fix grammar” in description and then such
-- things could be displayed in gray font and also there'd be an
-- automatically updated list of TODOs somewhere?)

-- TODO: [very-easy] focus the notes textarea on edit (can use jQuery's
-- .focus() on it)
renderItemNotes
  :: (MonadIO m, MonadRandom m)
  => Category -> Item -> HtmlT m ()
renderItemNotes category item = cached (CacheItemNotes (item^.uid)) $ do
  -- Don't change this ID, it's used in e.g. 'JS.expandHash'
  let thisId = "item-notes-" <> uidToText (item^.uid)
      this   = JS.selectId thisId
  editingSectionUid <- randomLongUid
  div_ [id_ thisId, class_ "item-notes"] $ do
    let notesLink = T.format "/haskell/{}#{}"
                             (categorySlug category, thisId)
    a_ [href_ notesLink] $
      strong_ "Notes"

    let renderTree :: Monad m => Forest (Inlines, Text) -> HtmlT m ()
        renderTree [] = return ()
        renderTree xs = ul_ $ do
          for_ xs $ \(Node (is, id') children) -> li_ $ do
            let handler = fromJS (JS.expandItemNotes [item^.uid])
                -- The link has to be full because sometimes we are
                -- looking at items from pages different from the
                -- proper category pages (e.g. if a search returned a
                -- list of items). Well, actually it doesn't happen
                -- yet (at the moment of writing), but it might start
                -- happening and then it's better to be prepared.
                fullLink = T.format "/haskell/{}#{}"
                                    (categorySlug category, id')
            a_ [href_ fullLink, onclick_ handler] $
              renderInlines def is
            renderTree children
    let renderTOC = do
          let toc = item^.notes.mdTOC
          div_ [class_ "notes-toc"] $ do
            if null toc
              then p_ (emptySpan "1.5em" >> "<notes are empty>")
              else renderTree toc

    section "collapsed" [shown] $ do
      textButton "expand notes" $
        JS.expandItemNotes [item^.uid]
      renderTOC

    section "expanded" [noScriptShown] $ do
      textareaUid <- randomLongUid
      contents <- if markdownNull (item^.notes)
        then liftIO $ T.readFile "static/item-notes-template.md"
        else return (item^.notes.mdText)
      let buttons = do
            textButton "collapse notes" $
              JS.switchSection (this, "collapsed" :: Text)
            emptySpan "1em"
            textButton "edit notes" $
              -- See Note [dynamic interface]
              JS.makeItemNotesEditor (
                   this, JS.selectUid editingSectionUid,
                   textareaUid,
                   -- See Note [blurb diffing]
                   markdownNull (item^.notes),
                   contents,
                   item^.uid) <>
              JS.switchSection (this, "editing" :: Text) <>
              JS.autosizeTextarea [JS.selectUid textareaUid]
      buttons
      renderTOC
      div_ [class_ "notes-like"] $ do
        if markdownNull (item^.notes)
          then p_ "add something!"
          else toHtml (item^.notes)
      unless (markdownNull (item^.notes)) $
        buttons
      -- TODO: [easy] the lower “hide notes” should scroll back to item when
      -- the notes are closed (but don't scroll if it's already visible after
      -- the notes have been hidden)

    section "editing" [uid_ editingSectionUid] $
      return ()

-- TODO: a shortcut for editing (when you press Ctrl-something, whatever was
-- selected becomes editable)

renderItemForFeed :: Monad m => Category -> Item -> HtmlT m ()
renderItemForFeed category item = do
  h1_ $ renderItemTitle item
  unless (markdownNull (item^.description)) $
    toHtml (item^.description)
  when (category^.prosConsEnabled) $ do
    h2_ "Pros"
    ul_ $ mapM_ (p_ . li_ . toHtml . view content) (item^.pros)
    h2_ "Cons"
    ul_ $ mapM_ (p_ . li_ . toHtml . view content) (item^.cons)
  when (category^.ecosystemEnabled) $ do
    unless (markdownNull (item^.ecosystem)) $ do
      h2_ "Ecosystem"
      toHtml (item^.ecosystem)
  -- TODO: include .notes-like style here? otherwise the headers are too big
  unless (markdownNull (item^.notes)) $ do
    h2_ "Notes"
    toHtml (item^.notes)

-- Utils

onPageLoad :: Monad m => JS -> HtmlT m ()
onPageLoad js = script_ $ T.format "$(document).ready(function(){{}});" [js]

emptySpan :: Monad m => Text -> HtmlT m ()
emptySpan w = span_ [style_ ("margin-left:" <> w)] mempty

-- Use inputValue to get the value (works with input_ and textarea_)
onEnter :: JS -> Attribute
onEnter handler = onkeydown_ $
  T.format "if (event.keyCode == 13) {{} return false;}" [handler]

textInput :: Monad m => [Attribute] -> HtmlT m ()
textInput attrs = input_ (type_ "text" : attrs)

inputValue :: JS
inputValue = JS "this.value"

clearInput :: JS
clearInput = JS "this.value = '';"

onFormSubmit :: (JS -> JS) -> Attribute
onFormSubmit f = onsubmit_ $ T.format "{} return false;" [f (JS "this")]

button :: Monad m => Text -> [Attribute] -> JS -> HtmlT m ()
button value attrs handler =
  input_ (type_ "button" : value_ value : onclick_ handler' : attrs)
  where
    handler' = fromJS handler

-- A text button looks like “[cancel]”
textButton
  :: Monad m
  => Text         -- ^ Button text
  -> JS           -- ^ Onclick handler
  -> HtmlT m ()
textButton caption (JS handler) =
  span_ [class_ "text-button"] $
    -- “#” is used instead of javascript:void(0) because the latter is slow
    -- in Firefox (at least for me – tested with Firefox 43 on Arch Linux)
    a_ [href_ "#", onclick_ (handler <> "return false;")]
       (toHtml caption)

-- So far all icons used here have been from <https://useiconic.com/open/>
imgButton :: Monad m => Text -> Url -> [Attribute] -> JS -> HtmlT m ()
imgButton alt src attrs (JS handler) =
  a_ [href_ "#", onclick_ (handler <> "return false;")]
     (img_ (src_ src : alt_ alt : title_ alt : attrs))

mkLink :: Monad m => HtmlT m a -> Url -> HtmlT m a
mkLink x src = a_ [href_ src] x

selectedIf :: With w => Bool -> w -> w
selectedIf p x = if p then with x [selected_ "selected"] else x

checkedIf :: With w => Bool -> w -> w
checkedIf p x = if p then with x [checked_] else x

hiddenIf :: With w => Bool -> w -> w
hiddenIf p x = if p then with x [style_ "display:none;"] else x

markdownEditor
  :: MonadRandom m
  => [Attribute]
  -> MarkdownBlock  -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving the contents of the editor
  -> JS             -- ^ “Cancel” handler
  -> HtmlT m ()
markdownEditor attr (view mdText -> s) submit cancel = do
  textareaUid <- randomLongUid
  -- Autocomplete has to be turned off thanks to
  -- <http://stackoverflow.com/q/8311455>.
  textarea_ ([uid_ textareaUid, autocomplete_ "off", class_ "big fullwidth"]
             ++ attr) $
    toHtml s
  let val = JS $ T.format "document.getElementById(\"{}\").value" [textareaUid]
  button "Save" [] $
    submit val
  emptySpan "6px"
  button "Cancel" [] $
    JS.assign val s <>
    cancel
  emptySpan "6px"
  a_ [href_ "/markdown", target_ "_blank"] "Markdown"

smallMarkdownEditor
  :: MonadRandom m
  => [Attribute]
  -> MarkdownInline -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving the contents of the editor
  -> Maybe JS       -- ^ “Cancel” handler (if “Cancel” is needed)
  -> HtmlT m ()
smallMarkdownEditor attr (view mdText -> s) submit mbCancel = do
  textareaId <- randomLongUid
  let val = JS $ T.format "document.getElementById(\"{}\").value" [textareaId]
  textarea_ ([class_ "fullwidth", uid_ textareaId, autocomplete_ "off",
              onEnter (submit val)] ++ attr) $
    toHtml s
  case mbCancel of
    Nothing -> return ()
    Just cancel -> do
      br_ []
      textButton "cancel" $
        JS.assign val s <>
        cancel
  a_ [href_ "/markdown", target_ "_blank", style_ "float:right"] "Markdown"

thisNode :: MonadRandom m => HtmlT m JQuerySelector
thisNode = do
  uid' <- randomLongUid
  -- If the class name ever changes, fix 'JS.moveNodeUp' and
  -- 'JS.moveNodeDown'.
  span_ [uid_ uid', class_ "dummy"] mempty
  return (JS.selectParent (JS.selectUid uid'))

itemNodeId :: Item -> Text
itemNodeId item = "item-" <> uidToText (item^.uid)

itemNode :: Item -> JQuerySelector
itemNode = JS.selectId . itemNodeId

categoryNodeId :: Category -> Text
categoryNodeId category = "category-" <> uidToText (category^.uid)

itemLink :: Category -> Item -> Text
itemLink category item =
  T.format "/haskell/{}#{}" (categorySlug category, itemNodeId item)

-- See Note [show-hide]; wheh changing these, also look at 'JS.switchSection'.
shown, noScriptShown :: Attribute
shown          = class_ " shown "
noScriptShown  = class_ " noscript-shown "

-- See Note [show-hide]
section
  :: Monad m
  => Text           -- ^ Section name (or names)
  -> [Attribute]    -- ^ Additional attributes
  -> HtmlT m ()     -- ^ Content of the section
  -> HtmlT m ()
section t attrs = div_ (class_ (t <> " section ") : attrs)

-- See Note [show-hide]
sectionSpan
  :: Monad m
  => Text           -- ^ Section name (or names)
  -> [Attribute]    -- ^ Additional attributes
  -> HtmlT m ()     -- ^ Content of the section
  -> HtmlT m ()
sectionSpan t attrs = span_ (class_ (t <> " section ") : attrs)

newGroupValue :: Text
newGroupValue = "-new-group-"
