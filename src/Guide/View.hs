{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Guide.View
(
  getJS,
  getCSS,

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
)
where

import Imports


-- Lists
import Data.List.Split
-- Containers
import qualified Data.Map as M
import Data.Tree
-- Text
import qualified Data.Text.All as T
import qualified Data.Text.Lazy.All as TL
import NeatInterpolation
-- Web
import Lucid hiding (for_)
-- Files
import qualified System.FilePath.Find as F
-- Network
import Data.IP
-- Time
import Data.Time
import Data.Time.Format.Human
-- Markdown
import qualified CMark as MD
-- Mustache (templates)
import Text.Mustache.Plus
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Semigroup as Semigroup
import qualified Data.List.NonEmpty as NonEmpty
import Text.Megaparsec
import Text.Megaparsec.Text

import Guide.Config
import Guide.Types
import Guide.Utils
import Guide.JS (JS(..), JQuerySelector)
import qualified Guide.JS as JS
import Guide.Markdown
import Guide.Cache


{- Note [autosize]
~~~~~~~~~~~~~~~~~~

All textareas on the site are autosized – i.e. they grow when the user is
typing. This is done by the autosize.js plugin, which is called on page load:

    autosize($('textarea'));

A slight problem is that it doesn't compute the height of hidden elements
correctly – thus, when something is shown and it happens to be a textarea or
contain a textarea, we have to call autosize again. This is done in
'JS.switchSection'. So far there are no textboxes that are shown *without*
switchSection being involved, and so there's no need to watch for elements
being added to the DOM.

It would be nicer if we could watch for elements becoming visible without
having to modify switchSection, but there doesn't seem to be an easy way to
do this – MutationObserver doesn't let us find out when something becomes
visible (i.e. when its clientHeight stops being 0).

In switchSection we use

    autosize($('textarea'));
    autosize.update($('textarea'));

instead of simple

    autosize.update($('textarea'));

– this is done because the textarea could have appeared after the original
`autosize($('textarea'));` was called on page load (which could happen if an
item was added, for instance).

-}

{- Note [show-hide]
~~~~~~~~~~~~~~~~~~~

A lot of things (notes, etc) can be expanded/collapsed by pressing a
button. Similarly, pressing “edit” replaces rendered text with a textbox, or
adds buttons to pros/cons. All this is done with sections and show/hide.

A section is something that can be shown or hidden. You define a section by
using 'section' (which creates a <div>) or 'sectionSpan' (which creates a
<span>).

    section "normal" [shown, noScriptShown] $ do
      renderText
      ...

    section "editing" [] $ do
      renderEditbox
      ...

You can even give 2 names to a section – e.g. "normal editing" if you want
the section be visible both in “normal” mode and in “editing” mode.

The list parameter is used to add attributes to the section. 'shown' is an
attribute that means that the section is normally visible; 'noScriptShown'
means that the section will be visible when Javascipt is disabled. Sections
without either attribute will be hidden. (Usually 'shown' and 'noScriptShown'
go together, but not always.)

When several sections are in the same container (e.g. a <div>), you can
toggle between them with 'JS.switchSection', which shows the section (or
several sections) with given name, and hides all sections with other
names. The elements that aren't sections are not affected.

Also, there's another function available – 'JS.switchSectionEverywhere' –
that switches sections everywhere inside the container, not only among
container's direct children. It's useful when you have something like a list
of pros/cons and you want to switch them all into the “editable” state.

////////////////////////////////////

And now, here's how it's all implemented.

In 'wrapPage' there's a piece of CSS wrapped in <noscript> that hides
everything except for 'noScriptShown' things:

    .section:not(.noscript-shown) {display:none;}

There's also a piece of Javascript that, when executed, will change it to the
following CSS:

    .section:not(.shown) {display:none;}

So, if Javascript is disabled we hide all sections except for those that have
the 'noScriptShown' attribute, and if it's enabled we hide all sections
except for those that have the 'shown' attribute.

After that switching sections is simply done by adding/removing the “shown”
class. (Note that we don't have to choose between “noscript-shown” and
“shown” because switching sections is *only* possible if Javascript is
enabled, and in this case the relevant tag will always be “shown” and not
“noscript-shown”.)

-}

renderSubtitle :: (MonadReader Config m) => HtmlT m ()
renderSubtitle =
  div_ [class_ "subtitle"] $ do
    "alpha version • don't post on Reddit yet"
    lift (asks _discussLink) >>= \case
      Nothing -> return ()
      Just l  -> " • " >> mkLink "discuss the site" l

renderRoot :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderRoot = do
  wrapPage "Aelve Guide" $ do
    h1_ "Aelve Guide"
    renderSubtitle
    h2_ (mkLink "Haskell" "/haskell")

-- TODO: show a “category not found” page

renderAdmin :: (MonadIO m) => GlobalState -> HtmlT m ()
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
  :: (MonadIO m)
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
        quote $ a_ [href_ (categoryLink category)] $
          toHtml (category ^. title)
  let printItem itemId = do
        let (category, item) = findItem itemId
        quote $ a_ [href_ (itemLink category item)] $
          toHtml (item ^. name)

  case edit of
    -- Add
    Edit'AddCategory _catId title' -> p_ $ do
      "added category " >> quote (toHtml title')
    Edit'AddItem catId _itemId name' -> p_ $ do
      "added item " >> quote (toHtml name')
      " to category " >> printCategory catId
    Edit'AddPro itemId _traitId content' -> do
      p_ $ "added pro to item " >> printItem itemId
      blockquote_ $ p_ $ toHtml (toMarkdownInline content')
    Edit'AddCon itemId _traitId content' -> do
      p_ $ "added con to item " >> printItem itemId
      blockquote_ $ p_ $ toHtml (toMarkdownInline content')

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
      p_ $ (if T.null oldNotes then "added" else "changed") >>
           " notes of category " >> printCategory catId
      table_ $ tr_ $ do
        unless (T.null oldNotes) $
          td_ $ blockquote_ $ toHtml (toMarkdownBlock oldNotes)
        td_ $ blockquote_ $ toHtml (toMarkdownBlock newNotes)
    Edit'SetCategoryProsConsEnabled catId _oldVal newVal -> do
      if newVal == True
        then p_ $ "enabled pros/cons for category " >> printCategory catId
        else p_ $ "disabled pros/cons for category " >> printCategory catId
    Edit'SetCategoryEcosystemEnabled catId _oldVal newVal -> do
      if newVal == True
        then p_ $ "enabled ecosystem for category " >> printCategory catId
        else p_ $ "disabled ecosystem for category " >> printCategory catId
    Edit'SetCategoryNotesEnabled catId _oldVal newVal -> do
      if newVal == True
        then p_ $ "enabled notes for category " >> printCategory catId
        else p_ $ "disabled notes for category " >> printCategory catId

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
      p_ $ (if T.null oldDescr then "added" else "changed") >>
           " description of item " >> printItem itemId
      table_ $ tr_ $ do
        unless (T.null oldDescr) $
          td_ $ blockquote_ $ toHtml (toMarkdownBlock oldDescr)
        td_ $ blockquote_ $ toHtml (toMarkdownBlock newDescr)
    Edit'SetItemNotes itemId oldNotes newNotes -> do
      p_ $ (if T.null oldNotes then "added" else "changed") >>
           " notes of item " >> printItem itemId
      table_ $ tr_ $ do
        unless (T.null oldNotes) $
          td_ $ blockquote_ $ toHtml (toMarkdownBlock oldNotes)
        td_ $ blockquote_ $ toHtml (toMarkdownBlock newNotes)
    Edit'SetItemEcosystem itemId oldEcosystem newEcosystem -> do
      p_ $ (if T.null oldEcosystem then "added" else "changed") >>
           " ecosystem of item " >> printItem itemId
      table_ $ tr_ $ do
        unless (T.null oldEcosystem) $
          td_ $ blockquote_ $ toHtml (toMarkdownBlock oldEcosystem)
        td_ $ blockquote_ $ toHtml (toMarkdownBlock newEcosystem)

    -- Change trait properties
    Edit'SetTraitContent itemId _traitId oldContent newContent -> do
      p_ $ (if T.null oldContent then "added" else "changed") >>
           " trait of item " >> printItem itemId
      table_ $ tr_ $ do
        unless (T.null oldContent) $
          td_ $ blockquote_ $ p_ (toHtml (toMarkdownInline oldContent))
        td_ $ blockquote_ $ p_ (toHtml (toMarkdownInline newContent))

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

-- | “Aelve Guide | Haskell”
haskellHeader :: (MonadReader Config m) => HtmlT m ()
haskellHeader = do
  h1_ $ mkLink ("Aelve Guide " >> span_ "| Haskell") "/haskell"
  renderSubtitle

haskellHeaderMain :: (MonadReader Config m) => HtmlT m ()
haskellHeaderMain = do
  h1_ $ "Aelve Guide " >> span_ "| Haskell"
  renderSubtitle

renderHaskellRoot
  :: (MonadIO m, MonadReader Config m)
  => GlobalState -> Maybe Text -> HtmlT m ()
renderHaskellRoot globalState mbSearchQuery =
  wrapPage "Aelve Guide | Haskell" $ do
    onPageLoad $ JS.expandHash ()
    case mbSearchQuery of
      Nothing -> haskellHeaderMain
      -- A search page isn't the main page, so we need a link to the main page
      Just _  -> haskellHeader
    renderNoScriptWarning
    renderSearch mbSearchQuery
    textInput [
      placeholder_ "add a category",
      class_ "add-category",
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
  :: (MonadIO m, MonadReader Config m)
  => Category -> HtmlT m ()
renderCategoryPage category = do
  wrapPage (category^.title <> " – Haskell – Aelve Guide") $ do
    onPageLoad $ JS.expandHash ()
    haskellHeader
    renderNoScriptWarning
    renderSearch Nothing
    renderCategory category

renderNoScriptWarning :: Monad m => HtmlT m ()
renderNoScriptWarning =
  noscript_ $ div_ [id_ "noscript-message"] $
    toHtml $ toMarkdownBlock [text|
      You have Javascript disabled! This site works fine without
      Javascript, but since all editing needs Javascript to work,
      you won't be able to edit anything.
      |]

renderDonate
  :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderDonate = wrapPage "Donate to Artyom" $ do
  toHtmlRaw =<< liftIO (readFile "static/donate.html")

renderStaticMd
  :: (MonadIO m, MonadReader Config m)
  => Text -> String -> HtmlT m ()
renderStaticMd t fn = wrapPage t $
  toHtml . toMarkdownBlock =<< liftIO (T.readFile ("static/" ++ fn))

-- Include all the necessary things
wrapPage
  :: (MonadIO m, MonadReader Config m)
  => Text                              -- ^ Page title
  -> HtmlT m ()
  -> HtmlT m ()
wrapPage pageTitle page = doctypehtml_ $ do
  head_ $ do
    title_ (toHtml pageTitle)
    meta_ [name_ "viewport",
           content_ "width=device-width, initial-scale=1.0, user-scalable=yes"]
    link_ [rel_ "icon", href_ "/favicon.ico"]
    googleToken <- _googleToken <$> lift ask
    unless (T.null googleToken) $
      meta_ [name_ "google-site-verification", content_ googleToken]
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
        , mkLink "donate" "/donate"
        , do "licensed under "
             mkLink "CC+ BY-SA 4.0" "/license"
        ]

renderSearch :: (MonadIO m) => Maybe Text -> HtmlT m ()
renderSearch mbSearchQuery =
  mustache "search" $ A.object [
    "query" A..= mbSearchQuery ]

-- If the presentation of the category list ever changes (e.g. to include
-- lists of items in categories, or their counts, or something), you might
-- have to start invalidating 'CacheCategoryList' in more things in
-- 'Cache.invalidateCache'.
renderCategoryList :: MonadIO m => [Category] -> HtmlT m ()
renderCategoryList allCats = cached CacheCategoryList $ do
  div_ [id_ "categories"] $
    for_ (groupWith (view group_) allCats) $ \catsInGroup ->
      div_ [class_ "category-group"] $ do
        -- Grandcategory name
        h2_ $ toHtml (catsInGroup^?!_head.group_)
        -- Finished categories
        do let cats = filter ((== CategoryFinished) . view status) catsInGroup
           unless (null cats) $
             div_ [class_ "categories-finished"] $ do
               mapM_ mkCategoryLink cats
        -- In-progress categories, separated with commas
        do let cats = filter ((== CategoryWIP) . view status) catsInGroup
           unless (null cats) $
             div_ [class_ "categories-wip"] $ do
               h3_ "In progress"
               p_ $ sequence_ $ intersperse ", " $
                 map mkCategoryLink cats
        -- Stub categories, separated with commas
        do let cats = filter ((== CategoryStub) . view status) catsInGroup
           unless (null cats) $
             div_ [class_ "categories-stub"] $ do
               h3_ "To be written"
               p_ $ sequence_ $ intersperse ", " $
                 map mkCategoryLink cats
  where
    -- TODO: this link shouldn't be absolute [absolute-links]
    mkCategoryLink :: Category -> HtmlT IO ()
    mkCategoryLink category =
      a_ [class_ "category-link", href_ (categoryLink category)] $
        toHtml (category^.title)

renderSearchResults :: Monad m => [Category] -> HtmlT m ()
renderSearchResults cats = do
  div_ [id_ "categories-search-results"] $
    for_ cats $ \category -> do
      a_ [class_ "category-link", href_ (categoryLink category)] $
        toHtml (category^.title)

renderCategoryStatus :: MonadIO m => Category -> HtmlT m ()
renderCategoryStatus category = do
  case category^.status of
    CategoryFinished -> return ()
    CategoryWIP -> catBanner $ do
      "This category is a work in progress"
    CategoryStub -> catBanner $ do
      "This category is a stub, contributions are welcome!"
  where
    catBanner :: MonadIO m => HtmlT m () -> HtmlT m ()
    catBanner divContent = do
      div_ [class_ "category-status-banner"] $
        strong_ divContent

renderCategoryInfo :: MonadIO m => Category -> HtmlT m ()
renderCategoryInfo category = cached (CacheCategoryInfo (category^.uid)) $ do
  let thisId = "category-info-" <> uidToText (category^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId, class_ "category-info"] $ do

    section "normal" [shown, noScriptShown] $ h2_ $ do
      -- TODO: this link shouldn't be absolute [absolute-links]
      span_ [class_ "controls"] $
        a_ [class_ "category-feed",
            href_ ("/haskell/feed/category/" <> uidToText (category^.uid))] $
          img_ [src_ "/rss-alt.svg",
                alt_ "category feed", title_ "category feed"]
      a_ [href_ (categoryLink category), class_ "category-title"] $
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
        label_ $ do
          input_ [type_ "checkbox", name_ "notes-enabled",
                  autocomplete_ "off"]
            & checkedIf (category^.notesEnabled)
          "“Notes” field enabled"
        br_ []
        input_ [type_ "submit", value_ "Save", class_ "save"]
        button "Cancel" [class_ "cancel"] $
          JS.switchSection (this, "normal" :: Text)

renderCategoryNotes :: MonadIO m => Category -> HtmlT m ()
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
        JS.switchSection (this, "editing" :: Text) <>
        JS.focusOn [(this `JS.selectSection` "editing")
                    `JS.selectChildren`
                    JS.selectClass "editor"]

    section "editing" [] $ do
      contents <- if markdownNull (category^.notes)
        then liftIO $ toMarkdownBlock <$>
               T.readFile "static/category-notes-template.md"
        else return (category^.notes)
      markdownEditor
        [rows_ "10", class_ " editor "]
        contents
        (\val -> JS.submitCategoryNotes
                   (this, category^.uid, category^.notes.mdText, val))
        (JS.switchSection (this, "normal" :: Text))
        "or press Ctrl+Enter to save"

renderCategory :: MonadIO m => Category -> HtmlT m ()
renderCategory category = cached (CacheCategory (category^.uid)) $ do
  div_ [class_ "category", id_ (categoryNodeId category)] $ do
    renderCategoryInfo category
    renderCategoryStatus category
    renderCategoryNotes category
    itemsNode <- div_ [class_ "items"] $ do
      mapM_ (renderItem category) (category^.items)
      thisNode
    textInput [
      class_ " add-item ",
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

Categories have flags that enable/disable showing some sections of the items
(currently pros/cons, ecosystem and notes); this is done because for some
items (like books, or people) “ecosystem” might not make any sense, and
pros/cons don't make sense for categories that contain diverse items.

When we change those flags (by editing category info), we want to update the
way items are shown (without reloading the page). So, if the “show ecosystem”
flag has been set and we unset it, we want to hide the ecosystem section in
all items belonging to the category. This happens in 'JS.submitCategoryInfo'.

If the category has showing pros/cons (or ecosystem, or both) disabled, we
have to render traits and ecosystem as hidden (we can't just not render them
at all, because then we wouldn't be able to un-hide them). How could we do
it? If we do it in 'renderItemTraits' or 'renderItemEcosystem', this would
mean that cached versions of traits/ecosystem/notes would have to be
rerendered whenever prosConsEnabled/ecosystemEnabled is changed. So, instead
we do a somewhat inelegant thing: we wrap traits/ecosystem/notes into yet
another <div>, and set “display:none” on it. 'JS.submitCategoryInfo' operates
on those <div>s.
-}

-- TODO: perhaps use jQuery Touch Punch or something to allow dragging items
-- instead of using arrows? Touch Punch works on mobile, too
renderItem :: MonadIO m => Category -> Item -> HtmlT m ()
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
      hiddenIf (not (category^.notesEnabled)) $
        div_ [class_ "notes-wrapper"] $
          renderItemNotes category item

-- TODO: warn when a library isn't on Hackage but is supposed to be

renderItemTitle :: (MonadIO m) => Item -> HtmlT m ()
renderItemTitle item =
  mustache "item-title" $ A.object [
    "item" A..= item ]

-- TODO: give a link to oldest available docs when the new docs aren't there
renderItemInfo :: (MonadIO m) => Category -> Item -> HtmlT m ()
renderItemInfo cat item = cached (CacheItemInfo (item^.uid)) $ do
  let itemkindname :: Text
      itemkindname = case item^.kind of
        Library{} -> "library"
        Tool{} -> "tool"
        Other{} -> "other"
  mustache "item-info" $ A.object [
    "category" A..= cat,
    "item" A..= item,
    "link_to_item" A..= itemLink cat item,
    "possible_kinds" A..= do
        kindname <- ["library", "tool", "other"]
        return $ A.object [
          "name" A..= kindname,
          "caption" A..= over _head toUpper kindname,
          "selected" A..= (itemkindname == kindname) ],
    "category_groups" A..= do
        gr <- M.keys (cat^.groups)
        return $ A.object [
          "name" A..= gr,
          "selected" A..= (Just gr == item^.group_) ],
    "item_no_group" A..= isNothing (item^.group_),
    "item_color" A..= A.object [
      "dark"  A..= hueToDarkColor (getItemHue cat item),
      "light" A..= hueToLightColor (getItemHue cat item) ] ]

renderItemDescription :: MonadIO m => Item -> HtmlT m ()
renderItemDescription item = cached (CacheItemDescription (item^.uid)) $
  mustache "item-description" $ A.object [
    "item" A..= item ]

renderItemEcosystem :: MonadIO m => Item -> HtmlT m ()
renderItemEcosystem item = cached (CacheItemEcosystem (item^.uid)) $ do
  let thisId = "item-ecosystem-" <> uidToText (item^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId, class_ "item-ecosystem"] $ do

    section "normal" [shown, noScriptShown] $ do
      strong_ "Ecosystem"
      emptySpan "0.5em"
      imgButton "edit ecosystem" "/pencil.svg"
        [style_ "width:12px;opacity:0.5", class_ " edit-item-ecosystem "] $
        JS.switchSection (this, "editing" :: Text) <>
        JS.focusOn [(this `JS.selectSection` "editing")
                    `JS.selectChildren`
                    JS.selectClass "editor"]
      div_ [class_ "notes-like"] $ do
        unless (markdownNull (item^.ecosystem)) $
          toHtml (item^.ecosystem)

    section "editing" [] $ do
      strong_ "Ecosystem"
      emptySpan "0.5em"
      imgButton "quit editing ecosystem" "/pencil.svg"
        [style_ "width:12px;opacity:0.5", class_ " edit-item-ecosystem "] $
        JS.switchSection (this, "normal" :: Text)
      markdownEditor
        [rows_ "3", class_ " editor "]
        (item^.ecosystem)
        (\val -> JS.submitItemEcosystem
                   (this, item^.uid, item^.ecosystem.mdText, val))
        (JS.switchSection (this, "normal" :: Text))
        "or press Ctrl+Enter to save"

renderItemTraits :: MonadIO m => Item -> HtmlT m ()
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
            (toMarkdownInline "")
            (\val -> JS.addPro (JS.selectUid listUid, item^.uid, val) <>
                     JS.assign val ("" :: Text))
            Nothing
            "press Ctrl+Enter or Enter to add"
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
            (toMarkdownInline "")
            (\val -> JS.addCon (JS.selectUid listUid, item^.uid, val) <>
                     JS.assign val ("" :: Text))
            Nothing
            "press Ctrl+Enter or Enter to add"
          textButton "edit off" $
            JS.switchSectionsEverywhere(this, "normal" :: Text)

renderTrait :: MonadIO m => Uid Item -> Trait -> HtmlT m ()
renderTrait itemUid trait =
  mustache "trait" $ A.object [
    "item"  A..= A.object [
        "uid" A..= itemUid ],
    "trait" A..= trait ]

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

renderItemNotes :: MonadIO m => Category -> Item -> HtmlT m ()
renderItemNotes category item = cached (CacheItemNotes (item^.uid)) $ do
  -- Don't change this ID, it's used in e.g. 'JS.expandHash'
  let thisId = "item-notes-" <> uidToText (item^.uid)
      this   = JS.selectId thisId
  editingSectionUid <- randomLongUid
  div_ [id_ thisId, class_ "item-notes"] $ do
    let notesLink = categoryLink category <> "#" <> thisId
    a_ [href_ notesLink] $
      strong_ "Notes"

    let renderTree :: Monad m => Forest ([MD.Node], Text) -> HtmlT m ()
        renderTree [] = return ()
        renderTree xs = ul_ $ do
          for_ xs $ \(Node (is, id') children) -> li_ $ do
            let handler = fromJS (JS.expandItemNotes [item^.uid])
                -- The link has to be absolute because sometimes we are
                -- looking at items from pages different from the proper
                -- category pages (e.g. if a search from the main page
                -- returned several items from different categories, and the
                -- user is looking at those items' notes without leaving the
                -- search page). Well, actually it doesn't happen yet because
                -- there's no search (or rather, there is search but it
                -- doesn't return items, only categories); however, it might
                -- start happening and then it's better to be prepared.
                fullLink = categoryLink category <> "#" <> id'
            a_ [href_ fullLink, onclick_ handler] $
              toHtmlRaw (renderMD is)
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
              JS.autosizeTextarea [JS.selectUid textareaUid] <>
              JS.focusOn [JS.selectUid textareaUid]
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

renderItemForFeed
  :: (MonadIO m)
  => Category -> Item -> HtmlT m ()
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
  T.format "if (event.keyCode == 13 || event.keyCode == 10)\
           \ {{} return false;}\n" [handler]

onCtrlEnter :: JS -> Attribute
onCtrlEnter handler = onkeydown_ $
  T.format "if ((event.keyCode == 13 || event.keyCode == 10) &&\
           \    (event.metaKey || event.ctrlKey))\
           \ {{} return false;}\n" [handler]

onEscape :: JS -> Attribute
onEscape handler = onkeydown_ $
  T.format "if (event.keyCode == 27)\
           \ {{} return false;}\n" [handler]

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
  :: MonadIO m
  => [Attribute]
  -> MarkdownBlock  -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving the contents of the editor
  -> JS             -- ^ “Cancel” handler
  -> Text           -- ^ Instruction (e.g. “press Ctrl+Enter to save”)
  -> HtmlT m ()
markdownEditor attr (view mdText -> s) submit cancel instr = do
  textareaUid <- randomLongUid
  let val = JS $ T.format "document.getElementById(\"{}\").value" [textareaUid]
  -- Autocomplete has to be turned off thanks to
  -- <http://stackoverflow.com/q/8311455>.
  textarea_ ([uid_ textareaUid,
              autocomplete_ "off",
              class_ "big fullwidth",
              onCtrlEnter (submit val),
              onEscape (JS.assign val s <> cancel) ]
             ++ attr) $
    toHtml s
  button "Save" [class_ " save "] $
    submit val
  emptySpan "6px"
  button "Cancel" [class_ " cancel "] $
    JS.assign val s <>
    cancel
  emptySpan "6px"
  span_ [class_ "edit-field-instruction"] (toHtml instr)
  a_ [href_ "/markdown", target_ "_blank"] $
    img_ [src_ "/markdown.svg", alt_ "markdown supported", class_ " markdown-supported "]

smallMarkdownEditor
  :: MonadIO m
  => [Attribute]
  -> MarkdownInline -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving the contents of the editor
  -> Maybe JS       -- ^ “Cancel” handler (if “Cancel” is needed)
  -> Text           -- ^ Instruction (e.g. “press Enter to add”)
  -> HtmlT m ()
smallMarkdownEditor attr (view mdText -> s) submit mbCancel instr = do
  textareaId <- randomLongUid
  let val = JS $ T.format "document.getElementById(\"{}\").value" [textareaId]
  textarea_ ([class_ "fullwidth", uid_ textareaId, autocomplete_ "off"] ++
             [onEnter (submit val)] ++
             [onEscape cancel | Just cancel <- [mbCancel]] ++
             attr) $
    toHtml s
  br_ []
  for_ mbCancel $ \cancel -> do
    textButton "cancel" $
      JS.assign val s <>
      cancel
  span_ [style_ "float:right"] $ do
    span_ [class_ "edit-field-instruction"] (toHtml instr)
    a_ [href_ "/markdown", target_ "_blank"] $
      img_ [src_ "/markdown.svg", alt_ "markdown supported", class_ " markdown-supported "]

thisNode :: MonadIO m => HtmlT m JQuerySelector
thisNode = do
  uid' <- randomLongUid
  -- If the class name ever changes, fix 'JS.moveNodeUp' and
  -- 'JS.moveNodeDown'.
  span_ [uid_ uid', class_ "dummy"] mempty
  return (JS.selectParent (JS.selectUid uid'))

itemNodeId :: Item -> Text
itemNodeId item = "item-" <> uidToText (item^.uid)

categoryNodeId :: Category -> Text
categoryNodeId category = "category-" <> uidToText (category^.uid)

-- TODO: another absolute link to get rid of [absolute-links]
categoryLink :: Category -> Url
categoryLink category = "/haskell/" <> categorySlug category

itemLink :: Category -> Item -> Url
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

{-
TODO: warn about how one shouldn't write @foo("{{bar}}")@ in templates,
because a newline in 'bar' directly after the quote will mess things
up. Write @foo({{{%js bar}}})@ instead.
-}
mustache :: MonadIO m => PName -> A.Value -> HtmlT m ()
mustache f v = do
  let functions = M.fromList [
        ("selectIf", \[x] -> if x == A.Bool True
            then return (A.String "selected")
            else return A.Null),
        ("js", \[x] -> return $
            A.String . T.toStrict . TL.decodeUtf8 . A.encode $ x),
        ("trace", \xs -> do
            mapM_ (BS.putStrLn . A.encodePretty) xs
            return A.Null) ]
  widgets <- readWidgets
  let templates = [(tname, t) | (HTML_ tname, t) <- widgets]
  when (null templates) $
    error "View.mustache: no HTML templates found in templates/"
  parsed <- for templates $ \(tname, t) -> do
    let pname = fromString (T.unpack tname)
    case compileMustacheText pname (T.toLazy t) of
      Left e -> error $ printf "View.mustache: when parsing %s: %s"
                               tname (parseErrorPretty e)
      Right template -> return template
  let combined = (Semigroup.sconcat (NonEmpty.fromList parsed)) {
                   templateActual = f }
  (rendered, warnings) <- liftIO $ renderMustacheM functions combined v
  when (not (null warnings)) $
    error $ printf "View.mustache: warnings when rendering %s:\n%s"
                   (unPName f) (unlines warnings)
  toHtmlRaw rendered

data SectionType
  = HTML_ Text | JS_ | CSS_ | Description_ | Note_ Text

-- | Used to turn collected section lines back into a section.
--
-- * Trims surrounding blank lines
-- * Doesn't append a newline when there's only one line
--   (useful for inline partials)
unlinesSection :: [Text] -> Text
unlinesSection = unlines' . dropWhile T.null . dropWhileEnd T.null
  where
    unlines' []  = ""
    unlines' [x] = x
    unlines' xs  = T.unlines xs

readWidget :: MonadIO m => FilePath -> m [(SectionType, Text)]
readWidget fp = liftIO $ do
  s <- T.readFile fp
  let isDivide line = (T.all (== '=') line || T.all (== '-') line) &&
                      T.length line >= 20
  let go (x:y:[]) = [(T.strip (last x), unlinesSection y)]
      go (x:y:xs) = (T.strip (last x), unlinesSection (init y)) : go (y : xs)
      go _ = error $ "View.readWidget: couldn't read " ++ fp
  let sections = go (splitWhen isDivide (T.lines s))
  let sectionTypeP :: Parser SectionType
      sectionTypeP = choice [
        do string "HTML"
           HTML_ <$> choice [
             string ": " >> (T.pack <$> some anyChar),
             return (T.pack (takeBaseName fp)) ],
        string "JS" $> JS_,
        string "CSS" $> CSS_,
        string "Description" $> Description_,
        do string "Note ["
           Note_ . T.pack <$> someTill anyChar (char ']') ]
  let parseSectionType t = case parse (sectionTypeP <* eof) fp t of
        Right x -> x
        Left e -> error $ printf "invalid section name: '%s'\n%s"
                          t (parseErrorPretty e)
  return $ over (each._1) parseSectionType sections

readWidgets :: MonadIO m => m [(SectionType, Text)]
readWidgets = liftIO $ do
  let isWidget = F.extension F.==? ".widget"
  files <- F.find F.always isWidget "templates/"
  concat <$> mapM readWidget files

getJS :: MonadIO m => m Text
getJS = do
  widgets <- readWidgets
  let js = [t | (JS_, t) <- widgets]
  return (T.concat js)

getCSS :: MonadIO m => m Text
getCSS = do
  widgets <- readWidgets
  let css = [t | (CSS_, t) <- widgets]
  return (T.concat css)
