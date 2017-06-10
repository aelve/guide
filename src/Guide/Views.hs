{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}


{- |
All views and all rendering logic.
-}
module Guide.Views
(
  module X,

  -- * Pages
  renderRoot,
  renderAdmin,
  renderDonate,
  renderCategoryPage,
  renderHaskellRoot,

  -- * Helpers
  renderEdits,
  renderStaticMd,
  renderSearchResults,
  renderCategoryList,
)
where


-- Reexporting children modules
import Guide.Views.Auth as X
import Guide.Views.Page as X
import Guide.Views.Item as X
import Guide.Views.Category as X

import Imports

-- Text
import qualified Data.Text.All as T
import NeatInterpolation
-- Web
import Lucid hiding (for_)
-- Network
import Data.IP
-- Time
import Data.Time.Format.Human
-- Mustache (templates)
import qualified Data.Aeson as A

import Guide.Config
import Guide.State
import Guide.Types
import Guide.Search
import Guide.Utils
import Guide.JS (JS(..))
import qualified Guide.JS as JS
import Guide.Markdown
import Guide.Diff hiding (DiffChunk)
import qualified Guide.Diff as Diff
import Guide.Cache
import Guide.Views.Utils


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

-- | Render the subtitle below the “Aelve Guide” header that is present on
-- every page.
renderSubtitle :: (MonadReader Config m) => HtmlT m ()
renderSubtitle =
  div_ [class_ "subtitle"] $ do
    "alpha version • don't post on Reddit yet"
    lift (asks _discussLink) >>= \case
      Nothing -> return ()
      Just l  -> " • " >> mkLink "discuss the site" l

-- | Render the main page (<https://guide.aelve.com>).
renderRoot :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderRoot = do
  wrapPage "Aelve Guide" $ do
    h1_ "Aelve Guide"
    renderSubtitle
    h2_ (mkLink "Haskell" "/haskell")

-- | Render the administration panel (</admin>).
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
      renderEdits globalState $
        take 20 $ (map (,Nothing) (globalState ^. pendingEdits))

-- | Render statistics on the admin page.
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

-- | Group edits by IP and render them.
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
  h1_ $ toHtml @Text $
    "Pending edits (IPs: "#|ipNum|#", blocks: "#|length editBlocks|#")"
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

-- | Render a single edit.
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
      "added item " >> printItem _itemId
      " (initially called " >> quote (toHtml name') >> ")"
      " to category " >> printCategory catId
    Edit'AddPro itemId _traitId content' -> do
      p_ $ "added pro to item " >> printItem itemId
      pre_ $ code_ $ toHtml content'
    Edit'AddCon itemId _traitId content' -> do
      p_ $ "added con to item " >> printItem itemId
      pre_ $ code_ $ toHtml content'

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
      renderDiff oldNotes newNotes
    Edit'ChangeCategoryEnabledSections catId toEnable toDisable -> do
      let sectName ItemProsConsSection  = "pros/cons"
          sectName ItemEcosystemSection = "ecosystem"
          sectName ItemNotesSection     = "notes"
      let list = toHtml . T.intercalate ", "
      unless (null toEnable) $
        p_ $ "enabled " >>
             strong_ (list (map sectName (toList toEnable))) >>
             " for category " >> printCategory catId
      unless (null toDisable) $
        p_ $ "disabled " >>
             strong_ (list (map sectName (toList toDisable))) >>
             " for category " >> printCategory catId

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
      renderDiff oldDescr newDescr
    Edit'SetItemNotes itemId oldNotes newNotes -> do
      p_ $ (if T.null oldNotes then "added" else "changed") >>
           " notes of item " >> printItem itemId
      renderDiff oldNotes newNotes
    Edit'SetItemEcosystem itemId oldEcosystem newEcosystem -> do
      p_ $ (if T.null oldEcosystem then "added" else "changed") >>
           " ecosystem of item " >> printItem itemId
      renderDiff oldEcosystem newEcosystem

    -- Change trait properties
    Edit'SetTraitContent itemId _traitId oldContent newContent -> do
      p_ $ (if T.null oldContent then "added" else "changed") >>
           " trait of item " >> printItem itemId >>
           " from category " >> printCategory (findItem itemId ^. _1.uid)
      renderDiff oldContent newContent

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
      pre_ $ code_ $ toHtml $ trait^.content

    -- Other
    Edit'MoveItem itemId direction -> p_ $ do
      "moved item " >> printItem itemId
      if direction then " up" else " down"
    Edit'MoveTrait itemId traitId direction -> do
      let (_, item, trait) = findTrait itemId traitId
      p_ $ "moved trait of item " >> quote (toHtml (item^.name)) >>
           if direction then " up" else " down"
      pre_ $ code_ $ toHtml $ trait^.content

renderDiff :: Monad m => Text -> Text -> HtmlT m ()
renderDiff old new =
    table_ $ tr_ $
      if | T.null old -> renderOne new
         | T.null new -> renderOne old
         | otherwise  -> renderBoth
  where
    cell = td_ . pre_ . code_
    renderOne s = cell (toHtml s)
    renderBoth = do
      let Diff{..} = diff old new
      cell $ do
        "[...] " >> toHtml (mconcat (takeEnd 10 diffContextAbove))
        mapM_ renderChunk diffLeft
        toHtml (mconcat (take 10 diffContextBelow)) >> " [...]"
      cell $ do
        "[...] " >> toHtml (mconcat (takeEnd 10 diffContextAbove))
        mapM_ renderChunk diffRight
        toHtml (mconcat (take 10 diffContextBelow)) >> " [...]"
    --
    renderChunk (Diff.Added   x) = ins_ (toHtml (showNewlines x))
    renderChunk (Diff.Deleted x) = del_ (toHtml (showNewlines x))
    renderChunk (Diff.Plain   x) = toHtml x
    --
    showNewlines x =
      let
        (pref, x')  = T.span   (== '\n') x
        (x'', suff) = tSpanEnd (== '\n') x'
      in
        T.replicate (T.length pref) "⏎\n" <> x'' <>
        T.replicate (T.length suff) "⏎\n"
    --
    tSpanEnd p = over both T.reverse . swap . T.span p . T.reverse

-- TODO: use “data Direction = Up | Down” for directions instead of Bool

-- | Render the header on the </haskell> subpage: “Aelve Guide | Haskell”.
haskellHeader :: (MonadReader Config m) => HtmlT m ()
haskellHeader = div_ [id_ "header"] $ do
  div_ $ do
    h1_ $ mkLink ("Aelve Guide " >> span_ "| Haskell") "/haskell"
    renderSubtitle
  div_ [class_ "auth-link-container"] $ do
    a_ [href_ "/auth"] "login/logout"

-- | Render </haskell>.
renderHaskellRoot
  :: (MonadIO m, MonadReader Config m)
  => GlobalState -> Maybe Text -> HtmlT m ()
renderHaskellRoot globalState mbSearchQuery =
  wrapPage "Aelve Guide | Haskell" $ do
    onPageLoad $ JS.expandHash ()
    haskellHeader
    renderNoScriptWarning
    renderSearch mbSearchQuery
    textInput [
      placeholder_ "add a category",
      class_ "add-category",
      autocomplete_ "off",
      onEnter $ JS.addCategoryAndRedirect [inputValue] ]
    case mbSearchQuery of
      Nothing -> renderCategoryList (globalState^.categories)
      Just query' -> renderSearchResults (search query' globalState)
    -- TODO: maybe add a button like “give me random category that is
    -- unfinished”

-- | Render a category.
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

-- | Render a warning that is displayed to the user when they don't have
-- Javascript enabled.
renderNoScriptWarning :: Monad m => HtmlT m ()
renderNoScriptWarning =
  noscript_ $ div_ [id_ "noscript-message"] $
    toHtml $ toMarkdownBlock [text|
      You have Javascript disabled! This site works fine without
      Javascript, but since all editing needs Javascript to work,
      you won't be able to edit anything.
      |]

-- | Render the </donate> page.
renderDonate
  :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderDonate = wrapPage "Donate to Artyom" $ do
  toHtmlRaw =<< liftIO (readFile "static/donate.html")

-- | Render any page that is a static piece of Markdown.
renderStaticMd
  :: (MonadIO m, MonadReader Config m)
  => Text -> String -> HtmlT m ()
renderStaticMd t fn = wrapPage t $
  toHtml . toMarkdownBlock =<< liftIO (T.readFile ("static/" ++ fn))

-- | Include all the necessary things into a page – header, footer, etc.
wrapPage
  :: (MonadIO m, MonadReader Config m)
  => Text                              -- ^ Page title
  -> HtmlT m ()
  -> HtmlT m ()
wrapPage pageTitle' page = doctypehtml_ $ do
  head_ $ do
    title_ (toHtml pageTitle')
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
    includeJS "/js/bundle.js"
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

-- | Render the search box.
renderSearch :: (MonadIO m) => Maybe Text -> HtmlT m ()
renderSearch mbSearchQuery =
  mustache "search" $ A.object [
    "query" A..= mbSearchQuery ]

-- | Render list of categories on the main page (the one with category groups
-- and categories in it).
--
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

-- | Render a <div> with search results.
renderSearchResults :: Monad m => [SearchResult] -> HtmlT m ()
renderSearchResults rs = do
  div_ [id_ "search-results"] $
    mapM_ renderSearchResult rs

-- | Render one search result.
renderSearchResult :: Monad m => SearchResult -> HtmlT m ()
renderSearchResult r = do
  div_ [class_ "search-result"] $
    case r of
      SRCategory cat -> do
        a_ [class_ "category-link", href_ (categoryLink cat)] $
          toHtml (cat^.title)
        div_ [class_ "category-description notes-like"] $
          toHtml (extractPreface $ toMarkdownTree "" $ cat^.notes.mdText)
      SRItem cat item -> do
        a_ [class_ "category-link in-item-sr", href_ (categoryLink cat)] $
          toHtml (cat^.title)
        span_ [class_ "breadcrumb"] "»"
        a_ [class_ "item-link", href_ (itemLink cat item)] $
          toHtml (item^.name)
        div_ [class_ "description notes-like"] $
          toHtml (item^.description)
      SRItemEcosystem cat item -> do
        a_ [class_ "category-link in-item-sr", href_ (categoryLink cat)] $
          toHtml (cat^.title)
        span_ [class_ "breadcrumb"] "»"
        a_ [class_ "item-link", href_ (itemLink cat item)] $
          toHtml (item^.name)
        span_ [class_ "item-link-addition"] "'s ecosystem"
        div_ [class_ "ecosystem notes-like"] $
          toHtml (item^.ecosystem)

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

-- TODO: warn when a library isn't on Hackage but is supposed to be

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
