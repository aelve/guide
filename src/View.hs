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
  renderUnwrittenRules,

  -- * Tracking
  renderTracking,

  -- * Methods
  renderHelp,
  -- ** Categories
  renderCategoryList,
  renderCategory,
  renderCategoryTitle,
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
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Monads and monad transformers
import Control.Monad.IO.Class
import Control.Monad.Reader
-- Containers
import qualified Data.Map as M
-- Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import NeatInterpolation
-- Web
import Lucid hiding (for_)
-- Time
import Data.Time.Format.Human

-- Local
import Config
import Types
import Utils
import JS (JS(..), JQuerySelector)
import qualified JS
import Markdown


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

A lot of things (help, notes, etc) can be expanded/collapsed by pressing a button. Similarly, pressing “edit” replaces rendered text with a textbox, or adds buttons to pros/cons. All this is done with sections and show/hide.

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

renderRoot :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderRoot = do
  wrapPage "Aelve Guide" $ do
    h1_ "Aelve Guide"
    h2_ (a_ [href_ "/haskell"] "Haskell")

-- TODO: show a “category not found” page

renderAdmin :: MonadIO m => GlobalState -> [(Edit, EditDetails)] -> HtmlT m ()
renderAdmin globalState edits = do
  head_ $ do
    includeJS "/js.js"
    includeJS "/jquery-2.2.0.min.js"
    includeCSS "/markup.css"
    includeCSS "/admin.css"
    title_ "admin – Aelve Guide"
    meta_ [name_ "viewport",
           content_ "width=device-width, initial-scale=1.0, user-scalable=yes"]

  body_ $ do
    h1_ "Pending edits"
    renderEdits globalState (map (,Nothing) edits)

-- TODO: when showing Edit'DeleteCategory, show the amount of items in that
-- category and titles of items themselves

-- | Group edits by IP and render them
renderEdits
  :: MonadIO m
  => GlobalState
  -> [((Edit, EditDetails), Maybe String)]
  -> HtmlT m ()
renderEdits globalState edits = do
  let editBlocks = groupBy (equating (editIP . snd . fst)) edits
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
    ul_ $ do
      for_ editBlock $ \((edit, EditDetails{..}), mbErr) -> li_ $ do
        editNode <- thisNode
        p_ $ do
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
    Edit'SetCategoryNotes catId oldNotes newNotes -> do
      p_ $ "changed notes of category " >> printCategory catId
      table_ $ tr_ $ do
        td_ $ blockquote_ $ toHtml (renderMarkdownBlock oldNotes)
        td_ $ blockquote_ $ toHtml (renderMarkdownBlock newNotes)

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
  :: (MonadIO m, MonadReader Config m)
  => GlobalState -> Maybe Text -> HtmlT m ()
renderHaskellRoot globalState mbSearchQuery =
  wrapPage "Aelve Guide" $ do
    -- TODO: [very-easy] this header looks bad when the page is narrow, it
    -- should be fixed in css.css by adding line-height to it
    case mbSearchQuery of
      Nothing -> h1_ "The Haskeller's guide"
      -- A search page isn't the main page, so we need a link to the main page
      Just _  -> h1_ (a_ [href_ "/haskell"] "The Haskeller's guide")
    renderNoScriptWarning
    renderHelp
    form_ $ do
      input_ [type_ "text", name_ "q", id_ "search", placeholder_ "search",
              value_ (fromMaybe "" mbSearchQuery)]
    textInput [
      placeholder_ "add a category",
      autocomplete_ "off",
      onEnter $ JS.addCategory (JS.selectId "categories", inputValue) <>
                clearInput ]
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
        renderCategoryList rankedCategories
    -- TODO: maybe add a button like “give me random category that is
    -- unfinished”

renderCategoryPage
  :: (MonadIO m, MonadReader Config m) => Category -> HtmlT m ()
renderCategoryPage category =
  wrapPage (category^.title <> " – Aelve Guide") $ do
    -- TODO: [very-easy] this header looks bad when the page is narrow, it
    -- should be fixed in css.css by adding line-height to it
    -- TODO: another absolute link [absolute-links]
    h1_ (a_ [href_ "/haskell"] "The Haskeller's guide")
    renderNoScriptWarning
    renderHelp
    renderCategory category

renderNoScriptWarning :: MonadIO m => HtmlT m ()
renderNoScriptWarning =
  noscript_ $ div_ [id_ "noscript-message"] $
    toHtml $ renderMarkdownBlock [text|
      You have Javascript disabled! This site works fine without
      Javascript, but since all editing needs Javascript to work,
      you won't be able to edit anything.
      |]

-- TODO: when submitting a text field, gray it out (but leave it selectable)
-- until it's been submitted

renderTracking :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderTracking = do
  trackingEnabled <- lift (asks _trackingEnabled)
  when trackingEnabled $ do
    tracking <- liftIO $ T.readFile "static/tracking.html"
    toHtmlRaw tracking

renderDonate :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderDonate = wrapPage "Donate to Artyom" $ do
  toHtmlRaw =<< liftIO (readFile "static/donate.html")

renderUnwrittenRules :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderUnwrittenRules = wrapPage "Unwritten rules" $ do
  toHtml . renderMarkdownBlock =<<
    liftIO (T.readFile "static/unwritten-rules.md")

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
    includeJS "/jquery-2.2.0.min.js"
    -- See Note [autosize]
    includeJS "/autosize-3.0.15.min.js"
    onPageLoad (JS "autosize($('textarea'));")
    -- The order is important – markup.css overrides some rules from
    -- highlight.css (e.g. div.sourceCode), css.css overrides the rule for
    -- a.anchor from markup.css.
    --
    -- TODO: maybe use !important or something instead?
    includeCSS "/highlight.css"
    includeCSS "/markup.css"
    includeCSS "/css.css"
    -- Include definitions of all Javascript functions that we have defined
    -- in this file. (This isn't an actual file, so don't look for it in the
    -- static folder – it's generated and served in 'otherMethods'.)
    includeJS "/js.js"
    renderTracking
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
    div_ [id_ "main"] $
      page
    div_ [id_ "footer"] $ do
      "made by " >> a_ [href_ "https://artyom.me"] "Artyom"
      emptySpan "2em"
      a_ [href_ "https://github.com/aelve/guide"] "source"
      emptySpan "2em"
      a_ [href_ "https://github.com/aelve/guide/issues"] "report an issue"
      emptySpan "2em"
      a_ [href_ "/donate"] "donate"
      sup_ [style_ "font-size:50%"] "I don't have a job"

-- TODO: allow archiving items if they are in every way worse than the rest,
-- or something (but searching should still be possible)

-- TODO: add a list for “interesting libraries, but too lazy to describe, so
-- somebody describe them for me”

renderHelp :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderHelp = do
  div_ [id_ "help"] $ do

    -- If you're going to change section names, look at 'JS.showHelp' and
    -- 'JS.hideHelp'
    section "collapsed" [shown] $ do
      textButton "show help" $
        JS.showHelp (JS.selectId "help", helpVersion)

    section "expanded" [noScriptShown] $ do
      textButton "hide help" $
        JS.hideHelp (JS.selectId "help", helpVersion)
      -- Don't forget to change 'helpVersion' when the text changes
      -- substantially and you think the users should reread it
      help <- liftIO $ T.readFile "static/help.md"
      toHtml $ renderMarkdownBlock help
      -- Replicating “hide help” so that it would be more noticeable
      p_ $ do
        let handler =
              fromJS (JS.hideHelp (JS.selectId "help", helpVersion)) <>
              "return false;"
        "If you're finished reading, "
        a_ [href_ "#", onclick_ handler] "hide this message"
        "."

  onPageLoad $ JS.showOrHideHelp (JS.selectId "help", helpVersion)

helpVersion :: Int
helpVersion = 3

-- TODO: when conflicts happen, maybe create an alert like “The thing you're
-- editing has been edited in the meantime. Here is a link with a diff of
-- your variant and the other person's variant. Please merge the changes
-- manually and submit them again, or press this button and we'll merge the
-- changes for you (don't worry, it's not a big deal for us). Thanks!”

-- TODO: automatic merge should be possible too (e.g. if the changes are in
-- different paragraphs)

renderCategoryList :: MonadIO m => [Category] -> HtmlT m ()
renderCategoryList cats =
  div_ [id_ "categories"] $
    mapM_ renderCategory cats

renderCategoryTitle :: Monad m => Category -> HtmlT m ()
renderCategoryTitle category = do
  let thisId = "category-title-" <> uidToText (category^.uid)
      this   = JS.selectId thisId
  h2_ [id_ thisId, class_ "category-title"] $ do
    -- TODO: this link shouldn't be absolute [absolute-links]
    span_ [class_ "controls"] $
      a_ [href_ ("/haskell/feed/category/" <> uidToText (category^.uid))] $
        img_ [src_ "/rss-alt.svg",
              alt_ "category feed", title_ "category feed"]

    sectionSpan "normal" [shown, noScriptShown] $ do
      -- TODO: this link shouldn't be absolute [absolute-links]
      a_ [href_ ("/haskell/" <> categorySlug category)] $
        toHtml (category^.title)
      emptySpan "1em"
      textButton "edit" $
        JS.switchSection (this, "editing" :: Text)
      emptySpan "1em"
      -- TODO: when on the category page, deleting the category should
      -- redirect to the main page
      textButton "delete" $
        JS.deleteCategory (category^.uid, categoryNode category)

    sectionSpan "editing" [] $ do
      textInput [
        value_ (category^.title),
        autocomplete_ "off",
        onEnter $
          JS.submitCategoryTitle (this, category^.uid, inputValue)]
      emptySpan "1em"
      textButton "cancel" $
        JS.switchSection (this, "normal" :: Text)

renderCategoryNotes :: MonadIO m => Category -> HtmlT m ()
renderCategoryNotes category = do
  let thisId = "category-notes-" <> uidToText (category^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId] $ do

    section "normal" [shown, noScriptShown] $ do
      if category^.notes == ""
        then p_ "write something here!"
        else toHtml (category^.notes)
      textButton "edit description" $
        JS.switchSection (this, "editing" :: Text)

    section "editing" [] $ do
      contents <- if category^.notes == ""
        then liftIO $ renderMarkdownBlock <$>
               T.readFile "static/category-notes-template.md"
        else return (category^.notes)
      markdownEditor
        [rows_ "10"]
        contents
        (\val -> JS.submitCategoryNotes (this, category^.uid, val))
        (JS.switchSection (this, "normal" :: Text))

renderCategory :: MonadIO m => Category -> HtmlT m ()
renderCategory category =
  div_ [class_ "category", id_ (categoryNodeId category)] $ do
    renderCategoryTitle category
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

-- TODO: perhaps use jQuery Touch Punch or something to allow dragging items
-- instead of using arrows? Touch Punch works on mobile, too
renderItem :: MonadIO m => Category -> Item -> HtmlT m ()
renderItem category item =
  -- The id is used for links in feeds, and for anchor links
  div_ [id_ (itemNodeId item), class_ "item"] $ do
    renderItemInfo category item
    -- TODO: replace “edit description” with a big half-transparent pencil
    -- to the left of it
    let bg = hueToLightColor $ getItemHue category item
    div_ [class_ "item-body", style_ ("background-color:" <> bg)] $ do
      renderItemDescription item
      renderItemTraits item
      renderItemEcosystem item
      -- TODO: add a separator here? [very-easy]
      renderItemNotes item

-- TODO: warn when a library isn't on Hackage but is supposed to be

renderItemTitle :: Monad m => Item -> HtmlT m ()
renderItemTitle item = do
  let hackageLink x = "https://hackage.haskell.org/package/" <> x
  case item^.kind of
    -- If the library is on Hackage, the title links to its Hackage
    -- page; otherwise, it doesn't link anywhere. Even if the link
    -- field is present, it's going to be rendered as “(site)”, not
    -- linked in the title.
    Library hackageName' -> do
      case hackageName' of
        Just x  -> a_ [href_ (hackageLink x)] (toHtml (item^.name))
        Nothing -> toHtml (item^.name)
      case item^.link of
        Just l  -> " (" >> a_ [href_ l] "site" >> ")"
        Nothing -> return ()
    -- For tools, it's the opposite – the title links to the item site
    -- (if present), and there's a separate “(Hackage)” link if the
    -- tool is on Hackage.
    Tool hackageName' -> do
      case item^.link of
        Just l  -> a_ [href_ l] (toHtml (item^.name))
        Nothing -> toHtml (item^.name)
      case hackageName' of
        Just x  -> " (" >> a_ [href_ (hackageLink x)] "Hackage" >> ")"
        Nothing -> return ()
    -- And now everything else
    Other -> do
      case item^.link of
        Just l  -> a_ [href_ l] (toHtml (item^.name))
        Nothing -> toHtml (item^.name)

-- TODO: give a link to oldest available docs when the new docs aren't there
renderItemInfo :: MonadIO m => Category -> Item -> HtmlT m ()
renderItemInfo cat item = do
  let bg = hueToDarkColor $ getItemHue cat item
  let thisId = "item-info-" <> uidToText (item^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId, class_ "item-info",
        style_ ("background-color:" <> bg)] $ do

    section "normal" [shown, noScriptShown] $ do
      -- TODO: [very-easy] move this style_ into css.css
      span_ [style_ "font-size:150%"] $ do
        -- TODO: absolute links again [absolute-links]
        let link' = format "/haskell/{}#{}" (categorySlug cat, itemNodeId item)
        a_ [class_ "anchor", href_ link'] "#"
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
        -- TODO: link to Stackage too
        -- TODO: should check for Stackage automatically

    section "editing" [] $ do
      let selectedIf p x = if p then with x [selected_ "selected"] else x
      -- When the info/header node changes its group (and is hence
      -- recolored), item's body has to be recolored too
      let bodyNode = JS.selectChildren (JS.selectParent this)
                                       (JS.selectClass "item-body")
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
          select_ [name_ "kind"] $ do
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
          --
          -- TODO: when a new group is created, add it to all other lists in
          -- forms in the category
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

-- TODO: categories that don't directly compare libraries but just list all
-- libraries about something (e.g. Yesod plugins, or whatever)

-- TODO: categories without items (e.g. “web dev”) that list links to other
-- categories

renderItemDescription :: MonadIO m => Item -> HtmlT m ()
renderItemDescription item = do
  let thisId = "item-description-" <> uidToText (item^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId, class_ "item-description"] $ do

    section "normal" [shown, noScriptShown] $ do
      if item^.description == ""
        then p_ "write something here!"
        else toHtml (item^.description)
      textButton "edit description" $
        JS.switchSection (this, "editing" :: Text)

    section "editing" [] $
      markdownEditor
        [rows_ "10"]
        (item^.description)
        (\val -> JS.submitItemDescription (this, item^.uid, val))
        (JS.switchSection (this, "normal" :: Text))

renderItemEcosystem :: MonadIO m => Item -> HtmlT m ()
renderItemEcosystem item = do
  let thisId = "item-ecosystem-" <> uidToText (item^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId, class_ "item-ecosystem"] $ do
    strong_ "Ecosystem"
    emptySpan "0.5em"
    imgButton "edit ecosystem" "/pencil.svg"
      [style_ "width:12px;opacity:0.5"] $
      JS.switchSection (this, "editing" :: Text)

    section "normal" [shown, noScriptShown] $ do
      unless (item^.ecosystem == "") $
        toHtml (item^.ecosystem)

    section "editing" [] $
      markdownEditor
        [rows_ "3"]
        (item^.ecosystem)
        (\val -> JS.submitItemEcosystem (this, item^.uid, val))
        (JS.switchSection (this, "normal" :: Text))

-- TODO: change MonadIO to MonadRandom mostly everywhere

renderItemTraits :: MonadIO m => Item -> HtmlT m ()
renderItemTraits item = do
  div_ [class_ "item-traits"] $ do
    this <- thisNode
    div_ [class_ "traits-groups-container"] $ do
      div_ [class_ "traits-group"] $ do
        strong_ "Pros"
        -- We can't use 'thisNode' inside <ul> because it creates a <span>
        -- and only <li> elements can be children of <ul>
        listUid <- randomLongUid
        ul_ [uid_ listUid] $
          mapM_ (renderTrait (item^.uid)) (item^.pros)
        section "editable" [] $
          smallMarkdownEditor
            [rows_ "3", placeholder_ "add pro"]
            ""
            (\val -> JS.addPro (JS.selectUid listUid, item^.uid, val) <>
                     JS.assign val ("" :: Text))
            Nothing
      div_ (emptySpan "1em")
      div_ [class_ "traits-group"] $ do
        strong_ "Cons"
        -- TODO: [easy] maybe add a line here?
        listUid <- randomLongUid
        ul_ [uid_ listUid] $
          mapM_ (renderTrait (item^.uid)) (item^.cons)
        section "editable" [] $
          smallMarkdownEditor
            [rows_ "3", placeholder_ "add con"]
            ""
            (\val -> JS.addCon (JS.selectUid listUid, item^.uid, val) <>
                     JS.assign val ("" :: Text))
            Nothing
    section "normal" [shown, noScriptShown] $ do
      textButton "edit pros/cons" $
        -- Switches sections in *all* traits
        JS.switchSectionsEverywhere(this, "editable" :: Text)
    section "editable" [] $ do
      textButton "edit off" $
        JS.switchSectionsEverywhere(this, "normal" :: Text)

renderTrait :: MonadIO m => Uid Item -> Trait -> HtmlT m ()
renderTrait itemId trait = do
  let thisId = "trait-" <> uidToText (trait^.uid)
      this   = JS.selectId thisId
  li_ [id_ thisId] $ do

    sectionSpan "normal editable" [shown, noScriptShown] $ do
      toHtml (trait^.content)

    sectionSpan "editable" [] $ do
      br_ []
      imgButton "move trait up" "/arrow-thick-top.svg" [width_ "12"] $
        JS.moveTraitUp (itemId, trait^.uid, this)
      imgButton "move trait down" "/arrow-thick-bottom.svg" [width_ "12"] $
        JS.moveTraitDown (itemId, trait^.uid, this)
      -- TODO: these 3 icons in a row don't look nice
      imgButton "delete trait" "/x.svg" [width_ "12"] $
        JS.deleteTrait (itemId, trait^.uid, this)
      textButton "edit" $
        JS.switchSection (this, "editing" :: Text)

    section "editing" [] $ do
      smallMarkdownEditor
        [rows_ "5"]
        (trait^.content)
        (\val -> JS.submitTrait (this, itemId, trait^.uid, val))
        (Just (JS.switchSection (this, "editable" :: Text)))

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
renderItemNotes :: MonadIO m => Item -> HtmlT m ()
renderItemNotes item = do
  let thisId = "item-notes-" <> uidToText (item^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId, class_ "item-notes"] $ do
    -- TODO: this duplicates code from renderCategoryNotes, try to reduce
    -- duplication

    section "collapsed" [shown] $ do
      textButton "show notes/examples" $
        JS.switchSection (this, "expanded" :: Text)

    section "expanded" [noScriptShown] $ do
      let buttons = do
            textButton "hide notes" $
              JS.switchSection (this, "collapsed" :: Text)
            emptySpan "1em"
            textButton "edit notes" $
              JS.switchSection (this, "editing" :: Text)
      buttons
      if item^.notes == ""
        then p_ "add something!"
        else toHtml (item^.notes) >>
             buttons
      -- TODO: [easy] the lower “hide notes” should scroll back to item when
      -- the notes are closed (but don't scroll if it's already visible after
      -- the notes have been hidden)

    section "editing" [] $ do
      contents <- if item^.notes == ""
        then liftIO $ renderMarkdownBlock <$>
               T.readFile "static/item-notes-template.md"
        else return (item^.notes)
      markdownEditor
        [rows_ "10"]
        contents
        (\val -> JS.submitItemNotes (this, item^.uid, val))
        (JS.switchSection (this, "expanded" :: Text))

-- TODO: a shortcut for editing (when you press Ctrl-something, whatever was
-- selected becomes editable)

renderItemForFeed :: Monad m => Item -> HtmlT m ()
renderItemForFeed item = do
  h1_ $ renderItemTitle item
  when (item^.description /= "") $
    toHtml (item^.description)
  h2_ "Pros"
  ul_ $ mapM_ (p_ . li_ . toHtml . view content) (item^.pros)
  h2_ "Cons"
  ul_ $ mapM_ (p_ . li_ . toHtml . view content) (item^.cons)
  when (item^.ecosystem /= "") $ do
    h2_ "Ecosystem"
    toHtml (item^.ecosystem)
  when (item^.notes /= "") $ do
    h2_ "Notes"
    toHtml (item^.notes)

-- Utils

onPageLoad :: Monad m => JS -> HtmlT m ()
onPageLoad js = script_ $ format "$(document).ready(function(){{}});" [js]

emptySpan :: Monad m => Text -> HtmlT m ()
emptySpan w = span_ [style_ ("margin-left:" <> w)] mempty

-- Use inputValue to get the value (works with input_ and textarea_)
onEnter :: JS -> Attribute
onEnter handler = onkeydown_ $
  format "if (event.keyCode == 13) {{} return false;}" [handler]

textInput :: Monad m => [Attribute] -> HtmlT m ()
textInput attrs = input_ (type_ "text" : attrs)

inputValue :: JS
inputValue = JS "this.value"

clearInput :: JS
clearInput = JS "this.value = '';"

onFormSubmit :: (JS -> JS) -> Attribute
onFormSubmit f = onsubmit_ $ format "{} return false;" [f (JS "this")]

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

markdownEditor
  :: MonadIO m
  => [Attribute]
  -> MarkdownBlock  -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving the contents of the editor
  -> JS             -- ^ “Cancel” handler
  -> HtmlT m ()
markdownEditor attr (markdownBlockText -> s) submit cancel = do
  textareaUid <- randomLongUid
  -- Autocomplete has to be turned off thanks to
  -- <http://stackoverflow.com/q/8311455>.
  textarea_ ([uid_ textareaUid, autocomplete_ "off", class_ "big fullwidth"]
             ++ attr) $
    toHtml s
  let val = JS $ format "document.getElementById(\"{}\").value" [textareaUid]
  button "Save" [] $
    submit val
  emptySpan "6px"
  button "Cancel" [] $
    JS.assign val s <>
    cancel
  emptySpan "6px"
  "Markdown"
  emptySpan "6px"
  -- TODO: this jumps around when there's a lot of text, need to somehow
  -- prevent jumping
  let checkHandler = fromJS $
        JS.setMonospace (JS.selectUid textareaUid, JS "this.checked")
  label_ $ do
    input_ [type_ "checkbox", name_ "monospace", onchange_ checkHandler]
    "monospace editor"

smallMarkdownEditor
  :: MonadIO m
  => [Attribute]
  -> MarkdownInline -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving the contents of the editor
  -> Maybe JS       -- ^ “Cancel” handler (if “Cancel” is needed)
  -> HtmlT m ()
smallMarkdownEditor attr (markdownInlineText -> s) submit mbCancel = do
  textareaId <- randomLongUid
  let val = JS $ format "document.getElementById(\"{}\").value" [textareaId]
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

thisNode :: MonadIO m => HtmlT m JQuerySelector
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

categoryNode :: Category -> JQuerySelector
categoryNode = JS.selectId . categoryNodeId

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
