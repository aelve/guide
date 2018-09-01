{-# LANGUAGE OverloadedStrings #-}


{- |
Item rendering.

The main functions this module provides are 'renderItem' and
'renderItemForFeed'.
-}
module Guide.Views.Item
(
  -- * Main functions
  renderItem,
  renderItemForFeed,

  -- * Helpers
  renderItemInfo,
  renderItemDescription,
  renderItemEcosystem,
  renderItemTraits,
  renderItemNotes,

  -- * Helpers that should probably be moved somewhere
  renderTrait,
  getItemHue,
)
where


import Imports

-- Containers
import qualified Data.Map as M
import Data.Tree
-- Text
import qualified Data.Text.IO as T
-- HTML
import Lucid hiding (for_)
-- JSON
import qualified Data.Aeson as A
-- Markdown
import qualified CMark as MD

import Guide.Types.Core
import Guide.JS (JS(..))
import qualified Guide.JS as JS
import Guide.Cache
import Guide.Markdown
import Guide.Utils
import Guide.Views.Utils


----------------------------------------------------------------------------
-- Main functions
----------------------------------------------------------------------------

-- | Render an item.
--
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
      hiddenIf (ItemProsConsSection `notElem` category^.enabledSections) $
        div_ [class_ "pros-cons-wrapper"] $
          renderItemTraits item
      hiddenIf (ItemEcosystemSection `notElem` category^.enabledSections) $
        div_ [class_ "ecosystem-wrapper"] $
          renderItemEcosystem item
      hiddenIf (ItemNotesSection `notElem` category^.enabledSections) $
        div_ [class_ "notes-wrapper"] $
          renderItemNotes category item

-- | Render item as it will be shown in a feed.
renderItemForFeed
  :: (MonadIO m)
  => Category -> Item -> HtmlT m ()
renderItemForFeed category item = do
  h1_ $ renderItemTitle item
  unless (markdownNull (item^.description)) $
    toHtml (item^.description)
  when (ItemProsConsSection `elem` category^.enabledSections) $ do
    h2_ "Pros"
    ul_ $ mapM_ (p_ . li_ . toHtml . view content) (item^.pros)
    h2_ "Cons"
    ul_ $ mapM_ (p_ . li_ . toHtml . view content) (item^.cons)
  when (ItemEcosystemSection `elem` category^.enabledSections) $ do
    unless (markdownNull (item^.ecosystem)) $ do
      h2_ "Ecosystem"
      toHtml (item^.ecosystem)
  -- TODO: include .notes-like style here? otherwise the headers are too big
  unless (markdownNull (item^.notes)) $ do
    h2_ "Notes"
    toHtml (item^.notes)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Render item's title.
renderItemTitle :: (MonadIO m) => Item -> HtmlT m ()
renderItemTitle item =
  mustache "item-title" $ A.object [
    "item" A..= item ]

-- TODO: warn when a library isn't on Hackage but is supposed to be

-- | Render item info.
--
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

-- | Render item description.
renderItemDescription :: MonadIO m => Item -> HtmlT m ()
renderItemDescription item = cached (CacheItemDescription (item^.uid)) $
  mustache "item-description" $ A.object [
    "item" A..= item ]

-- | Render the “ecosystem” section.
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
        3 -- rows
        (item^.ecosystem)
        (\val -> JS.withThis JS.submitItemEcosystem
          (this, item^.uid, item^.ecosystem.mdSource, val))
        (JS.withThis JS.switchSection (this, "normal" :: Text))
        "or press Ctrl+Enter to save"

-- | Render the “traits” section.
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
            3 -- rows
            (toMarkdownInline "")
            -- TODO: clearing the editor should be moved into 'addPro' and
            -- done only if the request succeeds
            (\val -> JS.withThis JS.addPro (JS.selectUid listUid, item^.uid, val))
            Nothing
            "press Ctrl+Enter or Enter to add"
            (Just "add pro") -- placeholder
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
            3 -- rows
            (toMarkdownInline "")
            -- TODO: clearing the editor should be moved into 'addCon' and
            -- done only if the request succeeds
            (\val -> JS.withThis JS.addCon (JS.selectUid listUid, item^.uid, val))
            Nothing
            "press Ctrl+Enter or Enter to add"
            (Just "add con") -- placeholder
          textButton "edit off" $
            JS.switchSectionsEverywhere(this, "normal" :: Text)

-- | Render a single trait.
renderTrait :: MonadIO m => Uid Item -> Trait -> HtmlT m ()
renderTrait itemUid trait =
  mustache "trait" $ A.object [
    "item"  A..= A.object [
        "uid" A..= itemUid ],
    "trait" A..= trait ]

-- | Render the “notes” section.
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
        else return (item^.notes.mdSource)
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

-- | Decide what color should an item have. (Requires looking at its parent
-- category.)
getItemHue :: Category -> Item -> Hue
getItemHue category item = case item^.group_ of
  Nothing -> NoHue
  Just s  -> M.findWithDefault NoHue s (category^.groups)
