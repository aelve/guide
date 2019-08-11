{-# LANGUAGE OverloadedStrings #-}


-- | Item rendering.
--
-- The main functions this module provides are 'renderItem' and
-- 'renderItemForFeed'.
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
)
where


import Imports

-- Containers
import Data.Tree
-- HTML
import Lucid hiding (for_)

import Guide.JS (JS (..))
import Guide.Markdown
import Guide.Types.Core
import Guide.Utils
import Guide.Views.Utils

import qualified Data.Aeson as A
import qualified Data.Text.IO as T

import qualified Guide.JS as JS

----------------------------------------------------------------------------
-- Main functions
----------------------------------------------------------------------------

-- | Render an item.
--
-- TODO: perhaps use jQuery Touch Punch or something to allow dragging items
-- instead of using arrows? Touch Punch works on mobile, too
renderItem :: MonadIO m => Category -> Item -> HtmlT m ()
renderItem category item =
  -- The id is used for links in feeds, and for anchor links
  div_ [id_ (itemNodeId item), class_ "item"] $ do
    renderItemInfo category item
    div_ [class_ "item-body", style_ ("background-color:#F0F0F0")] $ do
      -- See Note [enabled sections]
      renderItemDescription item
      hiddenIf (ItemProsConsSection `notElem` categoryEnabledSections category) $
        div_ [class_ "pros-cons-wrapper"] $
          renderItemTraits item
      hiddenIf (ItemEcosystemSection `notElem` categoryEnabledSections category) $
        div_ [class_ "ecosystem-wrapper"] $
          renderItemEcosystem item
      hiddenIf (ItemNotesSection `notElem` categoryEnabledSections category) $
        div_ [class_ "notes-wrapper"] $
          renderItemNotes category item

-- | Render item as it will be shown in a feed.
renderItemForFeed
  :: (MonadIO m)
  => Category -> Item -> HtmlT m ()
renderItemForFeed category item = do
  h1_ $ renderItemTitle item
  unless (markdownBlockSource (itemSummary item) == "") $
    toHtml (itemSummary item)
  when (ItemProsConsSection `elem` categoryEnabledSections category) $ do
    h2_ "Pros"
    ul_ $ mapM_ (p_ . li_ . toHtml . traitContent) (itemPros item)
    h2_ "Cons"
    ul_ $ mapM_ (p_ . li_ . toHtml . traitContent) (itemCons item)
  when (ItemEcosystemSection `elem` categoryEnabledSections category) $ do
    unless (markdownBlockSource (itemEcosystem item) == "") $ do
      h2_ "Ecosystem"
      toHtml (itemEcosystem item)
  -- TODO: include .notes-like style here? otherwise the headers are too big
  unless (markdownTreeSource (itemNotes item) == "") $ do
    h2_ "Notes"
    toHtml (itemNotes item)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Render item's title.
renderItemTitle :: (MonadIO m) => Item -> HtmlT m ()
renderItemTitle item =
  mustache "item-title" $ A.object [
    "item" A..= item ]

-- TODO: warn when a library isn't on Hackage but is supposed to be
-- TODO: give a link to oldest available docs when the new docs aren't there

-- | Render item info.
renderItemInfo :: (MonadIO m) => Category -> Item -> HtmlT m ()
renderItemInfo cat item =
  mustache "item-info" $ A.object [
    "category" A..= cat,
    "item" A..= item,
    "link_to_item" A..= mkItemLink cat item,
    "hackage" A..= itemHackage item ]

-- | Render item description.
renderItemDescription :: MonadIO m => Item -> HtmlT m ()
renderItemDescription item = mustache "item-description" $
                                A.object ["item" A..= item ]

-- | Render the “ecosystem” section.
renderItemEcosystem :: MonadIO m => Item -> HtmlT m ()
renderItemEcosystem item =
  let thisId = "item-ecosystem-" <> uidToText (itemUid item)
      this   = JS.selectId thisId
  in div_ [id_ thisId, class_ "item-ecosystem"] $ do

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
        unless (markdownBlockSource (itemEcosystem item) == "") $
          toHtml (itemEcosystem item)

    section "editing" [] $ do
      strong_ "Ecosystem"
      emptySpan "0.5em"
      imgButton "quit editing ecosystem" "/pencil.svg"
        [style_ "width:12px;opacity:0.5", class_ " edit-item-ecosystem "] $
        JS.switchSection (this, "normal" :: Text)
      markdownEditor
        3 -- rows
        (itemEcosystem item)
        (\val -> JS.withThis JS.submitItemEcosystem
          (this, itemUid item, markdownBlockSource (itemEcosystem item), val))
        (JS.withThis JS.switchSection (this, "normal" :: Text))
        "or press Ctrl+Enter to save"

-- | Render the “traits” section.
renderItemTraits :: MonadIO m => Item -> HtmlT m ()
renderItemTraits item = div_ [class_ "item-traits"] $ do
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
          mapM_ (renderTrait (itemUid item)) (itemPros item)
        section "editable" [] $ do
          smallMarkdownEditor
            3 -- rows
            (toMarkdownInline "")
            -- TODO: clearing the editor should be moved into 'addPro' and
            -- done only if the request succeeds
            (\val -> JS.withThis JS.addPro (JS.selectUid listUid, itemUid item, val))
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
          mapM_ (renderTrait (itemUid item)) (itemCons item)
        section "editable" [] $ do
          smallMarkdownEditor
            3 -- rows
            (toMarkdownInline "")
            -- TODO: clearing the editor should be moved into 'addCon' and
            -- done only if the request succeeds
            (\val -> JS.withThis JS.addCon (JS.selectUid listUid, itemUid item, val))
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
renderItemNotes category item = do
  -- Don't change this ID, it's used in e.g. 'JS.expandHash'
  let thisId = "item-notes-" <> uidToText (itemUid item)
      this   = JS.selectId thisId
  editingSectionUid <- randomLongUid
  div_ [id_ thisId, class_ "item-notes"] $ do
    let notesLink = mkCategoryLink category <> "#" <> thisId
    a_ [href_ notesLink] $
      strong_ "Notes"

    let renderTree :: Monad m => Forest Heading -> HtmlT m ()
        renderTree [] = return ()
        renderTree xs = ul_ $ do
          for_ xs $ \(Node {-(is, id')-} (Heading hMd id') children) -> li_ $ do
            let handler = fromJS (JS.expandItemNotes [itemUid item])
                -- The link has to be absolute because sometimes we are
                -- looking at items from pages different from the proper
                -- category pages (e.g. if a search from the main page
                -- returned several items from different categories, and the
                -- user is looking at those items' notes without leaving the
                -- search page). Well, actually it doesn't happen yet because
                -- there's no search (or rather, there is search but it
                -- doesn't return items, only categories); however, it might
                -- start happening and then it's better to be prepared.
                fullLink = mkCategoryLink category <> "#" <> id'
            a_ [href_ fullLink, onclick_ handler] $
              toHtmlRaw (markdownInlineHtml hMd)
            renderTree children
    let renderTOC = do
          let toc = markdownTreeTOC (itemNotes item)
          div_ [class_ "notes-toc"] $ do
            if null toc
              then p_ (emptySpan "1.5em" >> "<notes are empty>")
              else renderTree toc

    section "collapsed" [shown] $ do
      textButton "expand notes" $
        JS.expandItemNotes [itemUid item]
      renderTOC

    section "expanded" [noScriptShown] $ do
      textareaUid <- randomLongUid
      contents <- if markdownTreeSource (itemNotes item) == ""
        then liftIO $ T.readFile "static/item-notes-template.md"
        else return (markdownTreeSource (itemNotes item))
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
                   markdownTreeSource (itemNotes item) == "",
                   contents,
                   itemUid item) <>
              JS.switchSection (this, "editing" :: Text) <>
              JS.autosizeTextarea [JS.selectUid textareaUid] <>
              JS.focusOn [JS.selectUid textareaUid]
      buttons
      renderTOC
      div_ [class_ "notes-like"] $ do
        if markdownTreeSource (itemNotes item) == ""
          then p_ "add something!"
          else toHtml (itemNotes item)
      unless (markdownTreeSource (itemNotes item) == "") $
        buttons
      -- TODO: [easy] the lower “hide notes” should scroll back to item when
      -- the notes are closed (but don't scroll if it's already visible after
      -- the notes have been hidden)

    section "editing" [uid_ editingSectionUid] $
      return ()
