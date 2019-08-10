{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Category rendering.
--
-- The main function this module provides is 'renderCategory'.
module Guide.Views.Category
(
  -- * Main functions
  renderCategory,

  -- * Helpers
  renderCategoryInfo,
  renderCategoryStatus,
  renderCategoryNotes,
)
where


import Imports

-- HTML
import Lucid hiding (for_)

import Guide.Markdown
import Guide.Types.Core
import Guide.Utils
import Guide.Views.Item
import Guide.Views.Utils

import qualified Data.Text.IO as T

import qualified Guide.JS as JS

----------------------------------------------------------------------------
-- Main functions
----------------------------------------------------------------------------

-- | Render the whole category.
renderCategory :: MonadIO m => Category -> HtmlT m ()
renderCategory category = div_ [class_ "category", id_ (categoryNodeId category)] $ do
    renderCategoryInfo category
    renderCategoryNotes category
    itemsNode <- div_ [class_ "items"] $ do
      mapM_ (renderItem category) (categoryItems category)
      thisNode
    textInput [
      class_ " add-item ",
      placeholder_ "add an item",
      autocomplete_ "off",
      onEnter $ JS.addItem (itemsNode, categoryUid category, inputValue) <>
                clearInput ]

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Render info about the category (the header with category name + the edit
-- form + possibly status banner).
renderCategoryInfo :: MonadIO m => Category -> HtmlT m ()
renderCategoryInfo category =
  let thisId = "category-info-" <> uidToText (categoryUid category)
      this   = JS.selectId thisId
  in div_ [id_ thisId, class_ "category-info"] $ do
    section "normal" [shown, noScriptShown] $ do
      h2_ $ do
        -- TODO: this link shouldn't be absolute [absolute-links]
        span_ [class_ "controls"] $
          a_ [class_ "category-feed",
              href_ ("/haskell/feed/category/" <> uidToText (categoryUid category))] $
            img_ [src_ "/rss-alt.svg",
                  alt_ "category feed", title_ "category feed"]
        a_ [href_ (mkCategoryLink category), class_ "category-title"] $
          toHtml (categoryTitle category)
        emptySpan "1em"
        span_ [class_ "group"] $
          toHtml (categoryGroup category)
        emptySpan "1em"
        textButton "edit" $
          JS.switchSection (this, "editing" :: Text)
        emptySpan "1em"
        textButton "delete" $
          JS.deleteCategoryAndRedirect [categoryUid category]
      renderCategoryStatus category

    section "editing" [] $ do
      let formSubmitHandler formNode =
            JS.submitCategoryInfo (this, categoryUid category, formNode)
      form_ [onFormSubmit formSubmitHandler] $ do
        -- All inputs have "autocomplete = off" thanks to
        -- <http://stackoverflow.com/q/8311455>
        label_ $ do
          "Title" >> br_ []
          input_ [type_ "text", name_ "title",
                  autocomplete_ "off",
                  value_ (categoryTitle category)]
        br_ []
        label_ $ do
          "Group" >> br_ []
          input_ [type_ "text", name_ "group",
                  autocomplete_ "off",
                  value_ (categoryGroup category)]
        br_ []
        label_ $ do
          "Status" >> br_ []
          select_ [name_ "status", autocomplete_ "off"] $ do
            option_ [value_ "finished"] "Complete"
              & selectedIf (categoryStatus category == CategoryFinished)
            option_ [value_ "wip"] "Work in progress"
              & selectedIf (categoryStatus category == CategoryWIP)
            option_ [value_ "stub"] "Stub"
              & selectedIf (categoryStatus category == CategoryStub)
        br_ []
        label_ $ do
          input_ [type_ "checkbox", name_ "pros-cons-enabled",
                  autocomplete_ "off"]
            & checkedIf (ItemProsConsSection `elem` categoryEnabledSections category)
          "Pros/cons enabled"
        br_ []
        label_ $ do
          input_ [type_ "checkbox", name_ "ecosystem-enabled",
                  autocomplete_ "off"]
            & checkedIf (ItemEcosystemSection `elem` categoryEnabledSections category)
          "“Ecosystem” field enabled"
        br_ []
        label_ $ do
          input_ [type_ "checkbox", name_ "notes-enabled",
                  autocomplete_ "off"]
            & checkedIf (ItemNotesSection `elem` categoryEnabledSections category)
          "“Notes” field enabled"
        br_ []
        input_ [type_ "submit", value_ "Save", class_ "save"]
        button "Cancel" [class_ "cancel"] $
          JS.switchSection (this, "normal" :: Text)

-- | Render the category status banner that is shown on the page of each
-- unfinished category.
renderCategoryStatus :: MonadIO m => Category -> HtmlT m ()
renderCategoryStatus category = do
  case categoryStatus category of
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

-- | Render category notes (or “description”).
renderCategoryNotes :: MonadIO m => Category -> HtmlT m ()
renderCategoryNotes category =
  let thisId = "category-notes-" <> uidToText (categoryUid category)
      this   = JS.selectId thisId
  in div_ [id_ thisId, class_ "category-notes"] $ do
    section "normal" [shown, noScriptShown] $ do
      div_ [class_ "notes-like"] $ do
        if markdownBlockSource (categoryNotes category) == ""
          then p_ "write something here!"
          else toHtml (categoryNotes category)
      textButton "edit description" $
        JS.switchSection (this, "editing" :: Text) <>
        JS.focusOn [(this `JS.selectSection` "editing")
                    `JS.selectChildren`
                    JS.selectClass "editor"]

    section "editing" [] $ do
      contents <- if markdownBlockSource (categoryNotes category) == ""
        then liftIO $ toMarkdownBlock <$>
               T.readFile "static/category-notes-template.md"
        else return (categoryNotes category)
      markdownEditor
        10 -- rows
        contents
        (\val -> JS.withThis JS.submitCategoryNotes
          (this,
           categoryUid category,
           markdownBlockSource (categoryNotes category),
           val))
        (JS.withThis JS.switchSection (this, "normal" :: Text))
        "or press Ctrl+Enter to save"
