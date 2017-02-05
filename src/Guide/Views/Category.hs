{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Guide.Views.Category
(
  -- * Main functions
  renderCategory,

  -- * Helpers
  renderCategoryStatus,
  renderCategoryInfo,
  renderCategoryNotes,
)
where


import Imports

-- Text
import qualified Data.Text.All as T
-- HTML
import Lucid hiding (for_)

import Guide.Types.Core
import qualified Guide.JS as JS
import Guide.Cache
import Guide.Markdown
import Guide.Utils
import Guide.Views.Utils
import Guide.Views.Item


----------------------------------------------------------------------------
-- Main functions
----------------------------------------------------------------------------

-- | Render the whole category.
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

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Render the category status banner that is shown on the page of each
-- unfinished category.
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

-- | Render info about the category (the header with category name + the edit
-- form).
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

-- | Render category notes (or “description”).
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
