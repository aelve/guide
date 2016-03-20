{-# LANGUAGE
QuasiQuotes,
OverloadedStrings,
FlexibleContexts,
ViewPatterns,
NoImplicitPrelude
  #-}


module View
(
  -- * Pages
  renderRoot,
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
-- Containers
import qualified Data.Map as M
-- Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import NeatInterpolation
-- Web
import Lucid hiding (for_)

-- Local
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

renderRoot :: GlobalState -> Maybe Text -> HtmlT IO ()
renderRoot globalState mbSearchQuery =
  wrapPage "Aelve Guide" $ do
    -- TODO: [very-easy] this header looks bad when the page is narrow, it
    -- should be fixed in css.css by adding line-height to it
    h1_ "The Haskeller's guide"
    noscript_ $ div_ [id_ "noscript-message"] $
      toHtml $ renderMarkdownBlock [text|
        You have Javascript disabled! This site works fine without
        Javascript, but since all editing needs Javascript to work,
        you won't be able to edit anything.
        |]
    renderHelp
    onPageLoad $ JS.showOrHideHelp (JS.selectId "help", helpVersion)
    form_ $ do
      input_ [type_ "text", name_ "q", id_ "search", placeholder_ "search",
              value_ (fromMaybe "" mbSearchQuery)]
    textInput [
      placeholder_ "add a category",
      autocomplete_ "off",
      onEnter $ JS.addCategory (JS.selectId "categories", inputValue) <>
                clearInput ]
    -- TODO: sort categories by popularity, somehow? or provide a list of
    -- “commonly used categories” or even a nested catalog
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

-- TODO: when submitting a text field, gray it out (but leave it selectable)
-- until it's been submitted

renderTracking :: HtmlT IO ()
renderTracking = do
  trackingEnabled <- (== Just "1") <$> liftIO (lookupEnv "GUIDE_TRACKING")
  when trackingEnabled $ do
    tracking <- liftIO $ T.readFile "static/tracking.html"
    toHtmlRaw tracking

-- TODO: include jQuery locally so that it'd be possible to test the site
-- without internet

renderDonate :: HtmlT IO ()
renderDonate = doctypehtml_ $ do
  head_ $ do
    title_ "Donate to Artyom"
    includeCSS "/css.css"
    renderTracking
  body_ $
    toHtmlRaw =<< liftIO (readFile "static/donate.html")

renderUnwrittenRules :: HtmlT IO ()
renderUnwrittenRules = wrapPage "Unwritten rules" $ do
  toHtml . renderMarkdownBlock =<<
    liftIO (T.readFile "static/unwritten-rules.md")

-- Include all the necessary things
wrapPage :: Text -> HtmlT IO () -> HtmlT IO ()
wrapPage pageTitle page = doctypehtml_ $ do
  head_ $ do
    title_ (toHtml pageTitle)
    meta_ [name_ "viewport",
           content_ "width=device-width, initial-scale=1.0, user-scalable=yes"]
    let cdnjs = "https://cdnjs.cloudflare.com/ajax/libs/"
    includeJS (cdnjs <> "jquery/2.2.0/jquery.min.js")
    -- See Note [autosize]
    includeJS (cdnjs <> "autosize.js/3.0.15/autosize.min.js")
    onPageLoad (JS "autosize($('textarea'));")
    -- It's important that css.css comes second – it overwrites some rules
    -- from highlight.css (see the rule for div.sourceCode)
    includeCSS "/highlight.css"
    includeCSS "/css.css"
    -- Include definitions of all Javascript functions that we have defined
    -- in this file. (This isn't an actual file, so don't look for it in the
    -- static folder – it's generated and served in 'otherMethods'.)
    includeJS "/js.js"
    renderTracking
    -- CSS that makes 'shown' and 'noScriptShown' work
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

renderCategoryPage :: Category -> HtmlT IO ()
renderCategoryPage category =
  wrapPage (category^.title <> " – Aelve Guide") $ do
    -- TODO: [very-easy] this header looks bad when the page is narrow, it
    -- should be fixed in css.css by adding line-height to it
    -- TODO: another absolute link [absolute-links]
    h1_ (a_ [href_ "/haskell"] "The Haskeller's guide")
    noscript_ $ div_ [id_ "noscript-message"] $
      toHtml $ renderMarkdownBlock [text|
        You have Javascript disabled! This site works fine without
        Javascript, but since all editing needs Javascript to work,
        you won't be able to edit anything.
        |]
    renderCategory category

-- TODO: allow archiving items if they are in every way worse than the rest,
-- or something (but searching should still be possible)

-- TODO: add a list for “interesting libraries, but too lazy to describe, so
-- somebody describe them for me”

renderHelp :: HtmlT IO ()
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

helpVersion :: Int
helpVersion = 3

-- TODO: when conflicts happen, maybe create an alert like “The thing you're
-- editing has been edited in the meantime. Here is a link with a diff of
-- your variant and the other person's variant. Please merge the changes
-- manually and submit them again, or press this button and we'll merge the
-- changes for you (don't worry, it's not a big deal for us). Thanks!”

-- TODO: automatic merge should be possible too (e.g. if the changes are in
-- different paragraphs)

renderCategoryList :: [Category] -> HtmlT IO ()
renderCategoryList cats =
  div_ [id_ "categories"] $
    mapM_ renderCategory cats

renderCategoryTitle :: Category -> HtmlT IO ()
renderCategoryTitle category = do
  let thisId = "category-title-" <> uidToText (category^.uid)
      this   = JS.selectId thisId
  h2_ [id_ thisId] $ do
    sectionSpan "normal" [shown, noScriptShown] $ do
      -- TODO: this link shouldn't be absolute [absolute-links]
      a_ [href_ ("/haskell/" <> categorySlug category)] $
        toHtml (category^.title)
      emptySpan "1em"
      textButton "edit" $
        JS.switchSection (this, "editing" :: Text)

    sectionSpan "editing" [] $ do
      textInput [
        value_ (category^.title),
        autocomplete_ "off",
        onEnter $
          JS.submitCategoryTitle (this, category^.uid, inputValue)]
      emptySpan "1em"
      textButton "cancel" $
        JS.switchSection (this, "normal" :: Text)

renderCategoryNotes :: Category -> HtmlT IO ()
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

    section "editing" [] $
      markdownEditor
        [rows_ "10"]
        (category^.notes)
        (\val -> JS.submitCategoryNotes (this, category^.uid, val))
        (JS.switchSection (this, "normal" :: Text))

renderCategory :: Category -> HtmlT IO ()
renderCategory category =
  div_ [class_ "category", uid_ (category^.uid)] $ do
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
renderItem :: Category -> Item -> HtmlT IO ()
renderItem category item =
  -- The id is used for links in feeds, and for anchor links
  div_ [id_ ("item-" <> uidToText (item^.uid)), class_ "item"] $ do
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

-- TODO: some spinning thingy that spins in the corner of the page while a
-- request is happening

-- TODO: find some way to give all functions access to category and item (or
-- category, item and trait) without passing everything explicitly?

-- TODO: warn when a library isn't on Hackage but is supposed to be

renderItemHeader :: Monad m => Item -> HtmlT m ()
renderItemHeader item = do
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
renderItemInfo :: Category -> Item -> HtmlT IO ()
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
        let link' = format "/haskell/{}#item-{}" (categorySlug cat, item^.uid)
        a_ [class_ "anchor", href_ link'] "#"
        renderItemHeader item
      emptySpan "2em"
      toHtml (fromMaybe "other" (item^.group_))
      span_ [class_ "controls"] $ do
        let itemNode = JS.selectId ("item-" <> uidToText (item^.uid))
        imgButton "move item up" "/arrow-thick-top.svg" [] $
          -- TODO: [easy] the item should blink or somehow else show where it
          -- has been moved
          JS.moveItemUp (item^.uid, itemNode)
        imgButton "move item down" "/arrow-thick-bottom.svg" [] $
          JS.moveItemDown (item^.uid, itemNode)
        emptySpan "1.5em"
        imgButton "edit item info" "/pencil.svg" [] $
          JS.switchSection (this, "editing" :: Text)
        emptySpan "0.5em"
        imgButton "delete item" "/x.svg" [] $
          JS.deleteItem (item^.uid, itemNode)
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

-- TODO: use triangle icons instead of arrows [very-easy]

-- TODO: categories that don't directly compare libraries but just list all
-- libraries about something (e.g. Yesod plugins, or whatever)

-- TODO: categories without items (e.g. “web dev”) that list links to other
-- categories

renderItemDescription :: Item -> HtmlT IO ()
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

renderItemEcosystem :: Item -> HtmlT IO ()
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

renderItemTraits :: Item -> HtmlT IO ()
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
        JS.switchSectionsEverywhere(this, "editable" :: Text)
    section "editable" [] $ do
      textButton "edit off" $
        JS.switchSectionsEverywhere(this, "normal" :: Text)

renderTrait :: Uid -> Trait -> HtmlT IO ()
-- TODO: probably use renderMarkdownBlock here as well
renderTrait itemId trait = do
  let thisId = "trait-" <> uidToText (trait^.uid)
      this   = JS.selectId thisId
  li_ [id_ thisId] $ do

    sectionSpan "normal" [shown, noScriptShown] $ do
      toHtml (trait^.content)

    section "editable" [] $ do
      toHtml (trait^.content)
      br_ []
      imgButton "move trait up" "/arrow-thick-top.svg" [width_ "12"] $
        JS.moveTraitUp (itemId, trait^.uid, this)
      imgButton "move trait down" "/arrow-thick-bottom.svg" [width_ "12"] $
        JS.moveTraitDown (itemId, trait^.uid, this)
      -- TODO: these 3 icons in a row don't look nice
      -- TODO: there should be some way to undelete things (e.g. a list of
      -- deleted traits under each item)
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
-- TODO: [easy] add a button to make default editor font monospace
-- TODO: [easy] write that arrows are for arranging stuff, not upvoting

-- TODO: record IPs in the acid-state transaction log

-- TODO: [easy] add Hayoo search, Hoogle search, and Hackage search shortcut
-- boxes

-- TODO: when searching, show links to package, main modules, etc before all
-- categories

-- TODO: attach TODOs (“fix grammar”, etc) to items and categories (or should
-- people instead just write “TODO fix grammar” in description and then such
-- things could be displayed in gray font and also there'd be an
-- automatically updated list of TODOs somewhere?)

-- TODO: make it possible to link to notes (and automatically expand when
-- linked)

-- TODO: [very-easy] focus the notes textarea on edit (can use jQuery's
-- .focus() on it)
renderItemNotes :: Item -> HtmlT IO ()
renderItemNotes item = do
  let thisId = "item-notes-" <> uidToText (item^.uid)
      this   = JS.selectId thisId
  div_ [id_ thisId, class_ "item-notes"] $ do
    -- TODO: this duplicates code from renderCategoryNotes, try to reduce
    -- duplication

    section "collapsed" [shown] $ do
      -- TODO: when notes are hidden, show a list of headers in the notes
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
        else toHtml (item^.notes)
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

renderItemForFeed :: Item -> Html ()
renderItemForFeed item = do
  h1_ $ renderItemHeader item
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

onPageLoad :: JS -> HtmlT IO ()
onPageLoad js = script_ $ format "$(document).ready(function(){{}});" [js]

emptySpan :: Text -> HtmlT IO ()
emptySpan w = span_ [style_ ("margin-left:" <> w)] mempty

-- Use inputValue to get the value (works with input_ and textarea_)
onEnter :: JS -> Attribute
onEnter handler = onkeydown_ $
  format "if (event.keyCode == 13) {{} return false;}" [handler]

textInput :: [Attribute] -> HtmlT IO ()
textInput attrs = input_ (type_ "text" : attrs)

inputValue :: JS
inputValue = JS "this.value"

clearInput :: JS
clearInput = JS "this.value = '';"

onFormSubmit :: (JS -> JS) -> Attribute
onFormSubmit f = onsubmit_ $ format "{} return false;" [f (JS "this")]

button :: Text -> [Attribute] -> JS -> HtmlT IO ()
button value attrs handler =
  input_ (type_ "button" : value_ value : onclick_ handler' : attrs)
  where
    handler' = fromJS handler

-- A text button looks like “[cancel]”
-- 
-- TODO: consider dotted links instead?
textButton
  :: Text         -- ^ Button text
  -> JS           -- ^ Onclick handler
  -> HtmlT IO ()
textButton caption (JS handler) =
  span_ [class_ "text-button"] $
    -- “#” is used instead of javascript:void(0) because the latter is slow
    -- in Firefox (at least for me – tested with Firefox 43 on Arch Linux)
    a_ [href_ "#", onclick_ (handler <> "return false;")]
       (toHtml caption)

-- So far all icons used here have been from <https://useiconic.com/open/>
imgButton :: Text -> Url -> [Attribute] -> JS -> HtmlT IO ()
imgButton alt src attrs (JS handler) =
  a_ [href_ "#", onclick_ (handler <> "return false;")]
     (img_ (src_ src : alt_ alt : attrs))

markdownEditor
  :: [Attribute]
  -> MarkdownBlock  -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving the contents of the editor
  -> JS             -- ^ “Cancel” handler
  -> HtmlT IO ()
markdownEditor attr (markdownBlockText -> s) submit cancel = do
  textareaId <- randomLongUid
  -- Autocomplete has to be turned off thanks to
  -- <http://stackoverflow.com/q/8311455>.
  textarea_ ([uid_ textareaId, autocomplete_ "off", class_ "big fullwidth"]
             ++ attr) $
    toHtml s
  let val = JS $ format "document.getElementById(\"{}\").value" [textareaId]
  button "Save" [] $
    submit val
  emptySpan "6px"
  button "Cancel" [] $
    JS.assign val s <>
    cancel
  emptySpan "6px"
  "Markdown"

smallMarkdownEditor
  :: [Attribute]
  -> MarkdownInline -- ^ Default text
  -> (JS -> JS)     -- ^ “Submit” handler, receiving the contents of the editor
  -> Maybe JS       -- ^ “Cancel” handler (if “Cancel” is needed)
  -> HtmlT IO ()
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

thisNode :: HtmlT IO JQuerySelector
thisNode = do
  uid' <- randomLongUid
  -- If the class name ever changes, fix 'JS.moveNodeUp' and
  -- 'JS.moveNodeDown'.
  span_ [uid_ uid', class_ "dummy"] mempty
  return (JS.selectParent (JS.selectUid uid'))

-- Wheh changing these, also look at 'JS.switchSection'.

shown, noScriptShown :: Attribute
shown          = class_ " shown "
noScriptShown  = class_ " noscript-shown "

section :: Monad m => Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
section t attrs = div_ (class_ (t <> " section ") : attrs)

sectionSpan :: Monad m => Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
sectionSpan t attrs = span_ (class_ (t <> " section ") : attrs)

-- TODO: add something to edit a particular paragraph of the notes

newGroupValue :: Text
newGroupValue = "-new-group-"
