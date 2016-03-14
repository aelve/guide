{-# LANGUAGE
OverloadedStrings,
QuasiQuotes,
ScopedTypeVariables,
GeneralizedNewtypeDeriving,
TypeFamilies,
DataKinds,
MultiWayIf,
NoImplicitPrelude
  #-}


module Main (main) where


-- General
import BasePrelude hiding (Category)
-- Monads and monad transformers
import Control.Monad.State
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Containers
import qualified Data.Map as M
-- Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import NeatInterpolation
import qualified Data.Text.Buildable as Format
-- Randomness
import System.Random
-- Web
import Lucid hiding (for_)
import Lucid.Base (makeAttribute)
import Web.Spock hiding (head, get, text)
import qualified Web.Spock as Spock
import Network.Wai.Middleware.Static
-- Monitoring
import qualified System.Remote.Monitoring as EKG
import qualified Network.Wai.Metrics as EKG
import qualified System.Metrics.Gauge as EKG.Gauge
import Data.Generics.Uniplate.Data
-- acid-state
import Data.Acid as Acid

-- Local
import Types
import JS (JS(..), ToJS(..), allJSFunctions)
import qualified JS
import Utils
import Markdown
import SampleState


randomUid :: MonadIO m => m Uid
randomUid = liftIO $ Uid . tshow <$> randomRIO (0::Int, 10^(9::Int))

------------------------------------------------------------------------------
-- working with global state via acid-state
------------------------------------------------------------------------------

type DB = AcidState GlobalState

dbUpdate :: (MonadIO m, HasSpock m, SpockState m ~ DB,
             EventState event ~ GlobalState, UpdateEvent event)
         => event -> m (EventResult event)
dbUpdate x = do
  db <- Spock.getState
  liftIO $ Acid.update db x

dbQuery :: (MonadIO m, HasSpock m, SpockState m ~ DB,
            EventState event ~ GlobalState, QueryEvent event)
        => event -> m (EventResult event)
dbQuery x = do
  db <- Spock.getState
  liftIO $ Acid.query db x

itemVar :: Path '[Uid]
itemVar = "item" <//> var

categoryVar :: Path '[Uid]
categoryVar = "category" <//> var

traitVar :: Path '[Uid]
traitVar = "trait" <//> var

renderMethods :: SpockM () () DB ()
renderMethods = Spock.subcomponent "render" $ do
  -- Title of a category
  Spock.get (categoryVar <//> "title") $ \catId -> do
    category <- dbQuery (GetCategory catId)
    lucid $ renderCategoryTitle category
  -- Notes for a category
  Spock.get (categoryVar <//> "notes") $ \catId -> do
    category <- dbQuery (GetCategory catId)
    lucid $ renderCategoryNotes category
  -- Item colors
  Spock.get (itemVar <//> "colors") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    let hue = getItemHue category item
    json $ M.fromList [("light" :: Text, hueToLightColor hue),
                       ("dark" :: Text, hueToDarkColor hue)]
  -- Item info
  Spock.get (itemVar <//> "info") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucid $ renderItemInfo category item
  -- Item description
  Spock.get (itemVar <//> "description") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucid $ renderItemDescription category item
  -- Item notes
  Spock.get (itemVar <//> "notes") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucid $ renderItemNotes category item

-- TODO: [easy] use window.onerror to catch and show all JS errors (showing
-- could be done by displaying an alert)

setMethods :: SpockM () () DB ()
setMethods = Spock.subcomponent "set" $ do
  -- Title of a category
  Spock.post (categoryVar <//> "title") $ \catId -> do
    content' <- param' "content"
    category <- dbUpdate (SetCategoryTitle catId content')
    lucid $ renderCategoryTitle category
  -- Notes for a category
  Spock.post (categoryVar <//> "notes") $ \catId -> do
    content' <- param' "content"
    category <- dbUpdate (SetCategoryNotes catId content')
    lucid $ renderCategoryNotes category
  -- Item info
  Spock.post (itemVar <//> "info") $ \itemId -> do
    -- TODO: [easy] add a cross-link saying where the form is handled in the
    -- code and other notes saying where stuff is rendered, etc
    name' <- T.strip <$> param' "name"
    link' <- T.strip <$> param' "link"
    kind' <- do
      kindName :: Text <- param' "kind"
      hackageName' <- (\x -> if T.null x then Nothing else Just x) <$>
                      param' "hackage-name"
      return $ case kindName of
        "library" -> Library hackageName'
        "tool"    -> Tool hackageName'
        _         -> Other
    group' <- do
      groupField <- param' "group"
      customGroupField <- param' "custom-group"
      if | groupField == "-"           -> return Nothing
         | groupField == newGroupValue -> return (Just customGroupField)
         | otherwise                   -> return (Just groupField)
    -- Modify the item
    -- TODO: actually validate the form and report errors
    unless (T.null name') $ void $
      dbUpdate (SetItemName itemId name')
    case (T.null link', sanitiseUrl link') of
      (True, _)   -> void $ dbUpdate (SetItemLink itemId Nothing)
      (_, Just l) -> void $ dbUpdate (SetItemLink itemId (Just l))
      _otherwise  -> return ()
    dbUpdate (SetItemKind itemId kind')
    -- This does all the work of assigning new colors, etc. automatically
    dbUpdate (SetItemGroup itemId group')
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucid $ renderItemInfo category item
  -- Item description
  Spock.post (itemVar <//> "description") $ \itemId -> do
    content' <- param' "content"
    item <- dbUpdate (SetItemDescription itemId content')
    category <- dbQuery (GetCategoryByItem itemId)
    lucid $ renderItemDescription category item
  -- Item notes
  Spock.post (itemVar <//> "notes") $ \itemId -> do
    content' <- param' "content"
    item <- dbUpdate (SetItemNotes itemId content')
    category <- dbQuery (GetCategoryByItem itemId)
    lucid $ renderItemNotes category item
  -- Trait
  Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
    content' <- param' "content"
    trait <- dbUpdate (SetTraitContent itemId traitId content')
    lucid $ renderTrait itemId trait

-- TODO: [easy] add stuff like “add/category” here in comments to make it
-- easier to search with C-s (or maybe just don't use subcomponent?)
addMethods :: SpockM () () DB ()
addMethods = Spock.subcomponent "add" $ do
  -- New category
  Spock.post "category" $ do
    title' <- param' "content"
    catId <- randomUid
    newCategory <- dbUpdate (AddCategory catId title')
    lucid $ renderCategory newCategory
  -- New item in a category
  Spock.post (categoryVar <//> "item") $ \catId -> do
    name' <- param' "name"
    -- TODO: do something if the category doesn't exist (e.g. has been
    -- already deleted)
    itemId <- randomUid
    -- If the item name looks like a Hackage library, assume it's a Hackage
    -- library.
    newItem <- if T.all (\c -> isAscii c && (isAlphaNum c || c == '-')) name'
      then dbUpdate (AddItem catId itemId name' (Library (Just name')))
      else dbUpdate (AddItem catId itemId name' Other)
    category <- dbQuery (GetCategory catId)
    lucid $ renderItem category newItem
  -- Pro (argument in favor of an item)
  Spock.post (itemVar <//> "pro") $ \itemId -> do
    content' <- param' "content"
    traitId <- randomUid
    newTrait <- dbUpdate (AddPro itemId traitId content')
    lucid $ renderTrait itemId newTrait
  -- Con (argument against an item)
  Spock.post (itemVar <//> "con") $ \itemId -> do
    content' <- param' "content"
    traitId <- randomUid
    newTrait <- dbUpdate (AddCon itemId traitId content')
    lucid $ renderTrait itemId newTrait

otherMethods :: SpockM () () DB ()
otherMethods = do
  -- Javascript
  Spock.get "js.js" $ do
    setHeader "Content-Type" "application/javascript; charset=utf-8"
    Spock.bytes $ T.encodeUtf8 (fromJS allJSFunctions)

  -- Moving things
  Spock.subcomponent "move" $ do
    -- Move item
    Spock.post itemVar $ \itemId -> do
      direction :: Text <- param' "direction"
      dbUpdate (MoveItem itemId (direction == "up"))
    -- Move trait
    Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
      direction :: Text <- param' "direction"
      dbUpdate (MoveTrait itemId traitId (direction == "up"))

  -- Deleting things
  Spock.subcomponent "delete" $ do
    -- Delete item
    Spock.post itemVar $ \itemId -> do
      dbUpdate (DeleteItem itemId)
    -- Delete trait
    Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
      dbUpdate (DeleteTrait itemId traitId)

main :: IO ()
main = do
  bracket (openLocalStateFrom "state/" sampleState)
          (\db -> createCheckpoint db >> closeAcidState db) $ \db -> do
    -- Create a checkpoint every hour. Note: if nothing was changed,
    -- acid-state overwrites the previous checkpoint, which saves us some
    -- space.
    forkOS $ forever $ do
      createCheckpoint db
      threadDelay (1000000 * 3600)
    -- EKG metrics
    ekg <- EKG.forkServer "localhost" 5050
    waiMetrics <- EKG.registerWaiMetrics (EKG.serverMetricStore ekg)
    categoryGauge <- EKG.getGauge "db.categories" ekg
    itemGauge <- EKG.getGauge "db.items" ekg
    textGauge <- EKG.getGauge "db.text_length" ekg
    forkOS $ forever $ do
      globalState <- Acid.query db GetGlobalState
      let allCategories = globalState^.categories
      let allItems = allCategories^.each.items
      let textLength = sum (map T.length (childrenBi globalState))
      EKG.Gauge.set categoryGauge (fromIntegral (length allCategories))
      EKG.Gauge.set itemGauge (fromIntegral (length allItems))
      EKG.Gauge.set textGauge (fromIntegral textLength)
      threadDelay 1000000
    -- Run the server
    let config = defaultSpockCfg () PCNoDatabase db
    runSpock 8080 $ spock config $ do
      middleware (EKG.metrics waiMetrics)
      middleware (staticPolicy (addBase "static"))
      -- Main page
      Spock.get root $ do
        s <- dbQuery GetGlobalState
        q <- param "q"
        lucid $ renderRoot s q
      -- Donation page
      Spock.get "donate" $ do
        lucid $ renderDonate
      -- The add/set methods return rendered parts of the structure (added
      -- categories, changed items, etc) so that the Javascript part could take
      -- them and inject into the page. We don't want to duplicate rendering on
      -- server side and on client side.
      renderMethods
      setMethods
      addMethods
      otherMethods

-- TODO: RSS feeds for categories

-- TODO: when a category with the same name exists, show an error message and
-- redirect to that other category

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
renderRoot globalState mbSearchQuery = doctypehtml_ $ do
  head_ $ do
    title_ "Aelve Guide"
    let cdnjs = "https://cdnjs.cloudflare.com/ajax/libs/"
    includeJS (cdnjs <> "jquery/2.2.0/jquery.min.js")
    -- See Note [autosize]
    includeJS (cdnjs <> "autosize.js/3.0.15/autosize.min.js")
    onPageLoad (JS "autosize($('textarea'));")
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
    -- TODO: [very-easy] this header looks bad when the page is narrow, it
    -- should be fixed in css.css by adding line-height to it
    h1_ "A guide to Haskell libraries and tools"
    noscript_ $ div_ [id_ "noscript-message"] $
      renderMarkdownBlock [text|
        You have Javascript disabled! This site works fine without
        Javascript, but since all editing needs Javascript to work,
        you won't be able to edit anything.
        |]
    renderHelp
    onPageLoad $ JS.showOrHideHelp (selectId "help", helpVersion)
    form_ $ do
      input_ [type_ "text", name_ "q", id_ "search", placeholder_ "search",
              value_ (fromMaybe "" mbSearchQuery)]
    textInput [
      placeholder_ "add a category",
      autocomplete_ "off",
      onEnter $ JS.addCategory (selectId "categories", inputValue) <>
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
    -- TODO: perhaps use infinite scrolling/loading?
    -- TODO: maybe add a button like “give me random category that is
    -- unfinished”
    div_ [id_ "footer"] $ do
      "made by " >> a_ [href_ "https://artyom.me"] "Artyom"
      emptySpan "2em"
      a_ [href_ "https://github.com/aelve/guide"] "source"
      emptySpan "2em"
      a_ [href_ "https://github.com/aelve/guide/issues"] "report an issue"
      emptySpan "2em"
      a_ [href_ "/donate"] "donate"
      sup_ [style_ "font-size:50%"] "I don't have a job"

-- TODO: mention that (@hackage) is supported somewhere

-- TODO: code highlighting

-- TODO: when submitting a text field, gray it out (but leave it selectable)
-- until it's been submitted

-- TODO: disable tracking on localhost! (and edit INSTALL.md)
--
-- TODO: separate the tracking image and the tracking script – the former
-- should be in <body>, the latter in <head> [easy]
renderTracking :: HtmlT IO ()
renderTracking = do
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

  -- TODO: move this into its own file in static/?
  body_ $ do
    renderMarkdownBlock [text|
      Okay, the rules: if you donate *anything*, I'll spend some time working
      on the site this day (adding content, implementing new features, etc).

      (Of course, I'm planning to be working on the site anyway, donations
      or not! However, I jump from project to project way too often (and
      rarely manage to finish anything), so donating money is a good way to
      make sure that I'd feel obligated to keep working on this one. If I
      find out that it doesn't work as a motivation, I'll stop accepting
      donations.)

      Just in case, 1000 rub. is 14$ (or 12.5€), and you can choose any
      amount below 15000 rub. (I'd put a Paypal button, but Paypal doesn't
      allow receiving money in Belarus.)
      |]
    style_ [text|
      #iframe-hold {
        background: url(loading.svg) center center no-repeat; }
      |]
    div_ [id_ "iframe-hold"] $
      iframe_ [
        makeAttribute "frameborder" "0",
        makeAttribute "allowtransparency" "true",
        makeAttribute "scrolling" "no",
        width_ "450",
        height_ "197",
        style_ "display:block;margin:auto;",
        src_ "https://money.yandex.ru/embed/shop.xml\
             \?account=410011616040682\
             \&quickpay=shop\
             \&payment-type-choice=on\
             \&mobile-payment-type-choice=on\
             \&writer=seller\
             \&targets=Haskell+guide\
             \&targets-hint=\
             \&default-sum=1000\
             \&button-text=04\
             \&successURL=" ] ""

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
        JS.showHelp (selectId "help", helpVersion)

    section "expanded" [noScriptShown] $ do
      textButton "hide help" $
        JS.hideHelp (selectId "help", helpVersion)
      -- Don't forget to change 'helpVersion' when the text changes
      -- substantially and you think the users should reread it
      help <- liftIO $ T.readFile "static/help.md"
      renderMarkdownBlock help

helpVersion :: Int
helpVersion = 1

-- TODO: when conflicts happen, maybe create an alert like “The thing you're
-- editing has been edited in the meantime. Here is a link with a diff of
-- your variant and the other person's variant. Please merge the changes
-- manually and submit them again, or press this button and we'll merge the
-- changes for you (don't worry, it's not a big deal for us). Thanks!”

-- TODO: automatic merge should be possible too (e.g. if the changes are in
-- different paragraphs)

-- TODO: [very-easy] rename selectChild to selectChildren

renderCategoryList :: [Category] -> HtmlT IO ()
renderCategoryList cats =
  div_ [id_ "categories"] $
    mapM_ renderCategory cats

renderCategoryTitle :: Category -> HtmlT IO ()
renderCategoryTitle category = do
  let thisId = "category-title-" <> uidToText (category^.uid)
      this   = selectId thisId
  -- TODO: once pagination or something is implemented, we'll have to see
  -- whether an anchor has been used in the query string and load the
  -- necessary category if so
  h2_ [id_ thisId] $ do
    a_ [class_ "anchor", href_ ("/#" <> uidToText (category^.uid))] "#"

    sectionSpan "normal" [shown, noScriptShown] $ do
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
      this   = selectId thisId
  div_ [id_ thisId] $ do

    section "normal" [shown, noScriptShown] $ do
      if T.null (category^.notes)
        then p_ "write something here!"
        else renderMarkdownBlock (category^.notes)
      textButton "edit description" $
        JS.switchSection (this, "editing" :: Text)

    section "editing" [] $
      markdownEditor
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
renderItem cat item =
  div_ [id_ ("item-" <> uidToText (item^.uid)), class_ "item"] $ do
    renderItemInfo cat item
    -- TODO: replace “edit description” with a big half-transparent pencil
    -- to the left of it
    renderItemDescription cat item
    renderItemTraits cat item
    -- TODO: [very-easy] add a separator here?
    renderItemNotes cat item

-- TODO: some spinning thingy that spins in the corner of the page while a
-- request is happening

-- TODO: find some way to give all functions access to category and item (or
-- category, item and trait) without passing everything explicitly?

-- TODO: warn when a library isn't on Hackage but is supposed to be
-- TODO: give a link to oldest available docs when the new docs aren't there
renderItemInfo :: Category -> Item -> HtmlT IO ()
renderItemInfo cat item = do
  let bg = hueToDarkColor $ getItemHue cat item
  let thisId = "item-info-" <> uidToText (item^.uid)
      this   = selectId thisId
  div_ [id_ thisId, class_ "item-info",
        style_ ("background-color:" <> bg)] $ do

    section "normal" [shown, noScriptShown] $ do
      -- TODO: [very-easy] move this style_ into css.css
      span_ [style_ "font-size:150%"] $ do
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
      emptySpan "2em"
      toHtml (fromMaybe "other" (item^.group_))
      span_ [class_ "controls"] $ do
        let itemNode = selectId ("item-" <> uidToText (item^.uid))
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
      -- otherNodes are all nodes that have to be recolored when this node is
      -- recolored
      let otherNodes = selectChild (selectParent this)
                                   (selectClass "item-body")
      let formSubmitHandler formNode =
            JS.submitItemInfo (this, otherNodes, item^.uid, formNode)
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
        newGroupInputId <- randomUid
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

renderItemDescription :: Category -> Item -> HtmlT IO ()
renderItemDescription category item = do
  let bg = hueToLightColor $ getItemHue category item
  -- If the structure of HTML changes here, don't forget to update the
  -- 'otherNodes' selector in 'renderItemInfo'. Specifically, we depend on
  -- having a div with a class “item-body” here.
  let thisId = "item-description-" <> uidToText (item^.uid)
      this   = selectId thisId
  div_ [id_ thisId, class_ "item-description item-body",
        style_ ("background-color:" <> bg)] $ do

    section "normal" [shown, noScriptShown] $ do
      if T.null (item^.description)
        then p_ "write something here!"
        else renderMarkdownBlock (item^.description)
      textButton "edit description" $
        JS.switchSection (this, "editing" :: Text)

    section "editing" [] $
      markdownEditor
        (item^.description)
        (\val -> JS.submitItemDescription (this, item^.uid, val))
        (JS.switchSection (this, "normal" :: Text))

renderItemTraits :: Category -> Item -> HtmlT IO ()
renderItemTraits cat item = do
  let bg = hueToLightColor $ getItemHue cat item
  -- If the structure of HTML changes here, don't forget to update the
  -- 'otherNodes' selector in 'renderItemInfo'. Specifically, we depend on
  -- having a div with a class “item-body” here.
  div_ [class_ "item-traits item-body",
        style_ ("background-color:" <> bg)] $ do
    this <- thisNode
    div_ [class_ "traits-groups-container"] $ do
      div_ [class_ "traits-group"] $ do
        p_ "Pros:"
        listNode <- ul_ $ do
          mapM_ (renderTrait (item^.uid)) (item^.pros)
          thisNode
        section "editable" [] $
          smallMarkdownEditor
            [rows_ "3", placeholder_ "add pro"]
            ""
            (\val -> JS.addPro (listNode, item^.uid, val) <>
                     JS.assign val ("" :: Text))
            Nothing
      -- TODO: [easy] maybe add a separator explicitly? instead of CSS
      div_ [class_ "traits-group"] $ do
        p_ "Cons:"
        -- TODO: [easy] maybe add a line here?
        listNode <- ul_ $ do
          mapM_ (renderTrait (item^.uid)) (item^.cons)
          thisNode
        section "editable" [] $
          smallMarkdownEditor
            [rows_ "3", placeholder_ "add con"]
            ""
            (\val -> JS.addCon (listNode, item^.uid, val) <>
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
      this   = selectId thisId
  li_ [id_ thisId] $ do

    sectionSpan "normal" [shown, noScriptShown] $ do
      renderMarkdownLine (trait^.content)

    section "editable" [] $ do
      renderMarkdownLine (trait^.content)
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
renderItemNotes :: Category -> Item -> HtmlT IO ()
renderItemNotes category item = do
  let bg = hueToLightColor $ getItemHue category item
  -- If the structure of HTML changes here, don't forget to update the
  -- 'otherNodes' selector in 'renderItemInfo'. Specifically, we depend on
  -- having a div with a class “item-body” here.
  let thisId = "item-notes-" <> uidToText (item^.uid)
      this   = selectId thisId
  div_ [id_ thisId, class_ "item-notes item-body",
        style_ ("background-color:" <> bg)] $ do
    -- TODO: this duplicates code from renderCategoryNotes, try to reduce
    -- duplication

    section "collapsed" [shown] $ do
      -- TODO: when notes are hidden, show a list of headers in the notes
      textButton "show notes/examples" $
        JS.switchSection (this, "expanded" :: Text)

    section "expanded" [noScriptShown] $ do
      let buttons = do
            textButton "edit notes" $
              JS.switchSection (this, "editing" :: Text)
            emptySpan "1em"
            textButton "hide notes" $
              JS.switchSection (this, "collapsed" :: Text)
      buttons
      if T.null (item^.notes)
        then p_ "add something!"
        else renderMarkdownBlock (item^.notes)
      buttons
      -- TODO: [easy] the lower “hide notes” should scroll back to item when
      -- the notes are closed (but don't scroll if it's already visible after
      -- the notes have been hidden)

    section "editing" [] $ do
      contents <- if T.null (item^.notes)
                    then liftIO $ T.readFile "static/item-notes-template.md"
                    else return (item^.notes)
      markdownEditor
        contents
        (\val -> JS.submitItemNotes (this, item^.uid, val))
        (JS.switchSection (this, "expanded" :: Text))

-- TODO: a shortcut for editing (when you press Ctrl-something, whatever was
-- selected becomes editable)

-- TODO: a function to find all links to Hackage that have version in them

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
  :: Text         -- ^ Default text
  -> (JS -> JS)   -- ^ “Submit” handler, receiving the contents of the editor
  -> JS           -- ^ “Cancel” handler
  -> HtmlT IO ()
markdownEditor s submit cancel = do
  textareaId <- randomUid
  -- Autocomplete has to be turned off thanks to
  -- <http://stackoverflow.com/q/8311455>.
  textarea_ [uid_ textareaId, autocomplete_ "off",
             rows_ "10", class_ "big fullwidth"] $
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
  -> Text         -- ^ Default text
  -> (JS -> JS)   -- ^ “Submit” handler, receiving the contents of the editor
  -> Maybe JS     -- ^ “Cancel” handler (if “Cancel” is needed)
  -> HtmlT IO ()
smallMarkdownEditor attributes s submit mbCancel = do
  textareaId <- randomUid
  let val = JS $ format "document.getElementById(\"{}\").value" [textareaId]
  textarea_ ([class_ "fullwidth", uid_ textareaId, autocomplete_ "off",
              onEnter (submit val)] ++ attributes) $
    toHtml s
  case mbCancel of
    Nothing -> return ()
    Just cancel -> do
      br_ []
      textButton "cancel" $
        JS.assign val s <>
        cancel

uid_ :: Uid -> Attribute
uid_ = id_ . uidToText

newtype JQuerySelector = JQuerySelector Text
  deriving (ToJS, Format.Buildable)

selectId :: Text -> JQuerySelector
selectId x = JQuerySelector $ format "#{}" [x]

selectUid :: Uid -> JQuerySelector
selectUid x = JQuerySelector $ format "#{}" [x]

selectClass :: Text -> JQuerySelector
selectClass x = JQuerySelector $ format ".{}" [x]

selectParent :: JQuerySelector -> JQuerySelector
selectParent x = JQuerySelector $ format ":has(> {})" [x]

selectChild :: JQuerySelector -> JQuerySelector -> JQuerySelector
selectChild a b = JQuerySelector $ format "{} > {}" (a, b)

thisNode :: HtmlT IO JQuerySelector
thisNode = do
  uid' <- randomUid
  -- If the class name ever changes, fix 'JS.moveNodeUp' and
  -- 'JS.moveNodeDown'.
  span_ [uid_ uid', class_ "dummy"] mempty
  return (selectParent (selectUid uid'))

-- TODO: add an “ecosystem” field with related packages/etc (just a simple
-- Markdown-edited field under pros/cons)

-- Wheh changing these, also look at 'JS.switchSection'.

shown, noScriptShown :: Attribute
shown          = class_ " shown "
noScriptShown  = class_ " noscript-shown "

section :: Monad m => Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
section t attrs = div_ (class_ (t <> " section ") : attrs)

sectionSpan :: Monad m => Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
sectionSpan t attrs = span_ (class_ (t <> " section ") : attrs)

-- TODO: why not compare Haskellers too? e.g. for April Fools' we could ask
-- people to list their pros and cons

-- TODO: add something to edit a particular paragraph of the notes

newGroupValue :: Text
newGroupValue = "-new-group-"

-- TODO: is it indexable by Google? <given that we're hiding text and
-- Googlebot can execute Javascript>
