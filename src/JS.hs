{-# LANGUAGE
FlexibleInstances,
GeneralizedNewtypeDeriving,
OverloadedStrings,
QuasiQuotes,
BangPatterns,
NoImplicitPrelude
  #-}


-- TODO: try to make it more type-safe somehow?

-- TODO: maybe use jmacro or something?
module JS where


-- General
import BasePrelude
-- Text
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
-- Formatting and interpolation
import qualified Data.Text.Buildable as Format
import NeatInterpolation

-- Local
import Utils


-- | Javascript code.
newtype JS = JS {fromJS :: Text}
  deriving (Show, Format.Buildable, Monoid)

-- | A concatenation of all Javascript functions defined in this module.
allJSFunctions :: JS
allJSFunctions = JS . T.unlines . map fromJS $ [
  -- Utilities
  replaceWithData, prependData, appendData,
  moveNodeUp, moveNodeDown,
  switchSection, switchSectionsEverywhere,
  fadeIn, fadeOutAndRemove,
  setMonospace,
  -- Help
  showOrHideHelp, showHelp, hideHelp,
  -- Misc
  createAjaxIndicator,
  autosizeTextarea,
  expandHash,
  expandItemNotes,
  -- Creating parts of interface
  makeTraitEditor,
  makeItemNotesEditor,
  -- Add methods
  addCategoryAndRedirect, addItem,
  addPro, addCon,
  -- Set methods
  submitCategoryTitle, submitItemDescription, submitCategoryNotes,
  submitItemInfo, submitItemNotes, submitItemEcosystem,
  submitTrait,
  -- Other things
  deleteCategory,
  moveTraitUp, moveTraitDown, deleteTrait,
  moveItemUp, moveItemDown, deleteItem,
  -- Admin things
  acceptEdit, undoEdit,
  acceptBlock, undoBlock,
  createCheckpoint ]

-- | A class for things that can be converted to Javascript syntax.
class ToJS a where toJS :: a -> JS

instance ToJS Bool where
  toJS True  = JS "true"
  toJS False = JS "false"
instance ToJS JS where
  toJS = id
instance ToJS Text where
  toJS = JS . escapeJSString
instance ToJS Integer where
  toJS = JS . tshow
instance ToJS Int where
  toJS = JS . tshow
instance ToJS (Uid a) where
  toJS = toJS . uidToText

-- | A helper class for calling Javascript functions.
class JSParams a where
  jsParams :: a -> [JS]

instance JSParams () where
  jsParams () = []
instance ToJS a => JSParams [a] where
  jsParams = map toJS
instance (ToJS a,ToJS b) => JSParams (a,b) where
  jsParams (a,b) = [toJS a, toJS b]
instance (ToJS a,ToJS b,ToJS c) => JSParams (a,b,c) where
  jsParams (a,b,c) = [toJS a, toJS b, toJS c]
instance (ToJS a,ToJS b,ToJS c,ToJS d) => JSParams (a,b,c,d) where
  jsParams (a,b,c,d) = [toJS a, toJS b, toJS c, toJS d]
instance (ToJS a,ToJS b,ToJS c,ToJS d,ToJS e) => JSParams (a,b,c,d,e) where
  jsParams (a,b,c,d,e) = [toJS a, toJS b, toJS c, toJS d, toJS e]
instance (ToJS a,ToJS b,ToJS c,ToJS d,ToJS e,ToJS f) => JSParams (a,b,c,d,e,f) where
  jsParams (a,b,c,d,e,f) = [toJS a, toJS b, toJS c, toJS d, toJS e, toJS f]

{- | This hacky class lets you construct and use Javascript functions; you give 'makeJSFunction' function name, function parameters, and function body, and you get a polymorphic value of type @JSFunction a => a@, which you can use either as a complete function definition (if you set @a@ to be @JS@), or as a function that you can give some parameters and it would return a Javascript call:

> plus = makeJSFunction "plus" ["a", "b"] "return a+b;"

>>> plus :: JS
JS "function plus(a,b) {\nreturn a+b;}\n"
>>> plus (3, 5) :: JS
JS "plus(3,5);"
-}
class JSFunction a where
  makeJSFunction
    :: Text          -- ^ Name
    -> [Text]        -- ^ Parameter names
    -> Text          -- ^ Definition
    -> a

-- This generates function definition
instance JSFunction JS where
  makeJSFunction fName fParams fDef =
    JS $ format "function {}({}) {\n{}}\n"
                (fName, T.intercalate "," fParams, fDef)

-- This generates a function that takes arguments and produces a Javascript
-- function call
instance JSParams a => JSFunction (a -> JS) where
  makeJSFunction fName _fParams _fDef = \args ->
    JS $ format "{}({});"
                (fName, T.intercalate "," (map fromJS (jsParams args)))

-- This isn't a standalone function and so it doesn't have to be listed in
-- 'allJSFunctions'.
assign :: ToJS x => JS -> x -> JS
assign v x = JS $ format "{} = {};" (v, toJS x)

-- TODO: all links here shouldn't be absolute [absolute-links]

replaceWithData :: JSFunction a => a
replaceWithData =
  makeJSFunction "replaceWithData" ["node"]
  [text|
    return function(data) {$(node).replaceWith(data);};
  |]

prependData :: JSFunction a => a
prependData =
  makeJSFunction "prependData" ["node"]
  [text|
    return function(data) {$(node).prepend(data);};
  |]

appendData :: JSFunction a => a
appendData =
  makeJSFunction "appendData" ["node"]
  [text|
    return function(data) {$(node).append(data);};
  |]

-- | Move node up (in a list of sibling nodes), ignoring anchor elements
-- inserted by 'thisNode'.
moveNodeUp :: JSFunction a => a
moveNodeUp =
  makeJSFunction "moveNodeUp" ["node"]
  [text|
    var el = $(node);
    while (el.prev().is(".dummy"))
      el.prev().before(el);
    if (el.not(':first-child'))
      el.prev().before(el);
  |]

-- | Move node down (in a list of sibling nodes), ignoring anchor elements
-- inserted by 'thisNode'.
moveNodeDown :: JSFunction a => a
moveNodeDown =
  makeJSFunction "moveNodeDown" ["node"]
  [text|
    var el = $(node);
    while (el.next().is(".dummy"))
      el.next().after(el);
    if (el.not(':last-child'))
      el.next().after(el);
  |]

-- | Given something that contains section divs (or spans), show one and
-- hide the rest. The div/span with the given @class@ will be chosen.
--
-- See Note [show-hide]
switchSection :: JSFunction a => a
switchSection =
  makeJSFunction "switchSection" ["node", "section"]
  [text|
    $(node).children(".section").removeClass("shown");
    $(node).children(".section."+section).addClass("shown");
    // See Note [autosize]
    autosize($('textarea'));
    autosize.update($('textarea'));
  |]

-- | Switch sections /everywhere/ inside the container.
--
-- See Note [show-hide]
switchSectionsEverywhere :: JSFunction a => a
switchSectionsEverywhere =
  makeJSFunction "switchSectionsEverywhere" ["node", "section"]
  [text|
    $(node).find(".section").removeClass("shown");
    $(node).find(".section."+section).addClass("shown");
    // See Note [autosize]
    autosize($('textarea'));
    autosize.update($('textarea'));
  |]

-- | This function makes the node half-transparent and then animates it to
-- full opaqueness. It's useful when e.g. something has been moved and you
-- want to “flash” the item to draw user's attention to it.
fadeIn :: JSFunction a => a
fadeIn =
  makeJSFunction "fadeIn" ["node"]
  [text|
    $(node).fadeTo(0,0.2).fadeTo(600,1);
  |]

-- | This function animates the node to half-transparency and then removes it
-- completely. It's useful when you're removing something and you want to
-- draw user's attention to the fact that it's being removed.
--
-- The reason there isn't a simple @fadeOut@ utility function here is that
-- removal has to be done by passing a callback to @fadeTo@. In jQuery you
-- can't simply wait until the animation has stopped.
fadeOutAndRemove :: JSFunction a => a
fadeOutAndRemove =
  makeJSFunction "fadeOutAndRemove" ["node"]
  [text|
     $(node).fadeTo(400,0.2,function(){$(node).remove()});
  |]

setMonospace :: JSFunction a => a
setMonospace =
  makeJSFunction "setMonospace" ["node", "p"]
  [text|
    if (p)
      $(node).css("font-family", "monospace")
    else
      $(node).css("font-family", "");
    // See Note [autosize]; the size of the textarea will definitely change
    // after the font has been changed
    autosize.update($(node));
  |]

showHelp :: JSFunction a => a
showHelp =
  makeJSFunction "showHelp" ["node", "version"]
  [text|
    localStorage.removeItem("help-hidden-"+version);
    switchSection(node, "expanded");
  |]

hideHelp :: JSFunction a => a
hideHelp =
  makeJSFunction "hideHelp" ["node", "version"]
  [text|
    localStorage.setItem("help-hidden-"+version, "");
    switchSection(node, "collapsed");
  |]

-- TODO: find a better name for this (to distinguish it from 'showHelp' and
-- 'hideHelp')
showOrHideHelp :: JSFunction a => a
showOrHideHelp =
  makeJSFunction "showOrHideHelp" ["node", "version"]
  [text|
    if (localStorage.getItem("help-hidden-"+version) === null)
      showHelp(node, version)
    else
      hideHelp(node, version);
  |]

createAjaxIndicator :: JSFunction a => a
createAjaxIndicator =
  makeJSFunction "createAjaxIndicator" []
  [text|
    $("body").prepend('<div id="ajax-indicator"></div>');
    $(document).ajaxStart(function() {
      $("#ajax-indicator").show();
    });
    $(document).ajaxStop(function() {
      $("#ajax-indicator").hide();
    });
    $("#ajax-indicator").hide();
  |]

autosizeTextarea :: JSFunction a => a
autosizeTextarea =
  makeJSFunction "autosizeTextarea" ["textareaNode"]
  [text|
    autosize(textareaNode);
    autosize.update(textareaNode);
  |]

-- | Read the anchor from the address bar (i.e. the thing after #) and use it
-- to expand something (e.g. notes). It's needed to implement linking
-- properly – e.g. notes are usually unexpanded, but when you're giving
-- someone a direct link to notes, it makes sense to expand them. If you call
-- 'expandHash' after the page has loaded, it will do just that.
expandHash :: JSFunction a => a
expandHash =
  makeJSFunction "expandHash" []
  [text|
    hash = $(location).attr('hash');
    if (hash.slice(0,12) == "#item-notes-") {
      if (hash.indexOf('-', 12) != (-1))
        // For links to sections of items' notes (from the TOC)
        itemId = hash.slice(12, hash.indexOf('-', 12))
      else
        // For links to items' notes
        itemId = hash.slice(12);
      expandItemNotes(itemId);
    } else
    if (hash.slice(0,6) == "#item-") {
      itemId = hash.slice(6);
      expandItemNotes(itemId);
    }
  |]

expandItemNotes :: JSFunction a => a
expandItemNotes =
  makeJSFunction "expandItemNotes" ["itemId"]
  [text|
    switchSection("#item-notes-"+itemId, "expanded");
  |]

makeTraitEditor :: JSFunction a => a
makeTraitEditor =
  makeJSFunction "makeTraitEditor"
    ["traitNode", "sectionNode", "textareaUid", "content", "itemId", "traitId"]
  [text|
    $(sectionNode).html("");
    area = $("<textarea>", {
      "autocomplete" : "off",
      "rows"         : "5",
      "id"           : textareaUid,
      "class"        : "fullwidth",
      "text"         : content })[0];
    area.onkeydown = function (event) {
      if (event.keyCode == 13) {
        submitTrait(traitNode, itemId, traitId, area.value);
        return false; } };
    br = $("<br>")[0];
    a = $("<a>", {
      "href"    : "#",
      "text"    : "cancel" })[0];
    a.onclick = function () {
      $(sectionNode).html("");
      switchSection(traitNode, "editable");
      return false; };
    cancelBtn = $("<span>", {"class":"text-button"})[0];
    $(cancelBtn).append(a);
    $(sectionNode).append(area, br, cancelBtn);
  |]

makeItemNotesEditor :: JSFunction a => a
makeItemNotesEditor =
  makeJSFunction "makeItemNotesEditor"
    ["notesNode", "sectionNode", "textareaUid", "content", "itemId"]
  [text|
    $(sectionNode).html("");
    area = $("<textarea>", {
      "autocomplete" : "off",
      "rows"         : "10",
      "id"           : textareaUid,
      "class"        : "big fullwidth",
      "text"         : content })[0];
    saveBtn = $("<input>", {
      "value" : "Save",
      "type"  : "button" })[0];
    saveBtn.onclick = function () {
      submitItemNotes(notesNode, itemId, area.value); };
    // Can't use $()-generation here because then the <span> would have
    // to be cloned (since we're inserting it multiple times) and I don't
    // know how to do that.
    space = "<span style='margin-left:6px'></span>";
    cancelBtn = $("<input>", {
      "value" : "Cancel",
      "type"  : "button" })[0];
    cancelBtn.onclick = function () {
      $(sectionNode).html("");
      switchSection(notesNode, "expanded"); };
    monospace = $("<input>", {
      "name" : "monospace",
      "type" : "checkbox" })[0];
    monospace.onchange = function () {
      setMonospace(area, monospace.checked); };
    monospaceLabel = $("<label>")[0];
    $(monospaceLabel).append(monospace, "monospace editor");
    $(sectionNode).append(
      area, saveBtn, $(space), cancelBtn, $(space),
      "Markdown", $(space), monospaceLabel);
  |]

-- | Create a new category and redirect to it (or redirect to an old category
-- if it exists already).
addCategoryAndRedirect :: JSFunction a => a
addCategoryAndRedirect =
  makeJSFunction "addCategoryAndRedirect" ["s"]
  [text|
    $.post("/haskell/add/category", {content: s})
     .done(function (url) {
        window.location.href = url;
      });
  |]

-- | Add a new item to some category.
addItem :: JSFunction a => a
addItem =
  makeJSFunction "addItem" ["node", "catId", "s"]
  [text|
    $.post("/haskell/add/category/"+catId+"/item", {name: s})
     .done(appendData(node));
  |]

{- |
Finish category title editing (this happens when you submit the field).

This turns the title with the editbox back into a simple text title.
-}
submitCategoryTitle :: JSFunction a => a
submitCategoryTitle =
  makeJSFunction "submitCategoryTitle" ["node", "catId", "s"]
  [text|
    $.post("/haskell/set/category/"+catId+"/title", {content: s})
     .done(replaceWithData(node));
  |]

submitCategoryNotes :: JSFunction a => a
submitCategoryNotes =
  makeJSFunction "submitCategoryNotes" ["node", "catId", "s"]
  [text|
    $.post("/haskell/set/category/"+catId+"/notes", {content: s})
     .done(replaceWithData(node));
  |]

submitItemDescription :: JSFunction a => a
submitItemDescription =
  makeJSFunction "submitItemDescription" ["node", "itemId", "s"]
  [text|
    $.post("/haskell/set/item/"+itemId+"/description", {content: s})
     .done(replaceWithData(node));
  |]

submitItemEcosystem :: JSFunction a => a
submitItemEcosystem =
  makeJSFunction "submitItemEcosystem" ["node", "itemId", "s"]
  [text|
    $.post("/haskell/set/item/"+itemId+"/ecosystem", {content: s})
     .done(replaceWithData(node));
  |]

submitItemNotes :: JSFunction a => a
submitItemNotes =
  makeJSFunction "submitItemNotes" ["node", "itemId", "s"]
  [text|
    $.post("/haskell/set/item/"+itemId+"/notes", {content: s})
     .done(function (data) {
        $(node).replaceWith(data);
        switchSection(node, "expanded");
      });
    // Switching has to be done here and not in 'Main.renderItemNotes'
    // because $.post is asynchronous and will be done *after*
    // switchSection has worked.
  |]

-- | Add a pro to some item.
addPro :: JSFunction a => a
addPro =
  makeJSFunction "addPro" ["node", "itemId", "s"]
  [text|
    $.post("/haskell/add/item/"+itemId+"/pro", {content: s})
     .done(function (data) {
        var jData = $(data);
        jData.appendTo(node);
        switchSection(jData, "editable");
      });
  |]

-- | Add a con to some item.
addCon :: JSFunction a => a
addCon =
  makeJSFunction "addCon" ["node", "itemId", "s"]
  [text|
    $.post("/haskell/add/item/"+itemId+"/con", {content: s})
     .done(function (data) {
        var jData = $(data);
        jData.appendTo(node);
        switchSection(jData, "editable");
      });
  |]

submitTrait :: JSFunction a => a
submitTrait =
  makeJSFunction "submitTrait" ["node", "itemId", "traitId", "s"]
  [text|
    $.post("/haskell/set/item/"+itemId+"/trait/"+traitId, {content: s})
     .done(function (data) {
        $(node).replaceWith(data);
        switchSection(node, "editable");
      });
    // Switching has to be done here and not in 'Main.renderTrait'
    // because $.post is asynchronous and will be done *after*
    // switchSection has worked.
  |]

submitItemInfo :: JSFunction a => a
submitItemInfo =
  makeJSFunction "submitItemInfo" ["infoNode", "bodyNode", "itemId", "form"]
  [text|
    custom = $(form)[0].elements["custom-group"].value;
    items = $(form).closest(".items").find(".item");
    // If the group was changed, we need to recolor the whole item,
    // but we don't want to rerender the item on the server because
    // it would lose the item's state (e.g. what if the traits were
    // being edited? etc). So, instead we query colors from the server
    // and change the color of the item's body manually.
    $.post("/haskell/set/item/"+itemId+"/info", $(form).serialize())
     .done(function (data) {
        // Note the order – first we change the color, then we replace
        // the info node. The reason is that otherwise the bodyNode
        // selector might become invalid (if it depends on the infoNode
        // selector).
        $.get("/haskell/render/item/"+itemId+"/colors")
         .done(function (colors) {
            $(bodyNode).css("background-color", colors.light);
            $(infoNode).replaceWith(data);
         });
        // And now, if a custom group was created, we should add it to other
        // items' lists.
        if (custom != "") {
          items.each(function (i, item) {
            groups = $(item).find("select[name=group]")[0];
            isOurOption = function (opt) {return opt.text == custom};
            alreadyExists = $.grep(groups.options, isOurOption).length > 0;
            if (!alreadyExists) {
              groups.add(new Option(custom, custom), 1); }
          });
        }
     });
  |]

deleteCategory :: JSFunction a => a
deleteCategory =
  makeJSFunction "deleteCategory" ["catId", "catNode"]
  [text|
    if (confirm("Confirm deletion?")) {
      $.post("/haskell/delete/category/"+catId)
       .done(function () {
          fadeOutAndRemove(catNode);
       });
    }
  |]

moveTraitUp :: JSFunction a => a
moveTraitUp =
  makeJSFunction "moveTraitUp" ["itemId", "traitId", "traitNode"]
  [text|
    $.post("/haskell/move/item/"+itemId+"/trait/"+traitId, {direction: "up"})
     .done(function () {
        moveNodeUp(traitNode);
        fadeIn(traitNode);
     });
  |]

moveTraitDown :: JSFunction a => a
moveTraitDown =
  makeJSFunction "moveTraitDown" ["itemId", "traitId", "traitNode"]
  [text|
    $.post("/haskell/move/item/"+itemId+"/trait/"+traitId, {direction: "down"})
     .done(function () {
        moveNodeDown(traitNode);
        fadeIn(traitNode);
     });
  |]

deleteTrait :: JSFunction a => a
deleteTrait =
  makeJSFunction "deleteTrait" ["itemId", "traitId", "traitNode"]
  [text|
    if (confirm("Confirm deletion?")) {
      $.post("/haskell/delete/item/"+itemId+"/trait/"+traitId)
       .done(function () {
          fadeOutAndRemove(traitNode);
       });
    }
  |]

moveItemUp :: JSFunction a => a
moveItemUp =
  makeJSFunction "moveItemUp" ["itemId", "itemNode"]
  [text|
    $.post("/haskell/move/item/"+itemId, {direction: "up"})
     .done(function () {
        moveNodeUp(itemNode);
        fadeIn(itemNode);
     });
  |]

moveItemDown :: JSFunction a => a
moveItemDown =
  makeJSFunction "moveItemDown" ["itemId", "itemNode"]
  [text|
    $.post("/haskell/move/item/"+itemId, {direction: "down"})
     .done(function () {
        moveNodeDown(itemNode);
        fadeIn(itemNode);
     });
  |]

acceptEdit :: JSFunction a => a
acceptEdit =
  makeJSFunction "acceptEdit" ["editId", "editNode"]
  [text|
    $.post("/admin/edit/"+editId+"/accept")
     .done(function () {
        fadeOutAndRemove(editNode);
     });
  |]

undoEdit :: JSFunction a => a
undoEdit =
  makeJSFunction "undoEdit" ["editId", "editNode"]
  [text|
    $.post("/admin/edit/"+editId+"/undo")
     .done(function (data) {
        if (data == "")
          fadeOutAndRemove(editNode);
        else
          alert("couldn't undo edit: " + data);
     });
  |]

acceptBlock :: JSFunction a => a
acceptBlock =
  makeJSFunction "acceptBlock" ["editLatest", "editEarliest", "blockNode"]
  [text|
    $.post("/admin/edits/"+editLatest+"/"+editEarliest+"/accept")
     .done(function () {
        fadeOutAndRemove(blockNode);
     });
  |]

undoBlock :: JSFunction a => a
undoBlock =
  makeJSFunction "undoBlock" ["editLatest", "editEarliest", "blockNode"]
  [text|
    $.post("/admin/edits/"+editLatest+"/"+editEarliest+"/undo")
     .done(function (data) {
        if (data == "")
          fadeOutAndRemove(blockNode);
        else
          $(blockNode).replaceWith(data);
     });
  |]

createCheckpoint :: JSFunction a => a
createCheckpoint =
  makeJSFunction "createCheckpoint" ["buttonNode"]
  [text|
    $.post("/admin/create-checkpoint")
     .done(function () {
        fadeIn(buttonNode);
     });
  |]

deleteItem :: JSFunction a => a
deleteItem =
  makeJSFunction "deleteItem" ["itemId", "itemNode"]
  [text|
    if (confirm("Confirm deletion?")) {
      $.post("/haskell/delete/item/"+itemId)
       .done(function () {
          fadeOutAndRemove(itemNode);
       });
    }
  |]

-- When adding a function, don't forget to add it to 'allJSFunctions'!

escapeJSString :: Text -> Text
escapeJSString s =
    TL.toStrict . B.toLazyText $
    B.singleton '"' <> quote s <> B.singleton '"'
  where
    quote q = case T.uncons t of
      Nothing       -> B.fromText h
      Just (!c, t') -> B.fromText h <> escape c <> quote t'
      where
        (h, t) = T.break isEscape q
    -- 'isEscape' doesn't mention \n, \r and \t because they are handled by
    -- the “< '\x20'” case; yes, later 'escape' escapes them differently,
    -- but it's irrelevant
    isEscape c = c == '\"' || c == '\\' ||
                 c == '\x2028' || c == '\x2029' ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
      | c < '\x20' || c == '\x2028' || c == '\x2029' =
          B.fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
      | otherwise =
          B.singleton c
      where
        h = showHex (fromEnum c) ""

newtype JQuerySelector = JQuerySelector Text
  deriving (ToJS, Format.Buildable)

selectId :: Text -> JQuerySelector
selectId x = JQuerySelector $ format "#{}" [x]

selectUid :: Uid Node -> JQuerySelector
selectUid x = JQuerySelector $ format "#{}" [x]

selectClass :: Text -> JQuerySelector
selectClass x = JQuerySelector $ format ".{}" [x]

selectParent :: JQuerySelector -> JQuerySelector
selectParent x = JQuerySelector $ format ":has(> {})" [x]

selectChildren :: JQuerySelector -> JQuerySelector -> JQuerySelector
selectChildren a b = JQuerySelector $ format "{} > {}" (a, b)
