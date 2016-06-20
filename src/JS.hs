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
import qualified Data.Text.All as T
import Data.Text.All (Text)
-- Interpolation
import NeatInterpolation

-- Local
import Utils


-- | Javascript code.
newtype JS = JS {fromJS :: Text}
  deriving (Show, T.Buildable, Monoid)

-- | A concatenation of all Javascript functions defined in this module.
allJSFunctions :: JS
allJSFunctions = JS . T.unlines . map fromJS $ [
  -- Utilities
  replaceWithData, prependData, appendData,
  moveNodeUp, moveNodeDown,
  switchSection, switchSectionsEverywhere,
  fadeIn, fadeOutAndRemove,
  -- Misc
  createAjaxIndicator,
  autosizeTextarea,
  expandHash,
  expandItemNotes,
  showDiffPopup,
  -- Creating parts of interface
  makeTraitEditor,
  makeItemNotesEditor,
  -- Add methods
  addCategoryAndRedirect, addItem,
  addPro, addCon,
  -- Set methods
  submitCategoryInfo, submitCategoryNotes,
  submitItemDescription,
  submitItemInfo, submitItemNotes, submitItemEcosystem,
  submitTrait,
  -- Other things
  deleteCategoryAndRedirect,
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
  toJS = JS . T.show
instance ToJS Int where
  toJS = JS . T.show
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
    JS $ T.format "function {}({}) {\n{}}\n"
                  (fName, T.intercalate "," fParams, fDef)

-- This generates a function that takes arguments and produces a Javascript
-- function call
instance JSParams a => JSFunction (a -> JS) where
  makeJSFunction fName _fParams _fDef = \args ->
    JS $ T.format "{}({});"
                  (fName, T.intercalate "," (map fromJS (jsParams args)))

-- This isn't a standalone function and so it doesn't have to be listed in
-- 'allJSFunctions'.
assign :: ToJS x => JS -> x -> JS
assign v x = JS $ T.format "{} = {};" (v, toJS x)

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

showDiffPopup :: JSFunction a => a
showDiffPopup =
  makeJSFunction "showDiffPopup" ["ours", "modified", "merged", "send"]
  [text|
    dialog = $("<div>", {
      "class" : "diff-popup"
    })[0];
    choices = $("<div>", {
      "class" : "diff-choices"
    })[0];

    // our version
    choiceOurs = $("<div>", {
      "class" : "var-a" })[0];
    textOurs = $("<div>", {
      "class" : "text",
      "text"  : ours })[0];
    headerOurs = $("<strong>", {
      "text" : "Your version" })[0];
    buttonOurs = $("<button>", {
      "text" : "Submit this version, disregard changes on the server" })[0];
    $(buttonOurs).click(function() {
      send(ours); });
    $(choiceOurs).append(headerOurs, textOurs, buttonOurs);

    // modified version
    choiceMod = $("<div>", {
      "class" : "var-b" })[0];
    textMod = $("<div>", {
      "class" : "text",
      "text"  : modified })[0];
    headerMod = $("<strong>", {
      "text" : "Version on the server" })[0];
    buttonMod = $("<button>", {
      "text" : "Accept this version, disregard my changes" })[0];
    $(buttonMod).click(function() {
      send(modified); });
    $(choiceMod).append(headerMod, textMod, buttonMod);

    // building merged
    choiceMerged = $("<div>", {
      "class" : "var-merged" })[0];
    areaMerged = $("<textarea>", {
      "autocomplete" : "off",
      "text"         : merged })[0];
    headerMerged = $("<strong>", {
      "text" : "Merged version (edit if needed)" })[0];
    buttonMerged = $("<button>", {
      "text" : "Submit the merged version" })[0];
    $(buttonMerged).click(function () {
      send(areaMerged.value); });
    $(choiceMerged).append(headerMerged, areaMerged, buttonMerged);

    $(choices).append(choiceOurs, choiceMod, choiceMerged);
    $(dialog).append(choices);

    $.magnificPopup.open({
      modal: true,
      items: {
        src: dialog,
        type: 'inline' }
    });

    autosizeTextarea(areaMerged);
  |]


{- Note [dynamic interface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

'makeTraitEditor' creates a textbox that appears when you try to edit a pro/con; 'makeItemNotesEditor' creates a textbox that appears when you try to edit item's notes. (Both also create some buttons/etc.)

This is rather inelegant, rather hacky, and in most places we try *not* to create any HTML dynamically, instead relying on sections (see Note [show-hide]). However, in this case we have to – Firefox has a bug that makes loading pages with lots of <textarea>s slow, and so we have to reduce the number of <textarea>s contained on each page.

See <https://github.com/aelve/guide/issues/24>.

-}

-- | Dynamically creates a 'View.smallMarkdownEditor' (but specifically for a
-- trait). See Note [dynamic interface].
makeTraitEditor :: JSFunction a => a
makeTraitEditor =
  makeJSFunction "makeTraitEditor"
                 ["traitNode", "sectionNode", "textareaUid",
                  "content", "itemId", "traitId"]
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
        submitTrait(traitNode, itemId, traitId, content, area.value);
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
    markdown = $("<a>", {
      "href"   : "/markdown",
      "target" : "_blank",
      "style"  : "float:right",
      "text"   : "Markdown" })[0];
    $(cancelBtn).append(a);
    $(sectionNode).append(area, br, cancelBtn, markdown);
  |]

{- Note [blurb diffing]
~~~~~~~~~~~~~~~~~~~~~~~

A note on why we need 'empty' in 'makeItemNotesEditor'.

Assume that the notes are empty. The text in the area, therefore, will be some default blurb (“# Links, #Imports, #Usage”, etc). Suppose the user edits it. What will be sent to the server?

  * original: blurb
  * our version: modified blurb

What will happen next? The server will compare it to the value currently at the server (i.e. an empty string), and think that the blurb *was* on the server but got deleted while the client was doing editing. This is wrong, and will result in a diff popup comparing an edited blurb to an empty string. To prevent this, we pass 'empty' to 'makeItemNotesEditor' – if we're using a blurb, we'll pass an empty string as the original.

-}

-- | Dynamically creates a 'View.markdownEditor' (but specifically for item
-- notes). See Note [dynamic interface].
makeItemNotesEditor :: JSFunction a => a
makeItemNotesEditor =
  -- See Note [blurb diffing]
  makeJSFunction "makeItemNotesEditor"
                 ["notesNode", "sectionNode", "textareaUid",
                  "empty", "content", "itemId"]
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
      submitItemNotes(notesNode, itemId, empty ? "" : content, area.value); };
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
    markdown = $("<a>", {
      "href"   : "/markdown",
      "target" : "_blank",
      "text"   : "Markdown" })[0];
    $(sectionNode).append(
      area, saveBtn, $(space), cancelBtn, $(space), markdown);
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

submitCategoryInfo :: JSFunction a => a
submitCategoryInfo =
  makeJSFunction "submitCategoryInfo" ["infoNode", "catId", "form"]
  [text|
    $.post("/haskell/set/category/"+catId+"/info", $(form).serialize())
     .done(function (data) {
        $(infoNode).replaceWith(data);
        // If pros-cons-enabled and ecosystem-enabled were changed, we
        // have to show/hide relevant sections in all items of the category.
        // See Note [enabled sections] for details.
        if ($(form)[0]["pros-cons-enabled"].checked)
                 $(".pros-cons-wrapper").show();
            else $(".pros-cons-wrapper").hide();
        if ($(form)[0]["ecosystem-enabled"].checked)
                 $(".ecosystem-wrapper").show();
            else $(".ecosystem-wrapper").hide();
     });
  |]

submitCategoryNotes :: JSFunction a => a
submitCategoryNotes =
  makeJSFunction "submitCategoryNotes"
                 ["node", "catId", "original, ours"]
  [text|
    $.post({
      url: "/haskell/set/category/"+catId+"/notes",
      data: {
        original: original,
        content: ours },
      success: function (data) {
        $.magnificPopup.close();
        $(node).replaceWith(data); },
      statusCode: {
        409: function (xhr, st, err) {
          modified = xhr.responseJSON["modified"];
          merged   = xhr.responseJSON["merged"];
          showDiffPopup(ours, modified, merged, function (x) {
            submitCategoryNotes(node, catId, modified, x) }); } }
      });
  |]

submitItemDescription :: JSFunction a => a
submitItemDescription =
  makeJSFunction "submitItemDescription"
                 ["node", "itemId", "original", "ours"]
  [text|
    $.post({
      url: "/haskell/set/item/"+itemId+"/description",
      data: {
        original: original,
        content: ours },
      success: function (data) {
        $.magnificPopup.close();
        $(node).replaceWith(data); },
      statusCode: {
        409: function (xhr, st, err) {
          modified = xhr.responseJSON["modified"];
          merged   = xhr.responseJSON["merged"];
          showDiffPopup(ours, modified, merged, function (x) {
            submitItemDescription(node, itemId, modified, x) }); } }
      });
  |]

submitItemEcosystem :: JSFunction a => a
submitItemEcosystem =
  makeJSFunction "submitItemEcosystem"
                 ["node", "itemId", "original", "ours"]
  [text|
    $.post({
      url: "/haskell/set/item/"+itemId+"/ecosystem",
      data: {
        original: original,
        content: ours },
      success: function (data) {
        $.magnificPopup.close();
        $(node).replaceWith(data); },
      statusCode: {
        409: function (xhr, st, err) {
          modified = xhr.responseJSON["modified"];
          merged   = xhr.responseJSON["merged"];
          showDiffPopup(ours, modified, merged, function (x) {
            submitItemEcosystem(node, itemId, modified, x) }); } }
      });
  |]

submitItemNotes :: JSFunction a => a
submitItemNotes =
  makeJSFunction "submitItemNotes"
                 ["node", "itemId", "original", "ours"]
  [text|
    $.post({
      url: "/haskell/set/item/"+itemId+"/notes",
      data: {
        original: original,
        content: ours },
      success: function (data) {
        $.magnificPopup.close();
        $(node).replaceWith(data);
        // Switching has to be done here and not in 'Main.renderItemNotes'
        // because $.post is asynchronous and will be done *after*
        // switchSection has worked.
        switchSection(node, "expanded"); },
      statusCode: {
        409: function (xhr, st, err) {
          modified = xhr.responseJSON["modified"];
          merged   = xhr.responseJSON["merged"];
          showDiffPopup(ours, modified, merged, function (x) {
            submitItemNotes(node, itemId, modified, x) }); } }
      });
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
  makeJSFunction "submitTrait"
                 ["node", "itemId", "traitId", "original", "ours"]
  [text|
    $.post({
      url: "/haskell/set/item/"+itemId+"/trait/"+traitId,
      data: {
        original: original,
        content: ours },
      success: function (data) {
        $.magnificPopup.close();
        $(node).replaceWith(data);
        // Switching has to be done here and not in 'Main.renderTrait'
        // because $.post is asynchronous and will be done *after*
        // switchSection has worked.
        switchSection(node, "editable"); },
      statusCode: {
        409: function (xhr, st, err) {
          modified = xhr.responseJSON["modified"];
          merged   = xhr.responseJSON["merged"];
          showDiffPopup(ours, modified, merged, function (x) {
            submitTrait(node, itemId, traitId, modified, x) }); } }
      });
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

deleteCategoryAndRedirect :: JSFunction a => a
deleteCategoryAndRedirect =
  makeJSFunction "deleteCategoryAndRedirect" ["catId"]
  [text|
    if (confirm("Confirm deletion?")) {
      $.post("/haskell/delete/category/"+catId)
       .done(function () {
          window.location.href = "/haskell";
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
    T.toStrict $
    T.bsingleton '"' <> quote s <> T.bsingleton '"'
  where
    quote q = case T.uncons t of
      Nothing       -> T.toBuilder h
      Just (!c, t') -> T.toBuilder h <> escape c <> quote t'
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
          "\\u" <> T.left 4 '0' (T.hex (fromEnum c))
      | otherwise =
          T.bsingleton c

newtype JQuerySelector = JQuerySelector Text
  deriving (ToJS, T.Buildable)

selectId :: Text -> JQuerySelector
selectId x = JQuerySelector $ T.format "#{}" [x]

selectUid :: Uid Node -> JQuerySelector
selectUid x = JQuerySelector $ T.format "#{}" [x]

selectClass :: Text -> JQuerySelector
selectClass x = JQuerySelector $ T.format ".{}" [x]

selectParent :: JQuerySelector -> JQuerySelector
selectParent x = JQuerySelector $ T.format ":has(> {})" [x]

selectChildren :: JQuerySelector -> JQuerySelector -> JQuerySelector
selectChildren a b = JQuerySelector $ T.format "{} > {}" (a, b)
