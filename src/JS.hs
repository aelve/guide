{-# LANGUAGE
FlexibleInstances,
GeneralizedNewtypeDeriving,
OverloadedStrings,
QuasiQuotes,
NoImplicitPrelude
  #-}


-- TODO: try to make it more type-safe somehow?
module JS where


-- General
import BasePrelude
-- Text
import qualified Data.Text as T
import Data.Text (Text)
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
  -- Help
  showOrHideHelp, showHelp, hideHelp,
  -- Search
  search,
  -- Add methods
  addLibrary, addCategory,
  addPro, addCon,
  -- Render-as-editable methods
  setCategoryTitleMode, setCategoryNotesMode,
  setItemInfoMode, setItemTraitsMode,
  setTraitMode,
  -- Set methods
  submitCategoryTitle, submitCategoryNotes,
  submitTrait,
  submitItemInfo,
  -- Other things
  moveTraitUp, moveTraitDown, deleteTrait,
  moveItemUp, moveItemDown, deleteItem ]

-- | A class for things that can be converted to Javascript syntax.
class ToJS a where toJS :: a -> JS

instance ToJS Bool where
  toJS True  = JS "true"
  toJS False = JS "false"
instance ToJS JS where
  toJS = id
instance ToJS Text where
  toJS = JS . tshow
instance ToJS Integer where
  toJS = JS . tshow
instance ToJS Int where
  toJS = JS . tshow

-- | A helper class for calling Javascript functions.
class JSParams a where
  jsParams :: a -> [JS]

instance JSParams () where
  jsParams () = []
instance ToJS a => JSParams [a] where
  jsParams = map toJS
instance (ToJS a, ToJS b) => JSParams (a,b) where
  jsParams (a,b) = [toJS a, toJS b]
instance (ToJS a, ToJS b, ToJS c) => JSParams (a,b,c) where
  jsParams (a,b,c) = [toJS a, toJS b, toJS c]
instance (ToJS a, ToJS b, ToJS c, ToJS d) => JSParams (a,b,c,d) where
  jsParams (a,b,c,d) = [toJS a, toJS b, toJS c, toJS d]

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

showHelp :: JSFunction a => a
showHelp =
  makeJSFunction "showHelp" ["node", "version"]
  [text|
    localStorage.removeItem("help-hidden-"+version);
    $.get("/render/help", {mode: "shown"})
     .done(replaceWithData(node));
  |]

hideHelp :: JSFunction a => a
hideHelp =
  makeJSFunction "hideHelp" ["node", "version"]
  [text|
    localStorage.setItem("help-hidden-"+version, "");
    $.get("/render/help", {mode: "hidden"})
     .done(replaceWithData(node));
  |]

showOrHideHelp :: JSFunction a => a
showOrHideHelp =
  makeJSFunction "showOrHideHelp" ["node", "version"]
  [text|
    if (localStorage.getItem("help-hidden-"+version) === null)
      $.get("/render/help", {mode: "shown"})
       .done(replaceWithData(node))
    else
      $.get("/render/help", {mode: "hidden"})
       .done(replaceWithData(node));
  |]

search :: JSFunction a => a
search =
  makeJSFunction "search" ["node", "s"]
  -- TODO: set address bar to “/?query=...” so that the user would be able to
  -- see/share the search URL
  [text|
    $.post("/search", {query: s})
     .done(replaceWithData(node));
  |]

-- | Create a new category.
addCategory :: JSFunction a => a
addCategory =
  makeJSFunction "addCategory" ["node", "s"]
  [text|
    $.post("/add/category", {content: s})
     .done(prependData(node));
  |]

-- | Add a new library to some category.
addLibrary :: JSFunction a => a
addLibrary =
  makeJSFunction "addLibrary" ["node", "catId", "s"]
  [text|
    $.post("/add/category/"+catId+"/library", {name: s})
     .done(appendData(node));
  |]

setCategoryTitleMode :: JSFunction a => a
setCategoryTitleMode =
  makeJSFunction "setCategoryTitleMode" ["node", "catId", "mode"]
  [text|
    $.get("/render/category/"+catId+"/title", {mode: mode})
     .done(replaceWithData(node));
  |]

{- |
Finish category title editing (this happens when you submit the field).

This turns the title with the editbox back into a simple text title.
-}
submitCategoryTitle :: JSFunction a => a
submitCategoryTitle =
  makeJSFunction "submitCategoryTitle" ["node", "catId", "s"]
  [text|
    $.post("/set/category/"+catId+"/title", {content: s})
     .done(replaceWithData(node));
  |]

setCategoryNotesMode :: JSFunction a => a
setCategoryNotesMode =
  makeJSFunction "setCategoryNotesMode" ["node", "catId", "mode"]
  [text|
    $.get("/render/category/"+catId+"/notes", {mode: mode})
     .done(replaceWithData(node));
  |]

submitCategoryNotes :: JSFunction a => a
submitCategoryNotes =
  makeJSFunction "submitCategoryNotes" ["node", "catId", "s"]
  [text|
    $.post("/set/category/"+catId+"/notes", {content: s})
     .done(replaceWithData(node));
  |]

-- | Add a pro to some item.
addPro :: JSFunction a => a
addPro =
  makeJSFunction "addPro" ["node", "itemId", "s"]
  [text|
    $.post("/add/item/"+itemId+"/pro", {content: s})
     .done(appendData(node));
  |]

-- | Add a con to some item.
addCon :: JSFunction a => a
addCon =
  makeJSFunction "addCon" ["node", "itemId", "s"]
  [text|
    $.post("/add/item/"+itemId+"/con", {content: s})
     .done(appendData(node));
  |]

setItemInfoMode :: JSFunction a => a
setItemInfoMode =
  makeJSFunction "setItemInfoMode" ["node", "itemId", "mode"]
  [text|
    $.get("/render/item/"+itemId+"/info", {mode: mode})
     .done(replaceWithData(node));
  |]

setItemTraitsMode :: JSFunction a => a
setItemTraitsMode =
  makeJSFunction "setItemTraitsMode" ["node", "itemId", "mode"]
  [text|
    $.get("/render/item/"+itemId+"/traits", {mode: mode})
     .done(replaceWithData(node));
  |]

setTraitMode :: JSFunction a => a
setTraitMode =
  makeJSFunction "setTraitMode" ["node", "itemId", "traitId", "mode"]
  [text|
    $.get("/render/item/"+itemId+"/trait/"+traitId, {mode: mode})
     .done(replaceWithData(node));
  |]

submitTrait :: JSFunction a => a
submitTrait =
  makeJSFunction "submitTrait" ["node", "itemId", "traitId", "s"]
  [text|
    $.post("/set/item/"+itemId+"/trait/"+traitId, {content: s})
     .done(replaceWithData(node));
  |]

submitItemInfo :: JSFunction a => a
submitItemInfo =
  makeJSFunction "submitItemInfo" ["infoNode", "traitsNode", "itemId", "form"]
  [text|
    // If the group was changed, we need to recolor the whole item,
    // but we don't want to rerender the item on the server because
    // it would lose the item's state (e.g. what if the traits were
    // being edited? etc). So, instead we query colors from the server
    // and change the color of the “traits” div manually.
    $.post("/set/item/"+itemId+"/info", $(form).serialize())
     .done(function (data) {
        // Note the order – first we change the color, then we replace
        // the info node. The reason is that otherwise the traitsNode
        // selector might become invalid (if it depends on the infoNode
        // selector).
        $.get("/render/item/"+itemId+"/colors")
         .done(function (colors) {
            $(traitsNode).css("background-color", colors.light);
            replaceWithData(infoNode)(data);
         });
     });
  |]

moveTraitUp :: JSFunction a => a
moveTraitUp =
  makeJSFunction "moveTraitUp" ["itemId", "traitId", "traitNode"]
  [text|
    $.post("/move/item/"+itemId+"/trait/"+traitId, {direction: "up"});
    moveNodeUp(traitNode);
  |]

moveTraitDown :: JSFunction a => a
moveTraitDown =
  makeJSFunction "moveTraitDown" ["itemId", "traitId", "traitNode"]
  [text|
    $.post("/move/item/"+itemId+"/trait/"+traitId, {direction: "down"});
    moveNodeDown(traitNode);
  |]

deleteTrait :: JSFunction a => a
deleteTrait =
  makeJSFunction "deleteTrait" ["itemId", "traitId", "traitNode", "traitText"]
  [text|
    if (confirm("Confirm deletion: “"+traitText+"”")) {
      $.post("/delete/item/"+itemId+"/trait/"+traitId);
      $(traitNode).remove();
    }
  |]

moveItemUp :: JSFunction a => a
moveItemUp =
  makeJSFunction "moveItemUp" ["itemId", "itemNode"]
  [text|
    $.post("/move/item/"+itemId, {direction: "up"});
    moveNodeUp(itemNode);
  |]

moveItemDown :: JSFunction a => a
moveItemDown =
  makeJSFunction "moveItemDown" ["itemId", "itemNode"]
  [text|
    $.post("/move/item/"+itemId, {direction: "down"});
    moveNodeDown(itemNode);
  |]

deleteItem :: JSFunction a => a
deleteItem =
  makeJSFunction "deleteItem" ["itemId", "itemNode", "itemText"]
  [text|
    if (confirm("Confirm deletion: “"+itemText+"”")) {
      $.post("/delete/item/"+itemId);
      $(itemNode).remove();
    }
  |]

-- When adding a function, don't forget to add it to 'allJSFunctions'!
