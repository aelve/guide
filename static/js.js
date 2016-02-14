// add a new library
function addLibrary(catId, s) {
  $.post("/add/item/library/" + catId, {name: s})
   .done(function(data) {
     $("#cat"+catId+" > .items").append(data);
     });
  }

// create a new category
function addCategory(s) {
  $.post("/add/category", {title: s})
   .done(function(data) {
     $("#categories").append(data);
     });
  }

// add pros to some item
function addPros(itemId, s) {
  $.post("/add/pros/" + itemId, {content: s})
   .done(function(data) {
     setItemHtml(itemId, data);
     });
  }

// add cons to some item
function addCons(itemId, s) {
  $.post("/add/cons/" + itemId, {content: s})
   .done(function(data) {
     setItemHtml(itemId, data);
     });
  }

// reload an item
function setItemHtml(itemId, data) {
  $("#item"+itemId).replaceWith(data) }
