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

// start category heading editing (this happens when you click on “[edit]”)
// 
// this turns the heading into an editbox, and adds a [cancel] link
function startCategoryHeadingEditing(catId) {
  $.get("/edit/category/"+catId+"/title/edit")
   .done(function(data) {
     setCategoryHeadingHtml(catId, data);
     });
  }

// finish category heading editing (this happens when you submit the field)
// 
// this turns the heading with the editbox back into a simple text heading
function finishCategoryHeadingEditing(catId, s) {
  $.post("/edit/category/"+catId+"/title", {title: s})
   .done(function(data) {
     setCategoryHeadingHtml(catId, data);
     });
  }

// cancel category heading editing
// 
// this turns the heading with the editbox back into a simple text heading
function cancelCategoryHeadingEditing(catId) {
  $.get("/edit/category/"+catId+"/title/cancel")
   .done(function(data) {
     setCategoryHeadingHtml(catId, data);
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
  $("#item"+itemId).replaceWith(data);
  }

// reload a category heading
function setCategoryHeadingHtml(catId, data) {
  $("#cat"+catId+" > h2").replaceWith(data);
  }
