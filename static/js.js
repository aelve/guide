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
