// add a new library
function addLibrary(catId, s) {
  $.post("/add/item/library/" + catId, {name: s})
   .done(function(data) {
     var list = $("#cat"+catId+">ul");

     var newItem = document.createElement("li");
     $(newItem).append(data);
     list.append(newItem);
     });
  }

// create a new category
function addCategory(s) {
  $.post("/add/category", {title: s})
   .done(function(data) {
     $("#categories").append(data);
     });
  }
