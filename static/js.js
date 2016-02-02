// add a new item to a category
function addItem(catId, s) {
  $.post("/add/item/" + catId, {item: s})
   .done(function() {
     var list = $("#cat"+catId+">ul");

     var newItem = document.createElement("li");
     newItem.appendChild(document.createTextNode(s));
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
