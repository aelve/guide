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
   .done(function(catId) {
     var newHeader = document.createElement("h2");
     newHeader.appendChild(document.createTextNode(s));

     var newList = document.createElement("ul");

     var newButton = document.createElement("button");
     newButton.appendChild(document.createTextNode("add item"));
     newButton.onclick = function() {addItem(catId, "new item");};

     var newDiv = document.createElement("div");
     newDiv.id = "cat" + catId;
     newDiv.appendChild(newHeader);
     newDiv.appendChild(newList);
     newDiv.appendChild(newButton);

     $("#categories").append(newDiv);
     });
  }
