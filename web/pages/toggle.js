document.toggleDisplay = function(id1, id2) {
  var id1 = 'show';
  var id2 = 'edit';
  var isDisplayed = function(id) {
    return document.getElementById(id).style.display != 'none';
  }
  console.log(document.getElementById(id1));
  console.log(document.getElementById(id2));
  if (isDisplayed(id1)) {
    document.getElementById(id1).style.display = 'none';
    document.getElementById(id2).style.display = 'inherit';
  } else {
    document.getElementById(id1).style.display = 'inherit';
    document.getElementById(id2).style.display = 'none';
  }
};

document.getElementById('edit-btn').onclick = document.toggleDisplay;
