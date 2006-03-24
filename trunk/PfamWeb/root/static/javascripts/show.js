
function show( id ) {
  var divs = document.getElementsByTagName( "div" );
  for( var i = 0; i < divs.length; i++ ) {
    var div = divs[i];
    var style = div.style;
    if( "block" == div.className ) {
      if( id + "Block" == div.id ) {
        style.display = "block";
      } else {
        style.display = "none";
      }
    }
  }

  var lis = document.getElementsByTagName( "li" );
  for( var i = 0; i < lis.length; i++ ) {
    var li = lis[i];
    var label = id + "Selector";
    if( li.id == label ) {
      li.className = "selected";
    } else {
      li.className = "";
    }
  }

}
