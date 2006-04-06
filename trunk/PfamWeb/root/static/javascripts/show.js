
function show( id ) {
  var divs = YAHOO.util.Dom.get( "content" ).getElementsByTagName( "div" );
  for( var i = 0; i < divs.length; i++ ) {
    var div = divs[i];
    var style = div.style;
	if( div.className == "block" ) {
      if( id == div.id ) {
        style.display = "block";
      } else {
        style.display = "none";
      }
    }
  }

  var lis = YAHOO.util.Dom.get( "sidebar" ).getElementsByTagName( "li" );
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
