
// pfFunctions.js
// jt6 20060412 WTSI
//
// $Id: pfFunctions.js,v 1.1 2006-04-12 16:20:57 jt6 Exp $

// show the specified tab in the page body
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

// set up the callbacks for the domain graphics loading
var graphicsResponseSuccess = function( oResponse ) {
  var divs = YAHOO.util.Dom.get( "domainsBlock" ).getElementsByTagName( "div" );
  var parent;
  for( var i = 0; i< divs.length; i++ ) {
    if( ( ' ' + divs[i].className + ' ' ).indexOf("blockContent") != -1 ) {
      parent = divs[i];
      break;
    }
  }
  parent.innerHTML = oResponse.responseText;
}

var graphicsResponseFailure = function( oResponse ) {
  var message = "<p>Loading failed.</p>";
  var divs = YAHOO.util.Dom.get( "domainsBlock" ).getElementsByTagName( "div" );
  var parent;
  for( var i = 0; i < divs.length; i++ ) {
    if( ( ' ' + divs[i].className + ' ' ).indexOf("blockContent") != -1 ) {
      parent = divs[i];
      break;
    }
  }
  parent.innerHTML = message;
}

var graphicsCallback = { success:graphicsResponseSuccess,
                         failure:graphicsResponseFailure };

// and for the species tree load
var treeResponseSuccess = function( oResponse ) {
  var tree = new YAHOO.widget.TreeView("treeDiv");
  var root = tree.getRoot();
  eval( oResponse.responseText );
  tree.draw();
}

var treeResponseFailure = function( oResponse ) {
  var treeDiv = YAHOO.util.Dom.get( "treeDiv" );
  treeDiv.innerHTML = "Failed to load species tree.";
}

var treeCallback = { success:treeResponseSuccess,
                     failure:treeResponseFailure };
