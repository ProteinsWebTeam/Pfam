
// blockConfig.js
// jt6 20060303 WTSI
//
// set up collapsing blocks
//
// $Id: blockConfig.js,v 1.1 2006-03-20 14:44:40 jt6 Exp $

// initialise everything
function initialiseBlocks() {
  var divs = YAHOO.util.Dom.get( "content" ).getElementsByTagName( "div" );
  for( var i in divs ) {
	var div = divs[i];
	if( ( ' ' + div.className + ' ' ).indexOf("block") != -1 ) {
	  new Block( divs[i].id );
	}
  }
}

// initialise blocks on window load
YAHOO.util.Event.addListener( window, "load", initialiseBlocks );
