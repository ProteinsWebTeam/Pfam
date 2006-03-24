
// blockConfig.js
// jt6 20060303 WTSI
//
// set up collapsing blocks
//
// $Id: blockConfig.js,v 1.2 2006-03-24 11:45:13 jt6 Exp $

// initialise everything
function initialiseBlocks() {
  var divs = YAHOO.util.Dom.get( "content" ).getElementsByTagName( "div" );
  for( var i = 0; i < divs.length; i++ ) {
	var div = divs[i];
	if( ( ' ' + div.className + ' ' ).indexOf("block") != -1 ) {
	  new Block( divs[i].id );
	}
  }
}

// initialise blocks on window load
YAHOO.util.Event.addListener( window, "load", initialiseBlocks );
