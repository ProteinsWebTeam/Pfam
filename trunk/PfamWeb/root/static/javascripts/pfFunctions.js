
// pfFunctions.js
// jt6 20060412 WTSI
//
// javascript glue for the site. Requires the prototype library.
//
// $Id: pfFunctions.js,v 1.5 2006-05-26 15:44:57 jt6 Exp $

//------------------------------------------------------------
// show the specified tab in the page body

function show( id ) {

  // show/hide the blocks themselves
  $$("#content div.block").each( function( block ) {
								   if( id == block.id ) {
									 block.style.display = "block";
								   } else {
									 Element.hide( block );
								   }
								 } );

  // set the appropriate selector in the sidebar
  $$("#sidebar li").each( function( item ) {
							if( id+"Selector" == item.id ) {
							  Element.addClassName( item, "selected" );
							} else {
							  Element.removeClassName( item, "selected" );
							}
						  } );
}

//------------------------------------------------------------
// show/hide the DAS sources selection panel

showSources = true;
function revealDASSources() {
  if( showSources ) {
	Effect.BlindDown( "checkboxes", { duration: 0.3 } );
	showSources = false;
	Element.update( $("revealControl"), "Hide" );
  } else {
	Effect.BlindUp("checkboxes", { duration: 0.5 } );
	showSources = true;
	Element.update( $("revealControl"), "Show" );
  }
}

//------------------------------------------------------------
// callbacks for the domain graphics generation call

function dgSuccess( oResponse ) {
  Element.update( $("dgph").parentNode, oResponse.responseText );
}
function dgFailure() {
  Element.update( $("dgph"), "Domain graphics loading failed." );
}

//------------------------------------------------------------
// callbacks for the species tree generation call

function stSuccess( oResponse ) {
  var tree = new YAHOO.widget.TreeView("treeDiv");
  var root = tree.getRoot();
  eval( oResponse.responseText );
  tree.draw();
}
function stFailure() {
  Element.update( $("stph"), "Tree loading failed." );
}

//------------------------------------------------------------
// callbacks for the alignment tree generation call

function atSuccess( oResponse ) {
  Element.update( $("alignmentTree"), oResponse.responseText );
}
function atFailure() {
  var p = $("atph");

  // if a previous update succeeds, the "atph" should have
  // disappeared. We need to re-create it before trying to update it
  // with an error message...
  if( ! p ) {
	p = document.createElement( "p" );
	p.id = "atph";
	var parent = $("alignmentTree");
	parent.insertBefore( p, parent.firstChild );
  }
  Element.update( $("atph"), "Alignment tree loading failed." );
}

//------------------------------------------------------------
// callbacks for the alignment/DAS graphics in the protein section

function pgSuccess( oResponse ) {
  Element.update( $("graphicsHolder"), oResponse.responseText );
}
function pgFailure() {
  Element.update( $("pgph"), "Alignment loading failed." );
}

//------------------------------------------------------------
// code snippets in individual blocks will populate this object

var loadOptions = {};
loadOptions.dg = {}; // domain graphics
loadOptions.st = {}; // species tree
loadOptions.at = {}; // alignment tree
loadOptions.pg = {}; // protein graphics

//------------------------------------------------------------
// this will make the ajax calls for the family page components

function familyPostLoad() {
  new Ajax.Request( loadOptions.dg.uri,
					{ method:     "get", 
					  parameters: loadOptions.dg.params,
					  onComplete: dgSuccess,
					  onFailure:  dgFailure
					} );
  new Ajax.Request( loadOptions.st.uri,
	                { method:     "get", 
					  parameters: loadOptions.st.params,
                      onComplete: stSuccess,
                      onFailure:  stFailure
					} );
  new Ajax.Request( loadOptions.at.uri,
					{ method:     "get", 
					  parameters: loadOptions.at.params,
					  onComplete: atSuccess,
					  onFailure:  atFailure
					} );
}

//------------------------------------------------------------
// load ajax components for the protein page

function proteinPostLoad() {
   new Ajax.Request( loadOptions.pg.uri,
 					{ method: "get",
 						parameters: loadOptions.pg.params,
 						onComplete: pgSuccess,
 						onFailure:  pgFailure
 					} );
}

//------------------------------------------------------------
// these two functions from http://www.quirksmode.org/

function findPosX(obj) {
	var curleft = 0;
	if (obj.offsetParent) {
		while (obj.offsetParent) {
			curleft += obj.offsetLeft
			obj = obj.offsetParent;
		}
	} else if (obj.x) {
		curleft += obj.x;
  }
	return curleft;
}

function findPosY(obj) {
	var curtop = 0;
	if (obj.offsetParent) {
		while (obj.offsetParent) {
			curtop += obj.offsetTop
			obj = obj.offsetParent;
		}
	} else if (obj.y) {
		curtop += obj.y;
  }
	return curtop;
}

//------------------------------------------------------------
// log messages to the FireBug console

function printfire() {
  if( document.createEvent ) {
	printfire.args = arguments;
	var ev = document.createEvent( "Events" );
	ev.initEvent( "printfire", false, true );
	dispatchEvent( ev );
  }
}

//------------------------------------------------------------
// highlight an "area" in an image map by overlaying a coloured div

function highlight( e ) {
  var target;
  if( e.target ) {
    target = e.target;
  } else { 
    target = e.srcElement;
  }
  // work around the Safari bug that causes a text node to be the target
  if( target.nodeType == 3 ) target = target.parentNode;

  var coords = target.coords.split(",");
  var hs = $("highlight").style;

  hs.width = coords[2] - coords[0];
  hs.height = coords[3] - coords[1];

  var mapX = findPosX( $("featuresMap") );
  var mapY = findPosY( $("featuresMap") );	

  hs.left = Number( coords[0] ) + Number( mapX );
  hs.top  = Number( coords[1] ) + Number( mapY );

  printfire( "showing tip: " + target.id + "Tip" );
  domTT_activate( $("highlight"), e, "predefined", target.id + "Tip" );

  $("highlight").style.display = "block";
}

// and hide the div on mouseout
function unhighlight( e ) {
  domTT_mouseout( $("highlight"), e );
  $("highlight").style.display = "none";
}

//------------------------------------------------------------
// move a thin line across the image maps, by way of a cursor

function moveCursor( e ) {
  var cObj = $("cursor");
  var fObj = $("featuresMap");
  // set the cursor height to the height of the map
  cObj.style.height = Element.getHeight( fObj ) - 1 + "px";

  // get the positions of the various blocks
  var co = Position.cumulativeOffset( fObj );
  var px = Event.pointerX( e );
  var ol = fObj.offsetLeft;

  var cx = px - co[0] + ol - 1;

  var images = $A( $("featuresMap").getElementsByTagName("img") );

  var im = images[0];
  var minX = im.offsetLeft;
  var maxX = im.offsetLeft + Element.getDimensions( im ).width;

  x = cx;
  if( x < minX ) x = minX;
  if( x > maxX ) x = maxX;

  cObj.style.left = x + "px";

  var r = x - im.offsetLeft;
  // update the status display
  $("status").innerHTML = "residue: " + r;

  cObj.style.display = "block";
}
