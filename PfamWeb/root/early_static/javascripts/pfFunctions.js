
// pfFunctions.js
// jt6 20060412 WTSI
//
// javascript glue for the site. Requires the prototype library.
//
// $Id: pfFunctions.js,v 1.6 2006-05-31 16:50:53 jt6 Exp $

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
  installGizmos();
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

  // this is the <map> that contains this <area>
  var mapName = target.parentNode.name;

  console.debug( "target:   |" + target.id + "|" );
  console.debug( "mapName:  |" + mapName + "|" );

  // find the ID of the <img> that uses this <map>
  var regex = /^featuresMap(\d+)$/;
  var results = regex.exec( mapName );
  console.debug( "number: |" + results[1] + "|" );
  var image = $("featuresImage" + results[1]);
  console.debug( "image: |" + image.id + "|" );

  // show the tooltip
  //domTT_activate( target, e, "predefined", target.id + "Tip" );
  //domTT_activate( $("highlight"), e, "predefined", target.id + "Tip" );

  // place the highlight
  var coords = target.coords.split(",");
  var width  = coords[2] - coords[0];
  var height = coords[3] - coords[1];

  var left = findPosX( image ) 
	       + Number( coords[0] )
	       - findPosX( $("featuresMap") ) 
	       + $("featuresMap").offsetLeft;

  var top  = findPosY( image )
           + Number( coords[1] )
           - findPosY( $("featuresMap") )
           + $("featuresMap").offsetTop;

  console.debug( "WxH+X,Y:  " + width + "x" + height + "+" + left + "," + top );

  Element.setStyle( $("highlight"),
					{
					  "width":   width + "px",
					  "height":  height + "px",
					  "left":    left + "px",
					  "top":     top + "px",
					  "display": "block"
					}
				  );
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

  var x = px - co[0] + ol - 1;

  var im   = $A( $("featuresMap").getElementsByTagName("img") ).first();
  var minX = im.offsetLeft;
  var maxX = im.offsetLeft + Element.getDimensions( im ).width;

  if( x < minX ) x = minX;
  if( x > maxX ) x = maxX;

  cObj.style.left = x + "px";

  // update the status display
  var r = x - im.offsetLeft;
  $("status").innerHTML = "residue: " + r;

  cObj.style.display = "block";
}
