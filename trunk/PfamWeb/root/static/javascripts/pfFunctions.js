
// pfFunctions.js
// jt6 20060412 WTSI
//
// javascript glue for the site. Requires the prototype library.
//
// $Id: pfFunctions.js,v 1.12 2006-07-21 14:45:51 jt6 Exp $

//------------------------------------------------------------
// code snippets in individual blocks will populate this object

var loadOptions = {};
loadOptions.dg = {}; // domain graphics
loadOptions.st = {}; // species tree
loadOptions.at = {}; // alignment tree
loadOptions.pg = {}; // protein graphics
loadOptions.ca = {}; // coloured alignment
loadOptions.sg = {}; // sequence graphics
loadOptions.cg = {}; // clan graphics

//------------------------------------------------------------
//- objects --------------------------------------------------
//------------------------------------------------------------

// an object that takes care of mouseover highlighting of the 
// structure mapping table. This is in the core pfFunctions 
// file because it's used in a couple of places

// store the currently highlighted cells in a global
var highlightedCells = new Array();

// define the object
var highlight = new Object();

//----------------------------------------

// handle mouseovers - find the cells of the table that need to
// be highlighted and set the appropriate CSS class

highlight.mouseoverHandler = function( e ) {

  // get hold of the starting row, the one with the highlighted cell
  var startingRow;
  if( e.srcElement ) {
	// get it the IE way...
	startingRow = e.srcElement.parentNode;
  } else if( e.target ) {
	// and for the rest of the world...
	startingRow = e.target.parentNode;
  }

  // these are the cells that we'll need to colour
  var cells = new Array();

  // first, stash the cells in the starting row
  var startingCells = $A( startingRow.getElementsByTagName( "td" ) );
  cells.push( startingCells );
  // console.debug( "cells starts with " + cells.length + " cells" );

  // and then, if this row isn't the full width of the table, recurse down
  // (actually, up) the previous rows and collect more cells to highlight
  if( startingCells.length < numColsTable ) {
	this.walkRows( startingRow, cells );
  }
  // console.debug( "retrieved " + cells.flatten().length + " cells" );

  // highlight the collected cells
  cells.flatten().each( function( cell ) {
						  Element.addClassName( cell, "stripeHover" );
						  highlightedCells.push( cell );
						}
					  );
  
};

//----------------------------------------

// recursive method for walking back up the table rows and finding
// those that require cells to be highlighted. Because the table can
// (and likely does) contain rowspan'd cells, it's not as simple as
// just colouring a complete <tr>, unfortunately...

highlight.walkRows = function( startingRow, cells ) {
  // console.debug( "in walkRows" );

  // catch the empty events that come from, I think, table contents
  if( null == startingRow ) {
	return;
  }

  // find out how many columns are in the starting row
  var numColsStartingRow = startingRow.getElementsByTagName( "td" ).length;
  // console.debug( "startingRow has " + numColsStartingRow + " columns" );

  // get all of the previous rows in the table
  var thisRow = startingRow;
  var prevRow;
  var prevCols;
  var numCols;
  while( prevRow = thisRow.previousSibling ) {
	if( prevRow.nodeType == 1 ) {
	  // console.debug( "checking row " + prevRow.rowIndex );
	  
	  prevCols = prevRow.getElementsByTagName( "td" );
	  numCols = prevCols.length;
	  if( numCols == numColsTable ||
		  numCols >  numColsStartingRow ) {
		break;
	  }
	}
	thisRow = prevRow;
  }
  // console.debug( "previous longer row is row " + prevRow.rowIndex );
  
  // add the extra cells from the longer row into the array of cells to 
  // highlight and keep walking down (up) the table
  
  // there are "numCols" columns in the previous longest row
  // there are "numColsStartingRow" columns in the current row
  
  for( var i = 0; i < ( numCols - numColsStartingRow ); i++ ) {
	cells.push( prevCols[i] );
  }
  // console.debug( "adding " + cells.length + " cells" );
  
  // see if we need to move on to previous rows in the table
  if( numCols < numColsTable ) {
	this.walkRows( prevRow, cells );
  }
};
  
//----------------------------------------

// handle mouseout events on the cells. Walk down the list of
// currently highlighted cells and remove the hover classname
  
highlight.mouseoutHandler = function( e ) {
  highlightedCells.each( function( cell ) {
						   Element.removeClassName( cell, "stripeHover" );
						 }
					   );
  
  // reset the array
  highlightedCells.clear();
};
  
//------------------------------------------------------------
//- functions ------------------------------------------------
//------------------------------------------------------------

// show the selected tab
function chooseTab() {
  
  // see if the showTab variable points to tab that actually exists in
  // the page
  if( $(showTab) ) {
	// yes; show that tab
	show( showTab );
  } else {
	// no; get the first block in the page and show that instead
	var id = document.getElementsByClassName( "block" )[0].id;
	show( document.getElementsByClassName( "block" )[0].id );
  }
}

//------------------------------------------------------------

// display the specified tab in the page body

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
// calculate the position of the supplied object.
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

function highlightFeature( e ) {
  // console.debug( "entering highlight" )
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

  // console.debug( "target:   |" + target.id + "|" );
  // console.debug( "mapName:  |" + mapName + "|" );

  // find the ID of the <img> that uses this <map>
  var regex = /^featuresMap(\d+)$/;
  var results = regex.exec( mapName );
  // console.debug( "number: |" + results[1] + "|" );
  var image = $("featuresImage" + results[1]);
  // console.debug( "image: |" + image.id + "|" );

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

  // console.debug( "WxH+X,Y:  " + width + "x" + height + "+" + left + "," + top );

  Element.setStyle( $("highlight"),
					{
					  "width":   width + "px",
					  "height":  height + "px",
					  "left":    left + "px",
					  "top":     top + "px",
					  "display": "block"
					}
				  );
  // console.debug( "leaving highlight" )
}

//----------------------------------------

// and hide the div on mouseout
function unhighlight( e ) {
  domTT_mouseout( $("highlight"), e );
  $("highlight").style.display = "none";
}

//----------------------------------------

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
  var r = x - im.offsetLeft + 1;
  $("status").innerHTML = "Residue number: " + r;

  cObj.style.display = "block";
}

//------------------------------------------------------------
