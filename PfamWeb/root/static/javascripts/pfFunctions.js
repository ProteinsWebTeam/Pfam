
// pfFunctions.js
// jt6 20060412 WTSI
//
// javascript glue for the site. Requires the prototype library.
//
// $Id: pfFunctions.js,v 1.10 2006-07-14 13:06:20 jt6 Exp $

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
// show/hide the specified drop-down panel in the protein 
// section

showItems = {};
function reveal( oSwitch, sId ) {
  // // console.debug( "showing:        |" + sId + "|" );
  // // console.debug( "showItems[sId]: |" + showItems[sId] + "|" );
  var oSource = $(sId);
  if( showItems[sId] ) {
	// console.debug( sId + " is currently shown" );
	Effect.BlindUp( oSource, { duration: 0.3 } );
	showItems[sId] = false;
	Element.update( oSwitch, "Show" );
  } else {
	// console.debug( sId + " is currently hidden" );
	Effect.BlindDown( oSource, { duration: 0.3 } );
	showItems[sId] = true;
	Element.update( oSwitch, "Hide" );
  }
}

//------------------------------------------------------------
// callbacks for the domain graphics generation call in the 
// family section

function dgSuccess( oResponse ) {
  Element.update( $("dgph").parentNode, oResponse.responseText );
}
function dgFailure() {
  Element.update( $("dgph"), "Domain graphics loading failed." );
}

//------------------------------------------------------------
// callbacks for the species tree generation call in the 
// family section

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
// callbacks for the alignment tree generation call in the 
// family section

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
// callbacks for the coloured alignment in the family section

function caSuccess( oResponse ) {
  Element.update( $("caph"), oResponse.responseText );
}

function caFailure() {
  Element.update( $("caph"), "Alignment loading failed." );
}

//------------------------------------------------------------
// code snippets in individual blocks will populate this object

var loadOptions = {};
loadOptions.dg = {}; // domain graphics
loadOptions.st = {}; // species tree
loadOptions.at = {}; // alignment tree
loadOptions.pg = {}; // protein graphics
loadOptions.ca = {}; // coloured alignment
loadOptions.sg = {}; // sequence graphics

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
  new Ajax.Request( loadOptions.ca.uri,
					{ method:     "get", 
					  parameters: loadOptions.ca.params,
					  onComplete: caSuccess,
					  onFailure:  caFailure
					} );
}

//------------------------------------------------------------
// callbacks for the alignment/DAS graphics in the protein section

// called when the browser first fires the request
function pgStarted( oResponse ) {
  $("pgSubmitButton").disabled = true;
  $("pgUpdateSpinner").style.visibility = "visible";
}

// called when the request completes
function pgCompleted() {
  $("pgSubmitButton").disabled = false;
  $("pgUpdateSpinner").style.visibility = "hidden";
}

// called in response to a successful call
function pgSuccess( oResponse ) {
  Element.update( $("graphicsHolder"), oResponse.responseText );
  $("pgSubmitButton").disabled = false;
}

// called in response to a failed call
function pgFailure() {
  Element.update( $("pgph"), "Alignment loading failed." );
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
  var r = x - im.offsetLeft + 1;
  $("status").innerHTML = "Residue number: " + r;

  cObj.style.display = "block";
}

//------------------------------------------------------------
// an object that takes care of mouseover highlighting of the 
// structure mapping table.

// store the currently highlighted cells in a global
var highlightedCells = new Array();

// define the object
var highlight = new Object();

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
  
// handle mouseout events on the cells. Walk down the list of
// currently highlighted cells and remove the hover classname
  
highlight.mouseoutHandler = function( e ) {
  highlightedCells.each( function( cell ) {
						   Element.removeClassName( cell, "stripeHover" );
						 }
					   );
  
  // reset the array
  highlightedCells.clear();
}
  
//------------------------------------------------------------
// function to submit the alignment generation form  

function generateAlignment( type, start, end ) {

  // are we rendering a specified range or the previous/next block ?
  var range;
  if( "pager" == type ) {
	range = start + "-" + end;
  } else {
	range = $F("startSeq")+"-"+$F("endSeq");
  }

  // stuff that value into the form...
  $("rowRange").value = range;

  // store the scroll value for the alignment, so we can use it in the
  // new page
  $("scrollValue").value = $("alignmentData").scrollLeft;

  // submit the form
  new Ajax.Updater( "caph",
                    loadOptions.ca.uri, 
                    {   parameters:   Form.serialize( $("pagingForm") ),
						asynchronous: 1,
						evalScripts:  true
					}
                  );

  return false;
}




//------------------------------------------------------------
// load ajax components for the structure page

function structurePostLoad() {
   new Ajax.Request( loadOptions.sg.uri,
 					{ method: "get",
 					  parameters: loadOptions.sg.params,
 					  onComplete: sgSuccess,
 					  onFailure:  sgFailure
 					} );
}

// called in response to a successful call
function sgSuccess( oResponse ) {
  Element.update( $("structureGraphicsHolder"), oResponse.responseText );
}

// called in response to a failed call
function sgFailure() {
  Element.update( $("sgph"), "Graphics loading failed." );
}

