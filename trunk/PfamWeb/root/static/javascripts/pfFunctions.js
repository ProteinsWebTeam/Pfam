
// pfFunctions.js
// jt6 20060412 WTSI
//
// javascript glue for the site. Requires the prototype library.
//
// $Id: pfFunctions.js,v 1.39 2007-05-31 12:21:12 jt6 Exp $

// Copyright (c) 2007: Genome Research Ltd.
// 
// Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
// 
// This is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//  
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//  
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
// or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

//------------------------------------------------------------
// code snippets in individual blocks will populate this object

var loadOptions = {};
loadOptions.dg = {}; // domain graphics
loadOptions.si = {}; // structure image
loadOptions.st = {}; // species tree
loadOptions.at = {}; // alignment tree
loadOptions.pg = {}; // protein graphics
loadOptions.ca = {}; // coloured alignment
loadOptions.sg = {}; // sequence graphics
loadOptions.cg = {}; // clan graphics
loadOptions.cstruc = {}; // clan structure tab
loadOptions.fstruc = {}; // family structure tab
loadOptions.simap ={}; //simap graphics
loadOptions.getDomains = {}; //get structural domains from cath and scop

//------------------------------------------------------------
// name the original window, so that we can target links back to it
// from child-windows

window.name = "pfamParentWin";

//------------------------------------------------------------
//- objects --------------------------------------------------
//------------------------------------------------------------

// an object that takes care of mouseover highlighting of the 
// structure mapping table. This is in the core pfFunctions 
// file because it's used in a couple of places

// store the currently highlighted cells in a global
var highlightedCells = [];

// define the object
var highlight = {};

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

  // if the mouseover event originates at a link node within a table
  // cell, we need to get the parent of the parent of the link node
  if( "td" == startingRow.nodeName || "TD" == startingRow.nodeName ) {
    startingRow = startingRow.parentNode;
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
                          // console.debug( "added stripeHover for cell " + cell );
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

// switch between panels on the index page
function switchPanel( oTrigger, sId ) {
  
  // hide all of the panels
  $$("div.panel").each(
    function( panel ) {
      panel.hide();
    }
  );

  // show the selected panel. Note that we're not using Element.show() here, 
  // because we can't "show" an element that was hidden using CSS... see 
  // prototype docs
  $(sId).setStyle( { display: "block" } );

  // as a nicety, if there's a form in the panel, focus it
  $(sId).getElementsBySelector(".entryField").each(
    function( field ){
      field.focus();
    }
  );

  // switch the style of the trigger link  
  $$("#controlPanel li").each(
    function( link ) {
      link.removeClassName( "currentLink" );
      link.addClassName( "link" );
    }
  );
  oTrigger.removeClassName( "link" );
  oTrigger.addClassName( "currentLink" );
}

//------------------------------------------------------------
// show/hide the specified drop-down panel

showItems = {};

function reveal( oSwitch, sId, bStartState, bQuick ) {

  // we'll assign it bStartState as the "visible state" of the element, but
  // only if that's not already assigned
  if( typeof( showItems[sId] ) == "undefined" ) {
  	showItems[sId] = bStartState;
  }
  
  var oSource = $(sId);
  if( showItems[sId] === undefined || showItems[sId] ) {

    // hide the element. If bQuick is true, we'll literally hide() it, 
    // otherwise we'll reveal it
  	if( bQuick ) {
      oSource.hide();
    } else {
      Effect.BlindUp( oSource, { duration: 0.3 } );
    }
  	showItems[sId] = false;
  } else {

    // show the element
  	if( bQuick ) {
      oSource.show();
    } else {
    	Effect.BlindDown( oSource, { duration: 0.3 } );
    }
  	showItems[sId] = true;
  }
  var newIH;
  if( oSwitch.innerHTML.include( "Show" ) ) {
    newIH = oSwitch.innerHTML.sub( "Show", "Hide" );
  } else if( oSwitch.innerHTML.include( "Hide" ) ) {
    newIH = oSwitch.innerHTML.sub( "Hide", "Show" );
  } else if( oSwitch.innerHTML.include( "More" ) ) {
    newIH = oSwitch.innerHTML.sub( "More", "Less" );
  } else if( oSwitch.innerHTML.include( "Less" ) ) {
    newIH = oSwitch.innerHTML.sub( "Less", "More" );
  }
  oSwitch.innerHTML = newIH;
}
  
//------------------------------------------------------------
// show the selected tab

function chooseTab() {
  // console.debug( "pfFunctions.js:chooseTab" );
  // see if the showTab variable points to tab that actually exists in
  // the page
  if( ! ( showTab === undefined ) && $(showTab) ) {
    // console.debug( "chooseTab: using param setting: " + showTab );

    // yes; show that tab
    show( showTab );

  } else {
    // console.debug( "chooseTab: no param setting; checking cookie" );
    
    // no; see if there's a cookie to tell us the last tab
    var cookieTab = readCookie( "lastTab" );
    // console.debug( "cookieTab: |" + cookieTab + "|" );

    if( cookieTab && $(cookieTab) ) {
      // console.debug( "chooseTab: found a cookie; switching to |" + cookieTab + "|"  );
      
      // yes; show that tab
      show( cookieTab );
    } else {
      // console.debug( "chooseTab: no cookie switching to default"  );

      // no; get the first block in the page and show that instead
      var block = document.getElementsByClassName( "block" )[0];
      if( block && block.id ) {
        show( block.id );
      }
    }
  }
}

//------------------------------------------------------------
// display the specified tab in the page body

function switchTab( sId ) {
  // show/hide the blocks themselves
  $$("#content div.block").each( function( block ) {
                                   if( sId == block.id ) {
                                     block.setStyle( { display: "block" } );
                                   } else {
                                     block.hide();
                                   }
                                 } );

  // set the appropriate selector in the sidebar
  $$("#sidebar li").each( function( item ) {
                            if( sId+"Selector" == item.id ) {
                              item.addClassName( "selected" );
                            } else {
                              item.removeClassName( "selected" );
                            }
                          } );
}

//------------------------------------------------------------
// highlight an "area" in an image map by overlaying a coloured div

// hide the feature highlight div before we start
if( $("highlight" ) ) {
  Element.hide( "highlight" );
}

// a re-usable tooltip
var ht;

// the URL from the highlighted area
var highlightURL = "";

function highlightFeature( e ) {
  // console.debug( "entering highlight" )

  var target;
  if( e.target ) {
    target = e.target;
  } else { 
    target = e.srcElement;
  }
  // work around the Safari bug that causes a text node to be the target
  if( target.nodeType == 3 ) {
    target = target.parentNode;
  }

  // this is the <map> that contains this <area>
  var mapName = target.parentNode.name;

  // console.debug( "target:   |" + target.id + "|" );
  // console.debug( "mapName:  |" + mapName + "|" );

  // find the ID of the <img> that uses this <map>
  var num = mapName.substring( 11 );
  var image = $("featuresImage" + num);

  // where to place the highlight...
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

  // position the div
  Element.setStyle( $("highlight"),
                    {
                      "width":   width + "px",
                      "height":  height + "px",
                      "left":    left + "px",
                      "top":     top + "px"
                    }
                  );

  // find the contents of the label for the row containing this feature
  var mapNum = target.parentNode.id.substring( 11 );
  var label = $("featuresLabel" + mapNum).innerHTML;
  // console.debug( "features label: |" + label + "|" );

  // set the tooltip contents
  ht.setBody( target.alt );

  // retrieve a URL from the feature, if present, and set various
  // other bits and pieces...
  if( target.href ) {
    highlightURL = target.href;
    Element.addClassName( "highlight", "linked" );
    ht.setHeader( label +" feature (click for details)" );
  } else {
    highlightURL = "";
    Element.removeClassName( "highlight", "linked" );
    ht.setHeader( label +" feature" );
  }

  // "render" the tooltip, to make sure that the header and footer are
  // added correctly
  ht.render();
  
  // and finally, display the highlight div  
  Element.show( "highlight" );

  // console.debug( "leaving highlight" )
}

//----------------------------------------
// open a new window with the URL from a given feature

function openHighlightURL( e ) {
  if( highlightURL != "" ) {
    window.open( highlightURL );
  }
}
//----------------------------------------

// hide the div on mouseout
function unhighlight( e ) {
  Element.hide( "highlight" );
}

//----------------------------------------

// move a thin line across the image maps, by way of a cursor

var cObj;       // the cursor div
var fObj;       // the "featuresMap" div, which contains all this stuff
var images;     // an array of all of the images in the features map div
var tl, br, co, po; // various coordinates
var minX, maxX; // limits for cursor movement

var cursorInitialised = false; // flag to show whether the cursor is ready

function initialiseCursor() {

  // get some handles on important elements
  cObj = $("cursor");
  fObj = $("featuresMap");
  images = $A( $("featuresMap").getElementsByTagName("img") ); 

  // calculate the various coordinates and offsets... 
  tl = Position.cumulativeOffset( images.first() );
  var bl = Position.cumulativeOffset( images.last()  );
  br = [ bl[0] + images.last().getWidth(),
         bl[1] + images.last().getHeight() ];

  co = Position.cumulativeOffset( fObj );
  po = Position.positionedOffset( fObj );

  minX = tl[0] - co[0] + po[0] + 1;
  maxX = br[0] - co[0] + po[0] - 2;

  var y = tl[1] - co[1] + po[1];
  var h = br[1] - tl[1];

  // style the cursor div  
  cObj.setStyle( { left:    minX + "px",
                   top:     y + "px",
                   width:   "1px",
                   height:  h + "px",
                   display: "block" } );

  cursorInitialised = true;
}

function moveCursor( e ) {
  // initialise the cursor the first time it's used
  if( ! cursorInitialised ) {
    initialiseCursor();
  }

  // the absolute position of the event on the page
  var px = Event.pointerX( e );
  
  // offset the cursor from the mouse pointer by one pixel, otherwise we
  // mask some essential mouse events
  var x = px - co[0] + po[0] - 1;

  // stay over the images
  if( x < minX ) { x = minX }
  if( x > maxX ) { x = maxX }

  // move it
  cObj.setStyle( { left: x + "px" } );

  // fix the cursor offset here before updating the status display
  var r = x - minX + 1;
  $("status").update( "Residue number: " + r );
}

//------------------------------------------------------------
// post-load all of the sequences with a given architecture

function loadDomains( sArch, iIndex, uri, iNum ) {

  // the message for the confirmation dialogue
  var msg = "You are about to load " + iNum + 
            " domain graphics, which may take some time.\n\nAre you sure you want to continue ?";

  // only ask for confirmation if there are 50 or more sequences to load
  var continueLoad = ( iNum >= 50 ) ? confirm( msg ) : true;

  if( continueLoad ) {
    ['adSpinner' + sArch + iIndex,
     'loadSwitch' + iIndex,
     'showHideArchs' + iIndex ].each( Element.toggle );

    // and actually fire off a request to load the new graphics
    new Ajax.Updater( 'domainArch' + iIndex, uri );
  }
}

//------------------------------------------------------------
// various functions used in the domain query tab

// loads the list of IDs, chosen from the "alphabet" at the top of the form
function chooseIds( sLetter ) {

  Element.show( "nlUpdateSpinner" );
  $( "domainSearchForm" ).disable();
  
  new Ajax.Updater( "idSelectionWrapper",
                    queryURI,
                    {
                      parameters: "list=1&browse=" + sLetter,
                      onComplete: function () {
                                    Element.hide("nlUpdateSpinner");
                                    $( "domainSearchForm" ).enable();
                                  }
                    } );
}

// adds an ID from the selection list to another list, specified by the
// argument
function addId(listId) {
  if( $("idSelection").hasChildNodes() ) {
    var selected = $("idSelection").selectedIndex;
    if( selected >= 0 ) {
      var chosen = $("idSelection").options[selected];
      $("idSelection").removeChild( chosen );
      $(listId).appendChild( chosen );            
    }
  }
}

// removes an ID from the specified list and drops it back into the
// selection list
function removeId(listId) {
  if( $(listId).hasChildNodes() ) {
    var selected = $(listId).selectedIndex;
    if( selected >= 0 ) {
      var chosen = $(listId).options[selected];
      $(listId).removeChild( chosen );
      $("idSelection").appendChild( chosen );
    }
  }            
}

// constructs a list of the chosen IDs and submits the form
function buildIdLists() {

  $A( $("have").options ).each( function( opt ) {
                                  var i = document.createElement( "input" );
                                  i.type  = "hidden";
                                  i.name  = "have";
                                  i.value = opt.value;
                                  $("domainSearchForm").appendChild( i );
                                } );
  
  $A( $("not").options ).each( function( opt ) {
                                  var i = document.createElement( "input" );
                                  i.type  = "hidden";
                                  i.name  = "not";
                                  i.value = opt.value;
                                  $("domainSearchForm").appendChild( i );
                               } );

  $('domainSearchForm').submit();
}

// run before submitting the search. Disables the submit button and builds the 
// list of IDs for the query
function dsStarted() {
  Element.show( "searchUpdateSpinner" );
  $( "domainSearchForm" ).disable();

    var i = document.createElement( "input" );
  i.type  = "hidden";
  i.name  = "have";
  i.value = $A( $("have").options ).inject( '', 
                                            function( pars, i ) { 
                                              pars = i.value + " " + pars;
                                              return pars;
                                            } );
  $("domainSearchForm").appendChild( i );

  var i = document.createElement( "input" );
  i.type  = "hidden";
  i.name  = "not";
  i.value = $A( $("not").options ).inject( '',
                                           function( pars, i ) {
                                             pars = i.value + " " + pars;
                                             return pars;
                                           } );
  $("domainSearchForm").appendChild( i );
}

// run after the search. Re-enables the submit button
function dsCompleted() {
  $( "domainSearchForm" ).enable();
  Element.show( "resultsHeader" );
  Element.hide( "searchUpdateSpinner" );
}

// reset the forms
function resetDomainQueryForms() {
  [ "idSelection", "have", "not" ].each( function( list ) {
                                           log.debug( "resetting list \"" + list + "\"" );
                                           $A( $(list).childNodes ).each( function( n ) {
                                                                            $(list).removeChild( n );
                                                                          } )
                                             } );
}

//------------------------------------------------------------
//- external functions ---------------------------------------
//------------------------------------------------------------
// these functions are taken from http://www.quirksmode.org/

// calculate the position of the supplied object.

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

//----------------------------------------

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
// generate a pop-up window. Based on a function from  
// http://www.accessify.com/features/tutorials/the-perfect-popup/

function popUp( sURL, sType, sHeight, sWidth, sName ) {
  if( sName == "" ) {
    sName = "newWin";
  }
  var sOptions="";
  if( sType == "console" ) {
    sOptions = "resizable,scrollbars,height="+sHeight+",width="+sWidth;
  }
  if( sType == "fixed" ) {
    sOptions = "status,height="+sHeight+",width="+sWidth;
  }
  if( sType == "elastic") {
    sOptions = "toolbar,menubar,scrollbars,resizable,location,height="+sHeight+",width="+sWidth;
  }
  window.open( sURL, sName, sOptions );
}

//------------------------------------------------------------
// cookie handling functions

// this is a modified version of the "createCookies" method from
// quirksmode. Added the ability to specify the timeout value as
// days, hours or minutes, e.g. "10m". If the interval isn't
// specified, it defaults to minutes, so "10" is equivalent to "10m"

function createCookie( name, value, time, path ) {

  //console.debug( "pfFunctions.js:createCookie: creating a cookie: |%s|%s|%s|%s|", name, value, time, path );

  // was there a time specified ?
  var expires = "";
  if( time ) {

    try {
      var interval = time.charAt( time.length - 1 );

      // if the interval isn't specified, we default to "minutes" and
      // treat the whole of the period string as the period value
      var period;
      if( interval != "d" && interval != "h" && interval != "m" ) {
        period = time.substring( 0, time.length );
      } else {
        period = time.substring( 0, time.length - 1 );
      }

      // choose the multiplier - defaults to "minutes"
      var multiplier;
      switch( interval ) {
        case 'd': multiplier = period * 1000 * 60 * 60 * 24; break
        case 'h': multiplier = period * 1000 * 60 * 60;      break
        case 'm': multiplier = period * 1000 * 60;           break
        default:  multiplier = period * 1000 * 60;           break
      }

      // set the expiry date
      var date = new Date();
      date.setTime( date.getTime() + multiplier );
      var dateString = date.toUTCString();
      
      // make sure it's valid, just in case
      if( dateString != "Invalid Date" ) {
          expires = "; expires=" + dateString;
      }

    } catch( e ) {
      // default to a session cookie if something went wrong
      expires = "";
    }
  }

  // was there a path specified ?
  path = (path) ? path : "/";

  // add the cookie
  var cookieBody = name + "=" + value + expires + "; path=" + path;
  // console.debug( "pfFunctions.js:createCookie: cookie body: |" + cookieBody + "|" );
  try {
      document.cookie = cookieBody;
  } catch(e) {
    // console.error( "pfFunctions.js:createCookie: couldn't create cookie: " + e );
  }
}

//----------------------------------------

function readCookie( name ) {
  var nameEQ = name + "=";
  var ca = document.cookie.split( ';' );
  for( var i=0; i < ca.length; i++ ) {
    var c = ca[i];
    while( c.charAt( 0 ) == ' ' ) {
      c = c.substring( 1, c.length );
    }
    if( c.indexOf( nameEQ ) == 0 ) {
      return c.substring( nameEQ.length, c.length );
    }
  }
  return null;
}

//----------------------------------------
// this is UNTESTED !

function eraseCookie( name ) {
  createCookie( name, "", -1 );
}

//------------------------------------------------------------
