
// sharedFunctions.js
// jt6 20080229 WTSI
//
// javascript glue for the site. Requires the prototype library.
//
// $Id: sharedFunctions.js,v 1.4 2008-05-16 14:58:22 jt6 Exp $

// Copyright (c) 2007: Genome Research Ltd.
// 
// Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
// 
// This is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
// 
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
// 
// You should have received a copy of the GNU General Public License along with
// this program. If not, see <http://www.gnu.org/licenses/>.

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

// These are the arguments:
//   oSwitch     - object:  the element that acts as the toggle switch
//   sId         - string:  the ID of the element to show/hide
//   bStartState - boolean: the starting state of the element, true = visible
//   bQuick      - boolean: false = slide element open, true = show immediately 

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
//- event handlers to add underlining to hovered links -------
//------------------------------------------------------------

// add listeners to all of the links in the page
function addHoverListeners() {
  $A( document.getElementsByClassName('link') ).each(
    function( l ) {
      Event.observe( l, 'mouseover', hoverHandler.bindAsEventListener(l), false );
      Event.observe( l, 'mouseout',  hoverHandler.bindAsEventListener(l), false );
    }
  );
}

// a function to make "links" underlined when hovered over
function hoverHandler(e) {
  if( e.type == 'mouseover' ) {
    var element = Event.element(e);
    if( ! ( typeof element === 'undefined' ) ) {
      element.addClassName('hover');
    }
  } else if( e.type == 'mouseout' ) {
    var element = Event.element(e);
    if( ! ( typeof element === 'undefined' ) ) {
      element.removeClassName('hover');
    }
  }
}

//------------------------------------------------------------
//- a function to handle the "jump box" ----------------------
//------------------------------------------------------------

// this function should be called as the target of the onSubmit event
// for a "jump to"-style form. The first element of the form should be
// the single field that should be submitted to the form action. Any
// hidden fields must come AFTER that entry field.

function jump(form) {
  // get a handle on the form itself
  var oForm = $(form);
  
  // get the spinner and display it, bearing in mind that there may not
  // actually BE a spinner
  var spinner;
  try {
    spinner = oForm.select("[class='jumpSpinner']").first()
                .show(); // if we find a spinner, make it visible immediately
  } catch( e ) {}
  
  // get the entry field in the form, which we assume to be the first field
  var entryField = oForm.findFirstElement();

  // submit the request
  var r = new Ajax.Request( oForm.action, {
    method: 'get',
    parameters: oForm.serialize( true ),

    // if the request was successful, we get back a URL. Redirect there
    onSuccess: function(oResponse) {
      if( spinner !== undefined ) {
        spinner.update("Loading entry...");
      }
      window.location = oResponse.responseText;
    },

    // if it failed, we show the error message
    onFailure: function(oResponse) {
      var errorDiv = oForm.select("[class='jumpError']").first()
                       .update( oResponse.responseText )
                       .show();
      
      // select the contents of the entry field, to make it easy for the
      // user to overwrite it
      entryField.select();

      // we want to hide the error message if the user starts to change the
      // contents of the entry field. Add an event listener to do that
      Event.observe( oForm.findFirstElement(),
                     "change", 
                     function() { errorDiv.hide(); } );

      // and we are done with the spinner now
      if( spinner !== undefined ) {
        spinner.hide();
      }
    }
  } );
  
  // we always return false, since a call to the "jump" URL will only return 
  // a raw URL, not page contents
  return false;
}
