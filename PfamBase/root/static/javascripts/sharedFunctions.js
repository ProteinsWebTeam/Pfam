
// sharedFunctions.js
// jt6 20080229 WTSI
//
// javascript glue for the site. Requires the prototype library.
//
// $Id: sharedFunctions.js,v 1.12 2009-11-13 15:57:26 pg6 Exp $

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
  var startingRow = e.findElement("tr");

// if the mouseover event originates at a link node within a table
  // cell, we need to get the parent of the parent of the link node
  if( "td" == startingRow.nodeName || "TD" == startingRow.nodeName ) {
    startingRow = startingRow.parentNode;
  }

  // these are the cells that we'll need to colour
  var cells = new Array();

  // first, stash the cells in the starting row
  var startingCells = startingRow.select("td");
  cells.push( startingCells );
  // console.debug( "cells starts with " + cells.length + " cells" );

  // and then, if this row isn't the full width of the table, recurse down
  // (actually, up) the previous rows and collect more cells to highlight
  if ( startingCells.length < numColsTable ) {
    this.walkRows( startingRow, cells );
  }
  // console.debug( "retrieved " + cells.flatten().length + " cells" );

  // highlight the collected cells
  cells.flatten().each( function( cell ) {
    cell.addClassName( "stripeHover" );
    // console.debug( "added stripeHover for cell " + cell );
    highlightedCells.push( cell );
  } );
  
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
  highlightedCells.invoke( "removeClassName", "stripeHover" )
                  .clear();
};

//------------------------------------------------------------

// a prototype-based rewrite of the cbb function by Roger Johansson, 
// http://www.456bereastreet.com/

var CurvedBorders = Class.create( {

  initialize: function() {

    // get the elements to fix
    $$(".cbb").each( function( orig ) {

      // create a new wrapper element and give it the same class names as the 
      // original element, except for "cbb", which we switch for "cb"
      var outer = new Element( "div" );

      orig.classNames().each( function( cn ) {
        outer.addClassName( cn );
      } );

      outer.removeClassName( "cbb" )
           .addClassName( "cb" );

      // move the ID of the original element onto the new outer element
      // and give it a new class name to mark it as an inner element
      var outerId = orig.identify();
      orig.removeAttribute("id");
      outer.id = outerId;
      
      orig.removeClassName( "cbb" )
          .addClassName( "i3" );

      // switch the original element for the new outer element
      orig.parentNode.replaceChild( outer, orig );

      // two more nested elements...
      var i1 = new Element( "div", { "class": "i1" } );
      var i2 = new Element( "div", { "class": "i2" } );

      // string the inner elements together 
      outer.appendChild( i1 );
      i1.appendChild( i2 );
      i2.appendChild( orig );

      // add a top and bottom element
      this.insertTop( outer );
      this.insertBottom( outer );

    }.bind( this ) );
  },
  
  //----------------------------------------

  insertTop: function( obj ) {
    var outer = new Element( "div", { "class": "bt" } );
    outer.appendChild( new Element( "div" ) );
    obj.insertBefore( outer, obj.firstChild );
  },

  //----------------------------------------

  insertBottom: function( obj ) {
    var outer = new Element( "div", { "class": "bb" } );
    outer.appendChild( new Element( "div" ) );
    obj.appendChild( outer );
  }

} );

//------------------------------------------------------------

// a class that sets up the various components and behaviours of the 
// tabbed pages

var TabPage = Class.create( {
  
  initialize: function() {
  
    // this is all lifted directly from the YUI example page:
    //   http://developer.yahoo.com/yui/examples/history/history-tabview.html

    // see if we got here via a bookmark
    var bookmarkedTabViewState = YAHOO.util.History.getBookmarkedState( "tabview" ),
        initialTabViewState = bookmarkedTabViewState || "tab0";
    
    // register the "tabview" module with the history manager
    YAHOO.util.History.register( 
      "tabview", 
      initialTabViewState, 
      function (state) {
        this._tv.set( "activeIndex", state.substr(3) );
      }.bind( this )
    );

    // when the history manager is ready, initialise the TabView widget with 
    // whatever state we get from the history manager. The state is stored as 
    // "tabN", where "N" is the index number of the tab
    YAHOO.util.History.onReady( 
      function () {
        this._initTabView();
        var currentState = YAHOO.util.History.getCurrentState("tabview");
        this._tv.set ("activeIndex", currentState.substr(3) );
      }.bind( this )
    );

    // finally, initialise the history manager, which will, in turn, initialise 
    // the TabView widget. If initialising the history manager fails, we fall 
    // back on the function that initialises just the TabView, so that at least 
    // the page is usable
    try {
      YAHOO.util.History.initialize( "yui-history-field", "yui-history-iframe" );
    } catch (e) {
      this._initTabView();
    }
    
  },

  //----------------------------------------

  // set up the legacy tab switching mechanism. We need to continue support 
  // bookmarks that used the block names
  
  switchTab: function( id ) {

    // get a list of all list items in the sidebar, i.e. the tab selectors
    var tabs =  $("sidebar")
                  .descendants()
                  .findAll( function(el) { 
                    return el.nodeName == "LI";
                  } );

    if ( $(id) ) {

      // the argument was an id, so switch to that block
      var selector = id+"Selector";
      var tabIndex = tabs.indexOf( $(selector) );

      try {
        this._tv.selectTab( tabIndex );
      } catch ( e ) { }

    } else {

      // the argument doesn't appear to be a block; assume it's "next" or "prev".
      // This is a bit more complicated, because we need to take into account
      // any disabled tabs in the list...
      var selectedLi = $$("#sidebar li.selected").first();
      var selectedLiIndex = tabs.indexOf( selectedLi );

      // walk up/down the tabs list
      var limit = ( id == "next" ) ? tabs.size() - selectedLiIndex
                                   : selectedLiIndex;
      var newTabSelector;
      for ( var i = 0; i < limit; i++ ) {
        if ( id == "next" ) {
          newTabSelector = selectedLi.next("li", i);
        } else if ( id == "prev" ) {
          newTabSelector = selectedLi.previous("li", i);
        }
        if ( $(newTabSelector) && 
             ! $(newTabSelector).hasClassName("disabled") ) {
          break;
        }
      }

      if ( newTabSelector == undefined ) {
        return;
      }

      var tabIndex = tabs.indexOf( $(newTabSelector) );
      try {
        this._tv.selectTab( tabIndex );
      } catch ( e ) { }

    }
      
  },

  //----------------------------------------

  // a function to initialise the TabView widget
  
  _initTabView: function() {
    this._tv = new YAHOO.widget.TabView( "tabset" );
    
    this._tv.addListener( "activeTabChange",
                           this._handleTabChange, null, this );
    
    this._pageSetup();
  },

  //----------------------------------------

  // a handler function that is called whenever the TabView widget changes 
  // the tab. This is used to register the change with the history manager
  
  _handleTabChange: function( e ) {
    
    // avoid the second event that seems to 
    if ( e.newValue == e.prevValue ) {
      return;
    }
    
    var newState = "tab" + this._tv.getTabIndex( e.newValue ),
        currentState;

    try {
      currentState = YAHOO.util.History.getCurrentState("tabview");

      // only change tabs if we're not currently showing the requested tab,
      // otherwise we end up in an infinite loop
      if ( newState != currentState ) {
        YAHOO.util.History.navigate( "tabview", newState );
      }
    } catch (e) {
      // if the history manager doesn't work, change the tab anyway
      this._tv.set( "activeIndex", newState.substr(3) );
    }

    // get an ID for the tab that was just selected and register the change
    //with the urchin tracker
    var tabId = $$("#sidebar ul li")
                  .toArray()[newState.substr(3)]
                  .identify()
                  .replace( "Selector", "" );
    try {
      urchinTracker( "/tab/" + tabId );
    } catch( ex ) {
      // don't care
    }
  },

  //----------------------------------------

  // set up the final few bits of the page behaviour
  _pageSetup: function() {
    
    if ( $(showTab) ) {
      this.switchTab( showTab );
    } else if ( $$(".error").size() > 0 ) {
      // see if there's an error message in the page. If there is, we walk back
      // up the DOM tree from that error message node until we find the "block"
      // that encloses it, grab the ID for that block and use the tab switcher
      // to select it...
      this.switchTab( $$(".error")
                   .first()
                   .up("div.block")
                   .identify() );
    }

    // keep track of post-loading calls
     
    // register listeners for the start and end of each ajax call
    Ajax.Responders.register( {
      onCreate: function() {
        $('loadingComponentsCount').update( '&nbsp;(' + Ajax.activeRequestCount + ' remaining)' );
        $('loadingComponents').show();
      },
      onComplete: function() {
        $('loadingComponentsCount').update( '&nbsp;(' + Ajax.activeRequestCount + ' remaining)' );
        if( Ajax.activeRequestCount < 1 ) {
          $('loadingComponents').hide();
        }
      }
    } );

    // listen for keypresses and switch tabs based on which key was pressed
    document.observe( "keypress", function(e) {
      
      // don't capture events that originate on an input or textarea
      var targetNodeType = e.findElement().nodeName;
      if ( targetNodeType == 'INPUT' ||
           targetNodeType == 'TEXTAREA' ) {
        return;
      }

      var code;
      if ( e.keyCode ) {
        code = e.keyCode;
      } else if ( e.which ) {
        code = e.which;
      }

      switch (code) {
        case  75:            // "K"
        case 107:            // "k"
        case  80:            // "P"
        case 112:            // "p"
        /* case Event.KEY_UP:   // up arrow */
          this.switchTab("prev");
          break;
        case  74:            // "J"
        case 106:            // "j"
        case  78:            // "N"
        case 110:            // "n"
        /* case Event.KEY_DOWN: // down arrow */
          this.switchTab("next");
          break;
      }
    }.bind( this ) );

  }

} );

//------------------------------------------------------------
// a simple class to fix behavioural problems with wiki content. These are
// caused by us stripping javascript from the content when we import it, so
// we need to mimic at least the most important bits here.

var WikiContent = Class.create( {

  initialize: function( wikiContent ) {
    this._wikiContentEl = $(wikiContent);

    this._tocCount = 0;
    this._colCount = 0;

    this._collapseElements();
  },

  _collapseElements: function() {

    // collapse the table of contents and add a button to show/hide it
    this._wikiContentEl.select(".toc").each( function(toc) {
      var list     = toc.down("ul"),
          tocID    = toc.identify(),
          toggleId = tocID + "toctoggle" + this._tocCount,
          toggle   = new Element( "span", { "class": "toctoggle" } )
                       .update( "[<span class='link' id='" + toggleId + "'>hide</span>]" );
      list.previous("div").appendChild(toggle);
      $(toggleId).observe( "click", this._toggle.bind( this, toggleId, list ) );
      this._tocCount++;
    }.bind(this) );

    // collapse various other collapsible elements and add show/hide buttons
    this._wikiContentEl.select(".collapsible").each( function(col) {
      var colId    = col.identify(),
          toggleId = colId + "collapseButton" + this._colCount,
          toggle   = new Element( "span", { "class": "collapseButton" } )
                       .update( "[<span class='link' id='" + toggleId + "'>show</span>]" ),
          rows     = col.down("th").up("tr").nextSiblings();

      col.down("th").appendChild(toggle);

      $(toggleId).observe( "click", this._toggleRows.bind( this, toggleId, rows ) );

      if ( col.hasClassName("collapsed") ||
           col.hasClassName("autocollapse") ) {
        rows.invoke("hide");
      }

      this._colCount++;
    }.bind(this) );

  },

  // toggle the visibility of the supplied element and change the toggle switch text
  _toggle: function( toggleId, el ) {
    el.toggle();
    if ( el.visible() ) {
      $(toggleId).update("hide");
    } else {
      $(toggleId).update("show");
    }
  },

  // toggle the visibility of an array of elements
  _toggleRows: function( toggleId, rows ) {
    rows.each( function(row) {
      this._toggle( toggleId, row );
    }.bind(this) );
  }

} );

//------------------------------------------------------------
//- functions ------------------------------------------------
//------------------------------------------------------------

// switch between panels on the index page
function switchPanel( oTrigger, sId ) {
  
  // tell urchin about the switch
  try {
    urchinTracker( "/index/switchPanel/" + sId );
  } catch( e ) {}
  
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
  } else {
    newIH = oSwitch.innerHTML;
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
      var block = $$("div.block").first();
      if( block && block.id ) {
        show( block.id );
      }
    }
  }
}

//------------------------------------------------------------
// display the specified tab in the page body

// function switchTab( sId ) {
//   // show/hide the blocks themselves
//   $$("#content div.block").each( function( block ) {
//                                    if( sId == block.id ) {
//                                      block.setStyle( { display: "block" } );
//                                    } else {
//                                      block.hide();
//                                    }
//                                  } );
// 
//   // set the appropriate selector in the sidebar
//   $$("#sidebar li").each( function( item ) {
//                             if( sId+"Selector" == item.id ) {
//                               item.addClassName( "selected" );
//                             } else {
//                               item.removeClassName( "selected" );
//                             }
//                           } );
// }

//------------------------------------------------------------
// show/hide a tools palette

function toggleTools( sToggle, sContent ) {
  var toggle  = $(sToggle),
      content = $(sContent);

  if( content.visible() ) {
    content.hide();
    toggle.update( "Show" );
  } else {
    content.show();
    toggle.update( "Hide" );
  }
}

// function toggleTools() {
//   if( $("toolsContent").visible() ) {
//     $("toolsContent").hide();
//     $("toolsToggle").update( "Show" );
//   } else {
//     $("toolsContent").show();
//     $("toolsToggle").update( "Hide" );
//   }
// }

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

// unmodified versions of the cookie handling functions from quirksmode.
// (The modified versions weren't behaving well...)

function createCookie(name,value,days) {
  if (days) {
    var date = new Date();
    date.setTime(date.getTime()+(days*24*60*60*1000));
    var expires = "; expires="+date.toGMTString();
  }
  else var expires = "";
  document.cookie = name+"="+value+expires+"; path=/";
}

function readCookie(name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1,c.length);
    if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
  }
  return null;
}

function eraseCookie(name) {
  createCookie(name,"",-1);
}

// this is a modified version of the "createCookies" method from
// quirksmode. Added the ability to specify the timeout value as
// days, hours or minutes, e.g. "10m". If the interval isn't
// specified, it defaults to minutes, so "10" is equivalent to "10m"

/* 
 * function createCookie( name, value, time, path ) {
 * 
 *   //console.debug( "pfFunctions.js:createCookie: creating a cookie: |%s|%s|%s|%s|", name, value, time, path );
 * 
 *   // was there a time specified ?
 *   var expires = "";
 *   if( time ) {
 *     console.debug( "'time' was set to: |" + time + "|" );
 * 
 *     try {
 *       var interval = time.charAt( time.length - 1 );
 * 
 *       // if the interval isn't specified, we default to "minutes" and
 *       // treat the whole of the period string as the period value
 *       var period;
 *       if( interval != "d" && interval != "h" && interval != "m" ) {
 *         period = time.substring( 0, time.length );
 *       } else {
 *         period = time.substring( 0, time.length - 1 );
 *       }
 * 
 *       // choose the multiplier - defaults to "minutes"
 *       var multiplier;
 *       switch( interval ) {
 *         case 'd': multiplier = period * 1000 * 60 * 60 * 24; break
 *         case 'h': multiplier = period * 1000 * 60 * 60;      break
 *         case 'm': multiplier = period * 1000 * 60;           break
 *         default: multiplier = period * 1000 * 60;           break
 *       }
 * 
 *       // set the expiry date
 *       var date = new Date();
 *       date.setTime( date.getTime() + multiplier );
 *       var dateString = date.toUTCString();
 *       
 *       // make sure it's valid, just in case
 *       if( dateString != "Invalid Date" ) {
 *           expires = "; expires=" + dateString;
 *       }
 * 
 *     } catch( e ) {
 *       // default to a session cookie if something went wrong
 *       expires = "; expires=" + time;
 *     }
 *   
 *   }
 * 
 *   // was there a path specified ?
 *   path = (path) ? path : "/";
 * 
 *   // add the cookie
 *   var cookieBody = name + "=" + value + expires + "; path=" + path;
 *   //console.debug( "pfFunctions.js:createCookie: cookie body: |" + cookieBody + "|" );
 *   try {
 *     document.cookie = cookieBody;
 *   } catch(e) {
 *     //console.error( "pfFunctions.js:createCookie: couldn't create cookie: " + e );
 *   }
 * }
 */

//----------------------------------------

/*
 * function readCookie( name ) {
 *   var nameEQ = name + "=";
 *   var ca = document.cookie.split( ';' );
 *   for( var i=0; i < ca.length; i++ ) {
 *     var c = ca[i];
 *     while( c.charAt( 0 ) == ' ' ) {
 *       c = c.substring( 1, c.length );
 *     }
 *     if( c.indexOf( nameEQ ) == 0 ) {
 *       return c.substring( nameEQ.length, c.length );
 *     }
 *   }
 *   return null;
 * }
 */

//----------------------------------------
// this is UNTESTED !

/*  
 * function eraseCookie( name ) {
 *   createCookie( name, "", -1 );
 * }
 */

//------------------------------------------------------------
//- event handlers to add underlining to hovered links -------
//------------------------------------------------------------

// add listeners to all of the links in the page
function addHoverListeners() {
  $$(".link").each(
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
    parameters: oForm.serialize( true ),

    // if the request was successful, we get back a URL. Redirect there
    onSuccess: function(oResponse) {
      if( spinner !== undefined ) {
        spinner.update("Loading entry...");
      }
      
      // strip off the meaningful bit of the URI and tell urchin about it
      var uri        = oResponse.responseText;
      var matches    = uri.match( /.*?(\/\w+\/\w+)$/ );
      if ( matches ) {
        var jumpTarget = matches[1];
        if ( jumpTarget !== "undefined") {
          try {
            urchinTracker("/jump" + jumpTarget);
          } catch (e) {}
        }
      }
      window.location = uri;
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
      oForm
        .findFirstElement()
        .observe(
          "change", 
          function() { errorDiv.hide(); }
        );

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
//------------------------------------------------------------
//- species tree methods -------------------------------------
//------------------------------------------------------------

// toggle the highlighting of those sequences which are found in the 
// seed alignment

var seedsHighlighted = true;

function toggleHighlightSeed() {
  if( seedsHighlighted ) {
    $("treeDiv")
      .select(".highlightSeed")
      .invoke( "removeClassName", "highlightSeed" );
    $("seedToggle").update( "Show" );
  } else {
    $("treeDiv")
      .select(".seedNode")
      .invoke( "addClassName", "highlightSeed" );
    $("seedToggle").update( "Hide" );
  }
  seedsHighlighted = !seedsHighlighted;
}

//------------------------------------------------------------
// toggle showing/hiding of the node summaries

var summariesVisible = true;

function toggleShowSummaries() {
  if( summariesVisible ) {
    $$("div.nodeSummary").invoke( "hide" )
    $("sumToggle").update( "Show" );
  } else {
    $$("div.nodeSummary").invoke( "show" );
    $("sumToggle").update( "Hide" );
  }
  summariesVisible = !summariesVisible;
}

// turns out that the $$() function is quicker than walking the tree
// in this case... who knew ?
// jt6 20061016 WTSI
//
// function toggleShowSummariesSlowly() {
//   var divs = $A( document.getElementsByClassName("nodeSummary","treeDiv") );
//   if( summariesVisible ) {
//   divs.each( function( d ) {
//         Element.hide( d );
//         } );
//   } else {
//   divs.each( function( d ) {
//         Element.show( d );
//         } );
//   }
//   summariesVisible = !summariesVisible;
// }

//------------------------------------------------------------
// expand the tree to the depth specified in the little form in the
// tools palette

function expandToDepth() {
  tree.collapseAll();
  expandTo( $F("depthSelector"), tree.root );
}

// the method that actually expands to a given depth. Should really
// only be called by expandToDepth()
var currentDepth = 0;

function expandTo( finalDepth, node ) {

  if( currentDepth < finalDepth - 1 ) {

    for( var i=0; i< node.children.length; ++i ) {
    
      var c = node.children[i];
      c.expand();

      currentDepth++;
      expandTo( finalDepth, c );
      currentDepth--;
    }
  }

}

//------------------------------------------------------------
// unhighlight all highlighted nodes

function unhighlightAll() {
  var checkedNodes = tree.getNodesByProperty( 'highlightState', 1 )
  if( checkedNodes !== "undefined" && checkedNodes.size() ) {
    checkedNodes.invoke('unhighlight');
  }
}

//------------------------------------------------------------
// collect the sequences that are specified by the checked leaf nodes
// in the species trees. Submits the form in the page which will act on those
// accessions. The argument should be either "G" or "A", for graphical or
// sequence alignment view of the collected sequences.

function collectSequences( sStyle, sAcc ) {

  // retrieve all checked nodes in the tree
  var checkedNodes = tree.getNodesByProperty( "highlightState", 1 );  
  
  // make sure we have at least one checked node
  if( ! checkedNodes.size() ) {
    $("stError")
      .update( "Please select some nodes" )
      .show();
    return;
  }

  var seqAccs = checkedNodes.inject( "", function( accumulator, n ) {
    if ( typeof n.data == "string" &&
         nodeSequences[n.data] &&
         nodeSequences[n.data] !== "undefined" ) {
      return accumulator + nodeSequences[n.data] + " ";
    } else { 
      return accumulator;
    }
  } );

  // TODO we could optimise this a bit, by storing the list of selected 
  // accessions at this point and then checking the new list against the old
  // list before making another AJAX request to store a second, identical list
  // in the DB. 

  // store the IDs and get back a "job id"
  var jobId;
  var r = new Ajax.Request( selectStoreURI, {
    method: 'post',
    parameters: { ids: escape( seqAccs ) },
    onSuccess: function( oResponse ) {

      // the response should contain only the "job ID", which points to the list
      // of accessions
      jobId = oResponse.responseText;

      // build the URI for the next request, the one that actually does the 
      // work here
      var url;
      var popup = true;

      // view the selected sequences as...
      switch( sStyle ) {
        case 'G':
          url = selectGraphicsURI;   // domain graphics
          break;
        case 'L':
          url = selectAccessionsURI; // sequence accessions
          popup = false;
          break;
        case 'F':
          url = selectFastaURI;      // FASTA format
          popup = false;
          break;
        case 'S':
          url = selectRfamAlignmentURI; // Stockholm format Rfam alignment
          popup = false;
          break;
        default:
          url = selectPfamAlignmentURI;  // a Pfam alignment
      }

      // tack on the parameters that we need
      url +=   "?acc="   + sAcc
             + "&jobId=" + jobId;
        
      // load that URL, either in a popup or in the main window
      if( popup ) {
        popUp( url, 'console', 800, 800, 'selectedSeqsWin' );
      } else {
        window.location = url;
      }
      
    },
    onFailure: function( oResponse ) {
      $("stError")
        .update( "There was a problem collecting sequence accessions" )
        .show();
      return;
    }
  });

}


