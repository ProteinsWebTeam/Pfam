
// pfFunctions.js
// jt6 20060412 WTSI
//
// javascript glue for the site. Requires the prototype library.
//
// $Id: pfFunctions.js,v 1.60 2008-07-28 14:16:16 jt6 Exp $

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
loadOptions.stdg = {}; // species tree domain graphics
loadOptions.pdg = {}; // proteome domain graphics
loadOptions.cstruc = {}; // clan structure tab
loadOptions.fstruc = {}; // family structure tab
loadOptions.simap ={}; //simap graphics
loadOptions.getDomains = {}; //get structural domains from cath and scop

//------------------------------------------------------------
// name the original window, so that we can target links back to it
// from child-windows

window.name = "pfamParentWin";

//------------------------------------------------------------
//- functions ------------------------------------------------
//------------------------------------------------------------

//------------------------------------------------------------
// highlight an "area" in an image map by overlaying a coloured div

// the URL from the overlay area
var overlayURL = "";

function showOverlay( e ) {
  //console.debug( "----------------------------------------------------------------------------" )
  //console.debug( "entering showOverlay" )

  // this is the <area> that fired the event
  var target = e.element();

  // this is the parent element of this area
  var parent = target.parentNode;

  // these are details of the <map> that contains this <area>
  var mapName = parent.name;
  var mapNum  = parent.id.substring( 11 );

  //console.debug( "target:   |" + target.id + "|" );
  //console.debug( "mapName:  |" + mapName + "|" );

  // find the ID of the <img> that uses this <map>
  var num = mapName.substring( 11 );
  var image = $("featuresImage" + num);

  // calculate where to place the overlay...
  var coords = target.coords.split(",");
  var width  = coords[2] - coords[0];
  var height = coords[3] - coords[1];
  var left = image.cumulativeOffset()[0] 
           + Number( coords[0] )
           - $("featuresMap").cumulativeOffset()[0] 
           + $("featuresMap").offsetLeft;

  var top  = image.cumulativeOffset()[1]
           + Number( coords[1] )
           - $("featuresMap").cumulativeOffset()[1]
           + $("featuresMap").offsetTop;

  //console.debug( "WxH+X,Y:  " + width + "x" + height + "+" + left + "," + top );

  // position the div; add a little offset to the div, just to make it line up
  $("overlay").setStyle( { width:  width + "px",
                           height: height + "px",
                           left:   left + "px",
                           top:    top - ( Prototype.Browser.IE ? 0 : 1 ) + "px" } );

  // if there's a target for the overlaid area, set that as the URL for the 
  // overlay div, so that we can use it in the tip
  if( target.href ) {
    overlayURL = target.href;
    $("overlay").addClassName( "linked" );
  } else {
    overlayURL = "";
    $("overlay").removeClassName( "linked" );
  }

  // find the contents of the label for the row containing this feature
  var label = $("featuresLabel" + mapNum).innerHTML;
  //console.debug( "label: |" + label + "|" );

  // set the tooltip contents. For IE we're going to make do with the tooltips
  // that IE adds anyway. For other browsers we'll make a prettier version
  if( ! Prototype.Browser.IE ) {
    
    // set the tooltip title
    var title = label + " feature" + ( target.href ? " (click for details)" : "" );

    // and create the tooltip 
    new Tip( $("overlay"), 
             target.title,
             { border: 0,
               radius: 0,
               title: title
             } );
    $("overlay").show();
    $("overlay").prototip.show();
  
    //console.debug( "added tip for area: |" + target.id + "|" );
  } else {
    $("overlay").show();
  }

  //console.debug( "leaving showOverlay" )
}

//----------------------------------------
// open a new window with the URL from a given feature

function openOverlayURL( e ) {
  if( overlayURL != "" ) {
    window.open( overlayURL );
  }
}
//----------------------------------------

// hide the div on mouseout
function removeOverlay( e ) {
  //console.debug( "hiding overlay" );
  $("overlay").hide();
  //console.debug( "removing tip from overlay" );
  if ( $("overlay").prototip !== undefined ) {
    $("overlay").prototip.remove();
  }
}

//----------------------------------------

// move a thin line across the image maps, by way of a cursor

var cObj;       // the cursor div
var fObj;       // the "featuresMap" div, which contains all this stuff
var images;     // an array of all of the images in the features map div
var tl, br, co, po; // various coordinates
var minX, maxX; // limits for cursor movement

var cursorInitialised = false; // flag to show whether the cursor is ready
var initialiseFailed  = false; // flag to show whether cursor initialisation failed

function initialiseCursor() {

  // get some handles on important elements
  cObj = $("cursor");
  fObj = $("featuresMap");
  images = $A( $("featuresMap").getElementsByTagName("img") ); 

  // calculate the various coordinates and offsets... 
  tl = Position.cumulativeOffset( images.first() );
  var lastImage = images.last();
  var bl = Position.cumulativeOffset( lastImage );
  var d;
  try {
    d = lastImage.getDimensions(); // throws an error in IE...
  } catch( e ) {
    d = { width:  bl[0]+1,
          height: bl[1]+1 };
  }
  br = [ bl[0] + d.width,
         bl[1] + d.height ];

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
    try {
      initialiseCursor();
    } catch( e ) {
      initialiseFailed  = true;
    }
  }

  if( initialiseFailed ) {
    return;
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

function loadDomains( iIndex, uri, iNum ) {

  // the message for the confirmation dialogue
  var msg = "You are about to load " + iNum + 
            " domain graphics, which may take some time.\n\nAre you sure you want to continue ?";

  // only ask for confirmation if there are 50 or more sequences to load
  var continueLoad = ( iNum >= 50 ) ? confirm( msg ) : true;

  if( continueLoad ) {
    ['adSpinner' + iIndex,
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
