
// pfFunctions.js
// jt6 20060412 WTSI
//
// javascript glue for the site. Requires the prototype library.
//
// $Id: pfFunctions.js,v 1.58 2008-06-02 14:43:09 jt6 Exp $

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
  //console.debug( "entering showOverlay" )

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

  //console.debug( "target:   |" + target.id + "|" );
  //console.debug( "mapName:  |" + mapName + "|" );

  // find the ID of the <img> that uses this <map>
  var num = mapName.substring( 11 );
  var image = $("featuresImage" + num);

  // where to place the overlay...
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

  //console.debug( "WxH+X,Y:  " + width + "x" + height + "+" + left + "," + top );

  // position the div
  $("overlay").setStyle( { width:  width + "px",
                           height: height + "px",
                           left:   left + "px",
                           top:    top + "px" } );

  // find the contents of the label for the row containing this feature
  var mapNum = target.parentNode.id.substring( 11 );
  var label = $("featuresLabel" + mapNum).innerHTML;
  //console.debug( "features label: |" + label + "|" );

  // set the tooltip contents. For IE we're going to make do with the tooltips
  // that IE adds anyway. For other browsers we'll make a prettier version
  if( ! Prototype.Browser.IE ) {
    
    // retrieve a URL from the feature, if present, and set various
    // other bits and pieces...
    var title = label + " feature" + ( target.href ? " (click for details)" : "" );

    // set the options on the tooltip
    var options = { title: title,
                    offset: { x: 10, y: 10 } };

    // and create the tooltip 
    new Tip( "overlay", target.title, options );
  }

  // if there's a target for the overlaid area, set that as the URL for the 
  // overlay div
  if( target.href ) {
    overlayURL = target.href;
    $("overlay").addClassName( "linked" );
  } else {
    overlayURL = "";
    $("overlay").removeClassName( "linked" );
  }

  // and finally, display the overlay
  $("overlay").show();

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
  //console.debug( "removing overlay" );
  $("overlay").hide();
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
    new Ajax.Updater( 'domainArch' + iIndex, 
                      uri,
                      { method: 'get' } );
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
                      method: 'get',
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
//------------------------------------------------------------
//- species tree methods -------------------------------------
//------------------------------------------------------------

// toggle the highlighting of those sequences which are found in the 
// seed alignment

var seedsHighlighted = true;

function toggleHighlightSeed() {
  if( seedsHighlighted ) {
    $("treeDiv").select(".highlightSeed").each(
      function( a ) {
        a.removeClassName( "highlightSeed" );
      }
    );
    $("seedToggle").update( "Show" );
  } else {
    $("treeDiv").select(".seedNode").each(
      function( d ) {
        if( nodeMapping[d.id] ) {
          $(nodeMapping[d.id].labelElId).addClassName( "highlightSeed" );
        }
      }
    );
    $("seedToggle").update( "Hide" );
  }
  seedsHighlighted = !seedsHighlighted;
}

// the $$() function in prototype is variously described as wonderful
// or immensely slow, so we'll ditch it in favour of walking the DOM
// ourselves. This function is just here for historical reasons...
// jt6 20061016 WTSI
//
// function toggleHighlightSeedSlowly() {
//   if( seedsHighlighted ) {
//   $$(".highlightSeed").each( function( summary ) {
//     Element.removeClassName( summary, "highlightSeed" );
//     } );
//   } else {
//   $$(".seedNode").each( function( summary ) {
//     if( nodeMapping[summary.id] ) {
//       Element.addClassName( $(nodeMapping[summary.id].labelElId), "highlightSeed" );
//     }
//     } );
//   }
//   seedsHighlighted = !seedsHighlighted;
// }

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
// collect the sequences that are specified by the checked leaf nodes
// in the species trees. Submits the form in the page which will act on those
// accessions. The argument should be either "G" or "A", for graphical or
// sequence alignment view of the collected sequences.

function collectSequences( sStyle, sAcc ) {

  // get all leaf nodes
  var leaves = $("treeDiv").select(".leafNode");

  // and collect IDs from the checked ones
  var bail = true;
  var seqAccs = leaves.inject( "", function( accumulator, n ) {
      var taskNode = nodeMapping[n.id];
      if( taskNode.checked ) {
        bail = false;
        return accumulator + nodeSequences[n.id] + " ";
      } else {
        return accumulator;
      }
    }
  );

  // make sure we have at least one checked node
  if( bail ) {
    $("stError")
      .update( "Please select some nodes" )
      .show();
    return;
  }

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
        default:
          url = selectAlignmentURI;  // an alignment
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

//------------------------------------------------------------
//- callbacks for the species tree generation calls ----------
//------------------------------------------------------------
// this stuff is used in Pfam-A, Pfam-B and clan pages

var tree;
function stSuccess( oResponse ) {
  
  // build the tree widget and get a handle on the root, which we'll need
  // when eval'ing the javascript from the server
  tree = new YAHOO.widget.TreeView("treeDiv");
  var root = tree.getRoot();

  // eval the JS that the server generates. This is the set of calls that
  // build the TreeView widget object tree
  try {
    eval( oResponse.responseText );
  } catch( e ) {
    // don't care
  }

  // by this point the tree was successfully built, but the response might
  // have contained a message rather than tree components. If there was a
  // a tree, we must have more than just the root node
  if( YAHOO.widget.TreeView.nodeCount > 1 ) {
    // we got a tree; render it
    tree.draw();
    
    // bring back the control panel
    $("treeTools").show();
  } else {

    // we got a message from the server; display it
    $("treeDiv").update( oResponse.responseText );

    // hide the control panel too
    $("treeTools").hide();
  }
}

function stFailure() {
  $("treeDiv").update( "Tree loading failed." );
}

// this is an extra method to submit a new ajax request, this time with
// the "loadTree" flag set, which tells the controller to load the tree
// even it's large
function forceLoad() {
  
  // show the new spinner and disable the button
  $("secondaryLoadingSpinner").show();
  $("generateButton").disable();
  
  // override the limits on tree size
  loadOptions.st.params['loadTree'] = 1;

  // and override the browser check that will return a text tree when it 
  // sees IE coming 
  loadOptions.st.params['ie'] = false;
  
  new Ajax.Request( loadOptions.st.uri, // same URI was for original call
                    { method:     'get', 
                      parameters: loadOptions.st.params,
                      onSuccess:  stSuccess,
                      onFailure:  stFailure
                    } );
}

