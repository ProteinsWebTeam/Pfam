
// pfFunctions.js
// jt6 20060412 WTSI
//
// javascript glue for the site. Requires the prototype library.
//
// $Id: pfFunctions.js,v 1.2 2006-05-15 11:01:41 jt6 Exp $

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


// show/hide the DAS sources selection panel
showSources = true;
function revealDASSources() {
  if( showSources ) {
	Effect.BlindDown('checkboxes');
	showSources = false;
	Element.update( $("revealControl"), "Hide" );
  } else {
	Effect.BlindUp('checkboxes');
	showSources = true;
	Element.update( $("revealControl"), "Show" );
  }
}


// callbacks for the domain graphics generation call
function dgSuccess( oResponse ) {
  Element.update( $("dgph").parentNode, oResponse.responseText );
}
function dgFailure() {
  Element.update( $("dgph"), "Domain graphics loading failed." );
}


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


// re-load the alignment tree
//function loadAlignmentTree( 


// set-up to defer loading of the heavier components of the page
Event.observe( window, "load", postLoad, false );

// individual blocks will populate this...
var loadOptions = {};

// and this will make the ajax calls
function postLoad() {
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
