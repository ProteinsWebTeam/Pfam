
// family.js
// jt6 20060721 WTSI
//
// javascript glue for the family section
//
// $Id: family.js,v 1.8 2006-10-31 15:08:00 jt6 Exp $

// this will make the ajax calls for the family page components

function familyPostLoad() {

  // structure image
  if( typeof( loadOptions.si.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.si.uri,
					  { method:     'get', 
						parameters: loadOptions.si.params,
						onComplete: siSuccess
						// not even bothering with a failure callback...
					  } );
  }
  
  // domain graphics
  if( typeof( loadOptions.dg.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.dg.uri,
					  { method:     'get', 
						parameters: loadOptions.dg.params,
						onComplete: dgSuccess,
						onFailure:  dgFailure
					  } );
  }

  // species tree
  if( typeof( loadOptions.st.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.st.uri,
					  { method:     'get', 
						parameters: loadOptions.st.params,
						onComplete: stSuccess,
						onFailure:  stFailure
					  } );
  }

  // alignment tree
  if( typeof( loadOptions.at.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.at.uri,
					  { method:     'get', 
						parameters: loadOptions.at.params,
						onComplete: atSuccess,
						onFailure:  atFailure
					  } );
  }

  // coloured alignment
  if( typeof( loadOptions.ca.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.ca.uri,
					  { method:     'get', 
						parameters: loadOptions.ca.params,
						onComplete: caSuccess,
						onFailure:  caFailure
					  } );
  }
}

//------------------------------------------------------------
// callback for the structure image call

function siSuccess( oResponse ) {
  Element.update( $("siph"), oResponse.responseText );
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

var tree;
function stSuccess( oResponse ) {
  tree = new YAHOO.widget.TreeView("treeDiv");
  var root = tree.getRoot();
  eval( oResponse.responseText );
  tree.draw();
}
function stFailure() {
  Element.update( $("stph"), "Tree loading failed." );
}

//------------------------------------------------------------
//- alignment tree methods -----------------------------------
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
// callbacks for the coloured alignment

function caSuccess( oResponse ) {
  Element.update( $("caph"), oResponse.responseText );
}

function caFailure() {
  Element.update( $("caph"), "Coloured alignment loading failed." );
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
  Form.disable( "pagingForm" );
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
// tweak the alignment to scroll it horizontally to a saved point and
// to add links to the sequence IDs

function formatAlignment( urlBase) {
  // scroll the alignment to the same point as the previously viewed
  // alignment block
  $("alignmentData").scrollLeft = $F("scrollValue");

  // add links to the sequence IDs

  // pre-compile a regular expression to filter out the ID, start and
  // end residues
  var re = /^(.*?)\/(\d+)\-(\d+)$/;

  // get all of the spans and walk the list to add tags to each
  var spans = $("alignmentKey").getElementsByTagName( "span" );
  $A( spans ).each( function( item ) {
	  var s  = item.firstChild.nodeValue;
	  var ar = re.exec( s );

      // build the link
      var a = document.createElement( "a" );
	  var t = document.createTextNode( ar[1] );
      a.appendChild( t );
	  a.setAttribute( "href", urlBase + ar[1] );

      item.replaceChild( a, item.firstChild );

      // tack on the residue range, as plain text for now at least
	  var r = document.createTextNode( "/" + ar[2] + "-" + ar[3] );
      item.appendChild( r );
    }
  );

  Form.enable( "pagingForm" );
}

//------------------------------------------------------------
//- species tree methods -------------------------------------
//------------------------------------------------------------

// augment the base TextNode from Yahoo with functions to walk down
// the tree. These methods are here to allow cascades to work for
// TextNodes, although we're primarily interested in the TaskNodes,
// which have a check method from the outset

YAHOO.widget.TextNode.prototype.check = function( state ) { 
  for( var i = 0; i < this.children.length; i++ ) {
	this.children[i].check( state );
  }
};

YAHOO.widget.TextNode.prototype.uncheck = function( state ) { 
  this.check( 0 );
};

//------------------------------------------------------------
// toggle the highlighting of those sequences which are found in the 
// seed alignment

var seedsHighlighted = true;

function toggleHighlightSeed() {
  if( seedsHighlighted ) {
	var links = $A( document.getElementsByClassName("highlightSeed", "treeDiv") );
	links.each( function( a ) {
				  Element.removeClassName( a, "highlightSeed" );
				} );
	Element.update( "seedToggle", "Show" );
  } else {
	var divs = $A( document.getElementsByClassName("seedNode", "treeDiv") );
	divs.each( function( d ) {
				 if( nodeMapping[d.id] ) {
				   Element.addClassName( $(nodeMapping[d.id].labelElId), "highlightSeed" );
				 }
			   } );
	Element.update( "seedToggle", "Hide" );
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
// 	$$(".highlightSeed").each( function( summary ) {
// 		Element.removeClassName( summary, "highlightSeed" );
// 	  } );
//   } else {
// 	$$(".seedNode").each( function( summary ) {
// 		if( nodeMapping[summary.id] ) {
// 		  Element.addClassName( $(nodeMapping[summary.id].labelElId), "highlightSeed" );
// 		}
// 	  } );
//   }
//   seedsHighlighted = !seedsHighlighted;
// }

//------------------------------------------------------------
// toggle showing/hiding of the node summaries

var summariesVisible = true;

function toggleShowSummaries() {
  if( summariesVisible ) {
	$$("div.nodeSummary").each( function( node ) {
        Element.hide( node );
      } );
	Element.update( "sumToggle", "Show" );
  } else {
	$$("div.nodeSummary").each( function( node ) {
        Element.show( node );
      } );
	Element.update( "sumToggle", "Hide" );
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
// 	divs.each( function( d ) {
// 				Element.hide( d );
// 			  } );
//   } else {
// 	divs.each( function( d ) {
// 				Element.show( d );
// 			  } );
//   }
//   summariesVisible = !summariesVisible;
// }

//------------------------------------------------------------
// move the "tools palette" so that it remains at the top of the page
// as the browser window is scrolled

var toolStart;

function moveTools() { 
  if( typeof(toolStart) == "undefined" ) {
	toolStart = YAHOO.util.Dom.getY( "treeTools" );
  }
  
  var offset = document.documentElement.scrollTop
	|| document.body.scrollTop; // body in Safari

  var newY = ( offset > toolStart ) ? offset : toolStart;

  YAHOO.util.Dom.setY( "treeTools", newY );
}

//------------------------------------------------------------
// collect the sequences that are specified by the checked leaf nodes
// in the tree. Submit the form in the page which will act on those
// accessions

function collectSequences() {

  var seqs = "";

  var leaves = $A( document.getElementsByClassName( "leafNode", "treeDiv" ) );
  leaves.each( function( n ) {
				 var taskNode = nodeMapping[n.id];
				 if( taskNode.checked ) {
				   seqs = seqs + nodeSequences[n.id] + " ";
				 }
			   } );

  // escape the sequences string, just to be on the safe side
  $("seqs").value = escape(seqs);

  // and submit the form
  $("subTreeForm").submit();
}

// function collectSequencesSlowly() {

//   var seqs = "";

//   $$(".leafNode").each
// 	( function( summary ) {
// 	  var taskNode = nodeMapping[summary.id];
// 	  if( taskNode.checked ) {
// 		seqs = seqs + nodeSequences[summary.id] + " ";
// 	  }
// 	} );

//   // escape the sequences string, just to be on the safe side
//   $("seqs").value = escape(seqs);

//   // and submit the form
//   $("subTreeForm").submit();
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
// show/hide the tree tools palette

function toggleTools() {
  if( Element.visible("treeToolsContent") ) {
	Element.hide( "treeToolsContent" );
	Element.update( "toolsToggle", "Show" );
  } else {
	Element.show( "treeToolsContent" );
	Element.update( "toolsToggle", "Hide" );
  }
}

//------------------------------------------------------------
