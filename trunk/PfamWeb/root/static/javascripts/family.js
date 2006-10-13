
// family.js
// jt6 20060721 WTSI
//
// javascript glue for the family section
//
// $Id: family.js,v 1.4 2006-10-13 12:32:01 jt6 Exp $

// this will make the ajax calls for the family page components

function familyPostLoad() {
  new Ajax.Request( loadOptions.si.uri,
					{ method:     "get", 
 					  parameters: loadOptions.si.params,
 					  onComplete: siSuccess
					  // not even bothering with a failure callback...
 					} );
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

var seedsHighlighted = true;

function toggleHighlightSeed() {
  if( seedsHighlighted ) {
	$$(".highlightSeed").each( function( summary ) {
		Element.removeClassName( summary, "highlightSeed" );
	  } );
  } else {
	$$(".seedNode").each( function( summary ) {
		console.debug( "summary.id: |" + summary.id + "|" );
		if( nodeMapping[summary.id] ) {
		  console.debug( "retrieved node |" + nodeMapping[summary.id] + "|" );
		  Element.addClassName( $(nodeMapping[summary.id].labelElId), "highlightSeed" );
		}
	  } );
  }
  seedsHighlighted = !seedsHighlighted;
}

//------------------------------------------------------------

var summariesVisible = true;

function toggleShowSummaries() {
  if( summariesVisible ) {
	$$("div.nodeSummary").each( function( node ) {
        Element.hide( node );
      } );
  } else {
	$$("div.nodeSummary").each( function( node ) {
        Element.show( node );
      } );
  }
  summariesVisible = !summariesVisible;
}

//------------------------------------------------------------

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

function collectSequences() {

  $$(".leafNode").each
	( function( summary ) {
	  var taskNode = nodeMapping[summary.id];
	  console.debug( "checking taskNode |" + taskNode + "|" );
	  if( taskNode.checked ) {
		console.debug( "taskNode |" + taskNode + "| is checked" );
		console.debug( "selected sequences: |" + nodeSequences[summary.id] + "|" );
	  }
	} );

}

//------------------------------------------------------------
