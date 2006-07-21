
// family.js
// jt6 20060721 WTSI
//
// javascript glue for the family section
//
// $Id: family.js,v 1.1 2006-07-21 14:48:40 jt6 Exp $

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
// callbacks for the domain graphics generation call

function dgSuccess( oResponse ) {
  Element.update( $("dgph").parentNode, oResponse.responseText );
}
function dgFailure() {
  Element.update( $("dgph"), "Domain graphics loading failed." );
}

//------------------------------------------------------------
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
  Element.update( $("caph"), "Alignment loading failed." );
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

