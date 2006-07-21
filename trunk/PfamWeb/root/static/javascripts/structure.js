
// structure.js
// jt6 20060721 WTSI
//
// javascript glue for the structure section
//
// $Id: structure.js,v 1.1 2006-07-21 14:48:41 jt6 Exp $

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

