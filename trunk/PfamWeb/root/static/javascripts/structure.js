
// structure.js
// jt6 20060721 WTSI
//
// javascript glue for the structure section
//
// $Id: structure.js,v 1.2 2006-09-28 09:45:21 rdf Exp $

// load ajax components for the structure page
function structurePostLoad() {
   new Ajax.Request( loadOptions.sg.uri,
 					{ method: "get",
 					  parameters: loadOptions.sg.params,
 					  onComplete: sgSuccess,
 					  onFailure:  sgFailure
 					} );
   new Ajax.Request( loadOptions.getDomains.uri,
 					{ method: "get",
 					  parameters: loadOptions.getDomains.params,
 					  onComplete: getDomainsSuccess,
 					  onFailure:  getDomainsFailure
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

//Structural domains
// called in response to a successful call
function getDomainsSuccess( oResponse ) {
  Element.update( $("getDomph"), oResponse.responseText );
}

// called in response to a failed call
function getDomainsFailure() {
  Element.update( $("getDomph"), "Graphics loading failed." );
}
