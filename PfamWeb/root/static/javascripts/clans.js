
// clans.js
// jt6 20060721 WTSI
//
// javascript glue for the clans section
//
// $Id: clans.js,v 1.2 2006-08-14 10:53:23 jt6 Exp $

// load ajax components for the clans page

function clanPostLoad() {
  new Ajax.Request( loadOptions.cg.uri,
 					{ method: "get",
 					  parameters: loadOptions.cg.params,
 					  onComplete: cgSuccess,
 					  onFailure:  cgFailure
 					} );
}

//------------------------------------------------------------
// callbacks for the domain graphics generation call

function cgSuccess( oResponse ) {
  Element.update( $("clanGraphicsHolder"), oResponse.responseText );
}

function cgFailure() {
  Element.update( $("cgph"), "Graphics loading failed." );
}

