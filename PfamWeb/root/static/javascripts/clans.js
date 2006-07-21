
// clans.js
// jt6 20060721 WTSI
//
// javascript glue for the clans section
//
// $Id: clans.js,v 1.1 2006-07-21 14:48:40 jt6 Exp $

// load ajax components for the clans page

function clanPostLoad() {
  console.debug( "in clanPostLoad... uri = |" + loadOptions.cg.uri + "|" );
   new Ajax.Request( loadOptions.cg.uri,
 					{ method: "get",
 					  parameters: loadOptions.cg.params,
 					  onComplete: cgSuccess,
 					  onFailure:  cgFailure
 					} );
  console.debug( "in clanPostLoad; submitted request" );
}

//------------------------------------------------------------
// callbacks for the domain graphics generation call

function cgSuccess( oResponse ) {
  console.debug( "cgSuccess !" );
  Element.update( $("clanGraphicsHolder"), oResponse.responseText );
}

function cgFailure() {
  console.debug( "cgFailure !" );
  Element.update( $("cgph"), "Graphics loading failed." );
}

