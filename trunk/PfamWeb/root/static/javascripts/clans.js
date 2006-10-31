
// clans.js
// jt6 20060721 WTSI
//
// javascript glue for the clans section
//
// $Id: clans.js,v 1.4 2006-10-31 15:07:43 jt6 Exp $

// load ajax components for the clans page

function clanPostLoad() {

  // domain graphics
  if( typeof( loadOptions.cg.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.cg.uri,
					  { method: "get",
						parameters: loadOptions.cg.params,
						onComplete: cgSuccess,
						onFailure:  cgFailure
					  } );
  }

  // add an example structure image to the summary tab
  if( typeof( loadOptions.si.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.si.uri,
					  { method:     "get", 
 					    parameters: loadOptions.si.params,
 					    onComplete: siSuccess
					    // not even bothering with a failure callback...
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

}

//------------------------------------------------------------
// callbacks for the domain graphics generation call

function cgSuccess( oResponse ) {
  Element.update( $("clanGraphicsHolder"), oResponse.responseText );
}

function cgFailure() {
  Element.update( $("cgph"), "Graphics loading failed." );
}

//------------------------------------------------------------
// callback for the structure image call

function siSuccess( oResponse ) {
  Element.update( $("siph"), oResponse.responseText );
}

