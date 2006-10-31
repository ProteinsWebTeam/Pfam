
// pfamb.js
// jt6 20061018 WTSI
//
// javascript glue for the PfamB section
//
// $Id: pfamb.js,v 1.2 2006-10-31 15:08:57 jt6 Exp $

// this will make the ajax calls for the family page components

function pfambPostLoad() {
  if( typeof( loadOptions.st.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.st.uri,
					  { method:     "get", 
						parameters: loadOptions.st.params,
						onComplete: stSuccess,
						onFailure:  stFailure
					  } );
  }
  if( typeof( loadOptions.dg.uri ) != "undefined" ) {
	new Ajax.Request( loadOptions.dg.uri,
					  { method:     'get', 
						parameters: loadOptions.dg.params,
						onComplete: dgSuccess,
						onFailure:  dgFailure
					  } );
  }
}

//------------------------------------------------------------
