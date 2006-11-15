
// pfamb.js
// jt6 20061018 WTSI
//
// javascript glue for the PfamB section
//
// $Id: pfamb.js,v 1.3 2006-11-15 11:04:24 rdf Exp $

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
  // pfamb structure tab
  if( typeof( loadOptions.fstruc.uri ) != "undefined" ) {
	 new Ajax.Request( loadOptions.fstruc.uri,
		 			 	 { method:     'get', 
			 			parameters: loadOptions.fstruc.params,
						onComplete: fstrucSuccess,
						onFailure:  fstrucFailure
					  } );
  }	
}

//------------------------------------------------------------
