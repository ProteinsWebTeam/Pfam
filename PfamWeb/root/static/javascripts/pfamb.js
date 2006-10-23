
// pfamb.js
// jt6 20061018 WTSI
//
// javascript glue for the PfamB section
//
// $Id: pfamb.js,v 1.1 2006-10-23 12:09:25 jt6 Exp $

// this will make the ajax calls for the family page components

function pfambPostLoad() {
  new Ajax.Request( loadOptions.st.uri,
 	                { method:     "get", 
 					  parameters: loadOptions.st.params,
                      onComplete: stSuccess,
                      onFailure:  stFailure
 					} );
}

//------------------------------------------------------------
