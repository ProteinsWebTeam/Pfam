
// protein.js
// jt6 20060721 WTSI
//
// javascript glue for the protein section
//
// $Id: protein.js,v 1.4 2006-09-07 10:15:42 jt6 Exp $

// this will make the ajax calls for the protein page components

function proteinPostLoad() {
  new Ajax.Request( loadOptions.pg.uri,
 					{ method: "get",
 					  parameters: loadOptions.pg.params,
 					  onComplete: pgSuccess,
 					  onFailure:  pgFailure
 					} );

  new Ajax.Request( loadOptions.simap.uri,	
  					{ method: "get",
					  parameters: loadOptions.simap.params,
					  onComplete: simapSuccess,
					  onFailure:  simapFailure
					} );
}

// show/hide the specified drop-down panel

showItems = {};

function reveal( oSwitch, sId ) {
  var oSource = $(sId);
  if( showItems[sId] ) {
	// console.debug( sId + " is currently shown" );
	Effect.BlindUp( oSource, { duration: 0.3 } );
	showItems[sId] = false;
	Element.update( oSwitch, "Show" );
  } else {
	// console.debug( sId + " is currently hidden" );
	Effect.BlindDown( oSource, { duration: 0.3 } );
	showItems[sId] = true;
	Element.update( oSwitch, "Hide" );
  }
}

//------------------------------------------------------------
// callbacks for the alignment/DAS graphics

// called when the browser first fires the request - basically we
// just disable the submit button and show the "loading" spinner

function pgStarted( oResponse ) {
  $("pgSubmitButton").disabled = true;
  $("pgUpdateSpinner").style.visibility = "visible";
}

// called when the request completes - re-enable the button and 
// hide the spinner

function pgCompleted() {
  $("pgUpdateSpinner").style.visibility = "hidden";
  $("pgSubmitButton").disabled = false;
}

// called in response to a successful call - stuff the response
// into the page and enable the submit button

function pgSuccess( oResponse ) {
  Element.update( $("graphicsHolder"), oResponse.responseText );
  $("pgSubmitButton").disabled = false;
}

// called in response to a failed call - bugger. Leave the
// button disabled but hide the spinner

function pgFailure() {
  Element.update( $("pgph"), "Alignment loading failed." );
  $("pgUpdateSpinner").style.visibility = "hidden";
}

//------------------------------------------------------------

// called in response to a successful call
function simapSuccess( oResponse ) {
  Element.update( $("simapGraphicsHolder"), oResponse.responseText );
}

// called in response to a failed call
function simapFailure() {
  Element.update( $("simapph"), "Contacting SIMAP W/S - failed." );
}

//------------------------------------------------------------


