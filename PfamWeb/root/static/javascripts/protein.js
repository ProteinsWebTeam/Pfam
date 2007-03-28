
// protein.js
// jt6 20060721 WTSI
//
// javascript glue for the protein section
//
// $Id: protein.js,v 1.9 2007-03-28 14:41:46 jt6 Exp $

// Copyright (c) 2007: Genome Research Ltd.
// 
// Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
// 
// This is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//  
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//  
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
// or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

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

  Element.hide("checkboxes","plainSequence");
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


