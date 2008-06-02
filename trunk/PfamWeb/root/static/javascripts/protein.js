
// protein.js
// jt6 20060721 WTSI
//
// javascript glue for the protein section
//
// $Id: protein.js,v 1.13 2008-06-02 14:43:09 jt6 Exp $

// Copyright (c) 2007: Genome Research Ltd.
// 
// Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
// 
// This is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program. If not, see <http://www.gnu.org/licenses/>.

// this will make the ajax calls for the protein page components

function proteinPostLoad() {
  if( typeof( loadOptions.pg.uri ) != "undefined" ) {
    new Ajax.Request( loadOptions.pg.uri,
             					{ method:     "get",
             					  parameters: loadOptions.pg.params,
             					  onSuccess:  pgSuccess,
             					  onFailure:  pgFailure
             					} );
  }
  if( typeof( loadOptions.simap.uri ) != "undefined" ) {
    new Ajax.Request( loadOptions.simap.uri,
            					{ method:     "get",
            					  parameters: loadOptions.simap.params,
            					  onSuccess:  simapSuccess,
            					  onFailure:  simapFailure
            					} );
  }
  $("checkboxes").hide();
  $("plainSequence").hide();
}

//------------------------------------------------------------
// callbacks for the alignment/DAS graphics

// called in response to a successful call - stuff the response
// into the page and enable the submit button

function pgSuccess( oResponse ) {
  $("pgSubmitButton").enable();
  $("pgUpdateSpinner").hide()
  $("graphicsHolder").update( oResponse.responseText );
}

// called in response to a failed call - bugger. Leave the
// button disabled but hide the spinner

function pgFailure() {
  $("pgSubmitButton").disable();
  $("pgUpdateSpinner").hide();
  $("graphicsHolder").update( "Alignment loading failed." );
}

//------------------------------------------------------------

// called in response to a successful call
function simapSuccess( oResponse ) {
  $("simapGraphicsHolder").update( oResponse.responseText );
}

// called in response to a failed call
function simapFailure() {
  $("simapph").update( "Contacting SIMAP W/S - failed." );
}

//------------------------------------------------------------


