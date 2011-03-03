
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, $R, $w, $break, Class, console, Element, Hash, Event, document,
  window, G_vmlCanvasManager, Template, Tip */

// spoof a console, if necessary, so that we can run in IE (<8) without having
// to entirely disable debug messages
if ( ! window.console ) {
  window.console     = {};
  window.console.log = function() {};
}  

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A javascript object to control the drawing of domain graphics on the 
// UniProt sequence domain graphics generator page.
//
// jt6 20110303 WTSI
//
// $Id$
//
// Copyright (c) 2011: Genome Research Ltd.
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


var UniprotGraphicGenerator = Class.create( {
  
  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------

  initialize: function( base ) {

    this._pg = new PfamGraphic();

    // stash the URL root
    this._URLBase = base;

    // set up sequence field
    $("entry").focus();
    $("entry").select();

    // add listeners to the various buttons
    $("submit").observe( "click", this.generate.bind(this) );
    $("clear").observe( "click", this.clear.bind(this) );

    // catch "enter" being hit in the text field
    $("entry").observe( "keydown", function( e ) {
      if ( e.keyCode == 13 ) {
        this.generate();
      }
    }.bind( this ) );
  },

  //----------------------------------------------------------------------------
  //- methods ------------------------------------------------------------------
  //----------------------------------------------------------------------------
  
  generate: function() {

    // get rid of the "no graphic yet" message
    if ( $("none") ) {
      $("none").remove();
    }

    // hide any previous error messages and remove the previous canvas element
    $("errors").hide();
    if ( $("dg").select("canvas").size() > 0 ) {
      $("dg").select("canvas").first().remove();
    }

    // have to have *something* in the box
    if ( $("entry").getValue().empty() ) {
      return;
    }

    // has to be vaguely sensible
    if ( ! $("entry").getValue().match('^\\w+$') ) {
      $("error").update( "Not a valid Uniprot accession or ID" );
      $("errors").show();
      return;
    }

    // build the URL for retrieving the JSON graphic description
    var url = this._URLBase.replace( "%s", $("entry").getValue() );

    // set up the call-backs to handle the response
    var r = new Ajax.Request( url, {
      onLoading: function() {
        $("loading").show();
      },
      onSuccess: function( response ) {
        var json = response.responseJSON;

        if ( ! json ) {
          $("error").update( "Failed to retrieve the graphic" );
          $("errors").show();
          return;
        }

        console.log( "got a JSON string: ", json );
        this._json = json[0];

        this._generate();

      }.bind( this ),
      onFailure: function( response ) {
        $("error").update( "There was a problem retrieving the graphic" );
        $("errors").show();
      },
      onComplete: function() {
        $("loading").hide();
      }
    } );
  },

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------

  // actually generate the graphic
  _generate: function() {

    // set up the PfamGraphic object
    this._pg.setParent( "dg" );

    this._pg.setImageParams( {
      xscale: $F("xscale"),
      yscale: $F("yscale")
    } );

    // render the sequence
    try {
      this._pg.setSequence( this._json );
      this._pg.render();
    } catch ( e ) {
      $("error").update( e );
      $("errors").show();
      return;
    }
  },

  //----------------------------------------------------------------------------

  clear: function() {
    $("errors").hide();
    $("entry").setValue("");
    $("entry").focus()
    $("entry").select();
  }

  //----------------------------------------------------------------------------

} );

