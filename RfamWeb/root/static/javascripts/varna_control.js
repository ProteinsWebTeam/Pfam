
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, $w, Class, console, Element, window, Template */

// spoof a console, if necessary, so that we can run in IE (<8) without having
// to entirely disable debug messages
if ( ! window.console ) {
  window.console     = {};
  window.console.log = function() {};
}  

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A javascript library for controlling the VARNA applet.
//
// jt6 20100328 WTSI
//
// $Id$
//
// Copyright (c) 2009: Genome Research Ltd.
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

var VarnaControl = Class.create( {
  /**
   * @lends VarnaControl
   * @author John Tate
   */

  /**
   * A template for building the markup for the applet tag.
   *
   * @private
   */
  _appletTemplateString: '\
<applet id="VARNA"\
        code="VARNA.class"\
        codebase="static/varna"\
        archive="#{jarUri}"\
        width="760"\
        height="600">\
  <param name="java_version" value="1.5+" />\
  <param name="flat" value="true" />\
  <param name="sequenceDBN"  value="#{sequence}" />\
  <param name="structureDBN" value="#{structure}" />\
</applet>',

  /**
   * A hash giving the options for the colour map chooser.
   * 
   * @private
   */
  _ssOptions: [
    { value: "cons", "label": "Conservation" },
    { value: "cov",  "label": "Co-variance" },
    { value: "ent",  "label": "Entropy" },
    { value: "fcbp", "label": "Fraction canonical basepairs" }
  ],

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * @class 
   * A javascript class for controlling the VARNA applet. No user serviceable
   * parts; just instantiate the object with the necessary arguments. Builds 
   * the required markup and attaches the appropriate listeners, etc.
   * </p>
   * <p>
   *
   * @description Builds a new VarnaControl object. Needs the URI from which
   *   retrieve the JAR file, the data structure containing the sequence, 
   *   structure and colour map values, and the ID of an element into which
   *   the applet should be placed.
   * @constructs
   * @param {String} [jarUri] the URI of the VARNA applet JAR
   * @param {Object} [data] the object containing the secondary structure data
   * @param {String|Element} [appletContainer] container element for the applet
   */
  initialize: function( jarUri, data, appletContainer ) {
    this._jarUri          = jarUri;
    this._data            = data;
    this._appletContainer = $(appletContainer);

    this._buildMarkup();
    this._selector.observe( "change", this._changeMap.bindAsEventListener( this ) );
  },

  //----------------------------------------------------------------------------
  //- methods ------------------------------------------------------------------
  //----------------------------------------------------------------------------

  // No public methods

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * Builds the required markup for the applet and the colour map chooser.
   *
   * @private
   */
  _buildMarkup: function() {
    
    // build the colour map chooser markup
    var label = new Element( "span" )
                  .update( "Choose the colouring scheme for the secondary structure:" );
    var form = new Element( "form" );
    this._selector = new Element( "select" );
    form.appendChild( this._selector );
    
    var map;
    this._ssOptions.each( function( option, index ) {
      var opt = new Element( "option", { value: option.value } )
                  .update( option.label );
      if ( map === undefined ) {
        opt.selected = "selected";
        map = opt.value;
      }
      this._selector.appendChild( opt );
    }.bind( this ) );

    this._appletContainer.update( label );
    this._appletContainer.appendChild( form );

    // fill in the blanks in the template and add the resulting markup to the DOM
    var t = new Template( this._appletTemplateString );
    var v = {
      jarUri:    this._jarUri,
      structure: this._data.reference_structure,
      sequence:  this._data.reference_sequence
    };

    this._appletContainer.insert( t.evaluate( v ) );

    // set a custom colour map in the applet
    this._applet = $("VARNA");
    this._applet.runScript( 'setCustomColorMap([(-1.00,#FFFFFF),(-0.64,#4747FF),(0.00,#1CFF47),(0.19,#FFFF47),(0.64,#FF4747),(0.82,#B64747)])' );
    this._setColorMap( map );
  },

  //----------------------------------------------------------------------------
  /**
   * Changes the colour map to the one currently selected in the colour map
   * selector.
   *
   * @private
   */
  _changeMap: function() {
    var newMap = this._selector.value;
    this._setColorMap( newMap );
  },

  //----------------------------------------------------------------------------
  /**
   * Sets the colour map in the applet. Also disables the currently selected
   * option in the selector.
   *
   * @private
   * @param {String} newMap the name of the new colour map
   */
  _setColorMap: function( newMap ) {

    this._selector.select("option").each( function( opt ) {
      opt.disabled = "";
    } );

    var values = this._data[newMap].join(",");
    this._applet.runScript( 'setValues(['+values+'])' );

    this._selector.select("option[value='"+newMap+"']").first().disabled = "disabled";
  }

  //----------------------------------------------------------------------------

} );
