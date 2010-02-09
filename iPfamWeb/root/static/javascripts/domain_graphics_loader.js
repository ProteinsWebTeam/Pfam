
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, $$, window, Template, Class, Hash, Effect, Ajax, confirm, 
         console, PfamGraphic */

// spoof a console, if necessary, so that we can run in IE without having
// to entirely disable debug messages
if ( ! window.console ) {
  window.console     = {};
  window.console.log = function() {};
}  

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A javascript object to control the loading of domain graphics.
// 
// jt6 20090901 WTSI
//
// $Id: domain_graphics_loader.js,v 1.1 2010-01-08 14:06:59 pg6 Exp $
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

var DomainGraphicsLoader = Class.create( {
  /**
   * @lends DomainGraphicsLoader#
   * @author John Tate
   */

  /**
   * The template for the message for the confirmation dialogue.
   */
  _msg :new Template( "You are about to load #{num} domain graphics, " +
                      "which may take some time\n\n" +
                      "Are you sure you want to continue ?" ),

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * @class 
   * A javascript object to control the loading of Pfam graphics.
   *
   * @description Builds a new <code>DomainGraphicsLoader</code>.
   * @param {String} uri the URI to poll for the next set of graphics
   * @param {Object} layoutObject the object that defines the graphics to be
   *   drawn. This usually comes out of the <code>LayoutManager</code> on the
   *   server side.
   * @param {Object} [coloursObject] an object that defines the set of
   *   pre-assigned colours. This is used when rendering the set of domain
   *   graphics for all sequences with a given architecture. 
   */
  initialize: function( loadUri, layoutObject, coloursObject, baseUrl ) {
    this._uri             = loadUri;       // uri to hit for the next block of graphics
    this._layout          = layoutObject;  // the description of the graphics
    this._assignedColours = coloursObject; // previously assigned colours, so that 
                                           // the next block of graphics have the same
                                           // set of colours as the original set
    this._baseUrl         = baseUrl;       // stub to prepend to URLs on the graphic

    // if there are more than "_confirmNum" sequences for the architecture, we'll
    // ask the user if they really want to render them all
    this._confirmNum = 200; // TODO hard-coded for now; make it settable

    this._pg = new PfamGraphic();

    if ( this._baseUrl ) {
      this._pg.setBaseUrl( this._baseUrl );
    }

    if ( this._layout !== "undefined" ) {
      this._addDomainGraphics();
      this._addListeners();
    }
  },

  //----------------------------------------------------------------------------
  //- methods ------------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * Draws the domain graphics according to the supplied layout.
   * 
   * @private
   */
  _addDomainGraphics: function() {
    this._layout.each( function( sequence ) {
      this._pg.setSequence( sequence );
      this._pg.setParent( $("row" + sequence.metadata.identifier).down(".pgholder") );
      this._pg.render();
    }.bind(this) );
  },

  //----------------------------------------------------------------------------
  /**
   * Adds the event listeners that listen for clicks on the "show/hide" links.
   * 
   * @private
   */
  _addListeners: function() {

    $$(".revealLink").each( function(link) {
      link.observe( "click", function( e ) {
        var target = e.element();
        this._reveal( target, target.up("div").down(".domainArch") );
      }.bind(this) );
    }.bind(this) );

    $$(".loadLink").each( function(link) {
      link.observe( "click", function( e ) {
        this._loadDomains( e.element() );
      }.bind(this) );
    }.bind(this) );

  },

  //----------------------------------------------------------------------------
  /**
   * Loads the domain graphics for all sequences with a given architecture.
   * 
   * @private
   * @param {Element} loadLink the "show/hide" link element
   */
  _loadDomains: function( loadLink ) {

    var graphicRow = loadLink.up("div.graphicRow");
    var numSeqs    = graphicRow.retrieve("count");

    // only ask for confirmation if there are 50 or more sequences to load
    var continueLoad = ( numSeqs >= this._confirmNum ) ? confirm( this._msg.evaluate( { num: numSeqs } ) ) : true;
    if ( ! continueLoad ) {
      return;
    }

    loadLink.up().hide();
    loadLink.up("div").down(".loading").show();
    loadLink.up("div").down(".revealLink").up().show();

    var options = { evalScripts: true,
                    method: 'post' };

    // console.log( "DGL._loadDomains: setting up ac: ", this._assignedColours );
    if ( this._assignedColours !== undefined ) {
      options.parameters = {};
      options.parameters.ac = encodeURI( new Hash(this._assignedColours).toJSON() );
      // console.log( "DGL._loadDomains: options now: ", options );
    }

    // console.log( "DGL._loadDomains: checking for auto_arch" );
    var auto_arch = graphicRow.retrieve("arch");
    if ( auto_arch !== undefined ) {
      options.parameters.arch = auto_arch;
      // console.log( "DGL._loadDomains: auto_arch: %d", auto_arch );
    }

    // and actually fire off a request to load the new graphics
    var u = new Ajax.Updater(
      loadLink.up("div").down( "div.domainArch"),
      this._uri,
      options );
  },

  //----------------------------------------------------------------------------
  /**
   * Shows or hides the specified element. If the element is visible (as
   * determined by the result of <code>element.visible()</code>, it will be 
   * hidden, and vice versa. Also modifies the text in the switch element, 
   * converting "Show" to "Hide" or vice versa.
   *
   * @private
   * @param {Element} toggleSwitch the switch element
   * @param {Element} target the element to be shown/hidden
   */
  _reveal: function( toggleSwitch, target ) { 

    if ( target.visible() ) {
      Effect.BlindUp( target, { duration: 0.1 } );
      toggleSwitch.update( toggleSwitch.innerHTML.sub( "Hide", "Show" ) );
    } else {
      Effect.BlindDown( target, { duration: 0.1 } );
      toggleSwitch.update( toggleSwitch.innerHTML.sub( "Show", "Hide" ) );
    }

  }

  //----------------------------------------------------------------------------

} );

