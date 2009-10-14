
//------------------------------------------------------------------------------
//- Preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, Class, console, Element */

// spoof a console, if necessary, so that we can run in IE without having
// to entirely disable debug messages
if ( ! window.console ) {
  window.console     = {};
  window.console.log = function() {};
}  

//------------------------------------------------------------------------------
//- Class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// a class to underline domains in the graphic
//
// jt6 20090803 WTSI
//
// $Id: underline.js,v 1.3 2009-10-14 15:46:47 jt6 Exp $
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

var Underliner = Class.create( {
  /**
   * @lends Underliner#
   * @author John Tate
   */

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * @class
   * A simple object to mark highlighted domains in a Pfam domain graphic.
   *
   * @description Builds a new <code>Underliner</code>.
   * @param {Object} pg the <code>PfamGraphic</code> object to highlight.
   */
  initialize: function( /* PfamGraphic object */ pg ) {

    this._pg = pg;
    this._pgParent = pg.getParent();

    // add the div that we'll use as the underline
    if ( $("underline") ) {
      // don't keep creating new ones though
      this._underlineDiv = $("underline");
    } else {

      // add a "cleaner" div first, to make sure that the underline appears
      // below the domain graphic
      this._pgParent.insert( { bottom: new Element( "div", { "class": "cleaner" } ) } );

      // and now build the underline itself
      this._underlineDiv = new Element( "div", { id: "underline",
                                                 style: "display: none" } );
      this._pgParent.insert( { bottom: this._underlineDiv } );
    }
    
    // get the data structure that stores the area information
    var areaStructures = pg.getAreas();
    this._areasHash = areaStructures[1];
  
    // add the listeners for the mouse events
    this._areasHash.keys().each( function( linkId ) {
      // console.log( "linkId: %s", linkId );
      if ( ! $(linkId ) ) {
        return;
      }

      // add the listeners to the row that enclosed the links, so that the 
      // mouseovers work at row level      
      $(linkId).up("tr").observe( "mouseover", this._showLine.bind( this, linkId ) );
      $(linkId).up("tr").observe( "mouseout",  this._hideLine.bind( this ) );
    }.bind( this ) );
  },

  //----------------------------------------------------------------------------
  //- methods ------------------------------------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //- public methods -----------------------------------------------------------
  //----------------------------------------------------------------------------

  // no public methods

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * Shows the underline, positioned under the appropriate element in the 
   * domain graphic.
   *
   * @param {String} linkId the ID of the &quot;<code>&lt;area&gt;</code>&quot;
   *                        to be highlighted.
   * @param {Object} e the <code>Event</code> object.
   */
  _showLine: function( linkId, e ) {

    if ( ! linkId ) {
      this._hideLine();
      return;
    }

    // get the x-offset for the canvas element, so that we can calculate the
    // correct position for the underline
    var canvasOffset = this._pg.getCanvas().cumulativeOffset().left;
    
    var start = parseInt( this._areasHash.get( linkId ).coords[0], 10 ),
        end   = parseInt( this._areasHash.get( linkId ).coords[2], 10 ),
        l = canvasOffset + start - this._pgParent.scrollLeft,
        w = end - start;

    this._underlineDiv.setStyle( { left:  l + "px",
                                   width: w + "px" } )
                      .show();
  },

  //----------------------------------------------------------------------------
  /**
   * Hides the underline.
   */
  _hideLine: function() {
    this._underlineDiv.hide();
  }

  //----------------------------------------------------------------------------

} );

