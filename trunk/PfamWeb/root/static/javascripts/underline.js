
//------------------------------------------------------------------------------
//- Preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, Class, console, Element */

//------------------------------------------------------------------------------
//- Class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// a class to underline domains in the graphic
//
// jt6 20090803 WTSI
//
// $Id: underline.js,v 1.1 2009-09-04 13:01:38 jt6 Exp $
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

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------

  initialize: function( /* PfamGraphic object */ pg ) {

    // add the div that we'll use as the underline
    if ( $("underline") ) {
      // don't keep creating new ones though
      this._underlineDiv = $("underline");
    } else {

      // add a "cleaner" div first, to make sure that the underline appears
      // below the domain graphic
      pg.getParent().insert( { bottom: new Element( "div", { "class": "cleaner" } ) } );

      // and now build the underline itself
      this._underlineDiv = new Element( "div", { id: "underline",
                                                  style: "display: none" } );
      pg.getParent().insert( { bottom: this._underlineDiv } );
    }

    // get the x-offset for the canvas element, so that we can calculate the
    // correct position for the underline
    this._canvasOffset = pg.getCanvas().cumulativeOffset().left;

    // get the data structure that stores the area information
    var areaStructures = pg.getAreas();
    this._areasHash = areaStructures[1];
  
    // add the listeners for the mouse events
//    for ( var linkId in this._areasHash ) {
    this._areasHash.keys().each( function( linkId ) {
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

  // shows the underline, positioned under the appropriate element in the 
  // domain graphic
   
  _showLine: function( linkId, e ) {

    if ( ! linkId ) {
      this._underlineDiv.hide();
      return;
    }

    var start = parseInt( this._areasHash.get( linkId ).coords[0], 10 ),
        end   = parseInt( this._areasHash.get( linkId ).coords[2], 10 ),
        l = this._canvasOffset + start,
        w = end - start;

    this._underlineDiv.setStyle( { left:  l + "px",
                                    width: w + "px" } )
                       .show();
  },

  //----------------------------------------------------------------------------

  // hides the underline

  _hideLine: function( e ) {
    this._underlineDiv.hide();
  }

  //----------------------------------------------------------------------------

} );

