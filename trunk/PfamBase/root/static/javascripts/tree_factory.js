
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, Class, console, Element, Hash, document, window, TreeFactory,
         setTimeout, clearTimeout, Draggable, G_vmlCanvasManager, Tip */

//------------------------------------------------------------------------------
//- file -----------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Two classes used by the <code>Sunburst</code> class to convert a JSON 
// representation of a tree into an object structure.
//
// $Id$
//
// Copyright (c) 2010: Genome Research Ltd.
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

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A class representing a node in the object structure of a sunburst tree.

var SunburstNode = Class.create( {
  /**
   * @lends SunburstNode#
   * @author John Tate
   * @author Rob Finn
   */
  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * @class
   * A class representing a node in the object structure of a sunburst tree.
   * </p>
   *
   * <p>
   * Not intended to be used in isolation. Used by the <code>TreeFactory</code>.
   * </p>
   */
  initialize: function( nodeData ) {
    this.numSequences = 0;
    this.numDomains   = 0;
    this.numSpecies   = 0;
    this.nodeName     = "";
    this.id           = null;
    this.children     = [];
    this.parentNode   = {};
    Object.extend( this, nodeData );
  },

  toString: function() { 
    return "SunburstNode object (" + this.nodeName + ") with " + this.children.length + " children";
  }

} );

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A simple factory class for converting a JSON representation of a tree 
// into an object structure.

var TreeFactory = function( treeData, parentNode ) {
  /**
   * @lends SunburstNode#
   * @author John Tate
   * @author Rob Finn
   */
  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * @class
   * A simple factory class for converting a JSON representation of a tree 
   * into an object structure.
   * </p>
   *
   * <h2>Synopsis</h2>
   *
   * <code><pre>
   * var tree = TreeFactory( jsonTreeData );</pre>
   * </code>
   */
  var node = new SunburstNode( {
    numSequences: treeData.numSequences,
    numDomains:   treeData.numDomains,
    numSpecies:   treeData.numSpecies,
    nodeName:     treeData.node || "Unknown",
    id:           treeData.id,
    parentNode:   parentNode
  } );

  if ( treeData.children !== undefined ) {
    treeData.children.each( function( child ) {
      node.children.push( TreeFactory( child, node ) );
    }.bind(this) );
  }

  return node;
};

//------------------------------------------------------------------------------

