
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, Class */

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A javascript library for drawing Pfam-style domain graphics in an HTML5
// canvas.
//
// jt6 20090803 WTSI
//
// $Id: image_switcher.js,v 1.1 2009-08-21 10:47:13 jt6 Exp $
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

var ImageSwitcher = Class.create( {
  /**
   * @lends ImageSwitcher#
   * @author John Tate
   */

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * @class
   * A class to manage switching between a series of panels (say, images).
   * The page must contain the following hierarchy of elements:
   * <pre>
   *   parent element
   *     |
   *     +- &lt;img class="switcherControl" rel="next" ...&gt;
   *     |  &lt;img class="switcherControl" rel="prev" ...&gt;
   *     |
   *     +- 'Showing &lt;span class="switcherCurr"&gt;X&lt;/span&gt; of 
   *     |    &lt;span class="switcherTotal"&gt;N&lt;/span&gt; images'
   *     |
   *     +- &lt;div&gt;
   *          |
   *          +- &lt;img class="switcherPanel" ...&gt;
   *             &lt;img class="switcherPanel" ...&gt;
   *             &lt;img class="switcherPanel" ...&gt;
   * </pre>
   * 
   * @description
   * Builds a new ImageSwitcher object. Needs the element that
   * contains the various bits of markup.
   *
   * @constructs
   * @param {String|Element} parentEl Parent element for all markup 
   */
  initialize: function( parentEl ) {
    this._parent = $(parentEl);

    if ( ! this._parent ) {
      throw( "ImageSwitcher: ERROR: couldn't find parent element" );
    }

    this._panels      = this._parent.select( ".switcherPanel" );
    this._currentPanelNumber = 0;

    this._prevControl = this._parent.down( ".switcherControl[rel='prev']" );
    this._nextControl = this._parent.down( ".switcherControl[rel='next']" );
    
    this._currentEl   = this._parent.down( ".switcherCurr" );
    this._totalEl     = this._parent.down( ".switcherTotal" ).update( this._panels.size() );

    this._updateStatus();

    this._addListeners();
  },
  
  //----------------------------------------------------------------------------
  //- methods ------------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * Switches to the next or previous image, as determined by the "next" flag.
   * 
   * @param {boolean} next "true" to move to next image, "false" to move to 
   *                       previous image 
   */
  switchPanel: function( next ) {
  
    var newPanelNumber = this._currentPanelNumber + ( next ? 1 : -1 );
  
    if ( newPanelNumber < 0 || newPanelNumber >= this._panels.size() ) {
      return;
    }
    
    var oldPanel = this._panels[this._currentPanelNumber];
    var newPanel = this._panels[newPanelNumber];
    
    oldPanel.hide();
    newPanel.show();
    
    this._currentPanelNumber = newPanelNumber;

    if ( this._currentPanelNumber < 1 ) {
      this._prevControl.addClassName( "disabled" );
    } else {
      this._prevControl.removeClassName( "disabled" );
    }

    if ( this._currentPanelNumber >= ( this._panels.size() - 1 ) ) {
      this._nextControl.addClassName( "disabled" );
    } else {
      this._nextControl.removeClassName( "disabled" );
    }

    this._updateStatus();    
  },

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * Updates the "X of Y" status message.
   *
   * @private
   */
  _updateStatus: function() {
    if ( this._currentEl ) {
      this._currentEl.update( this._currentPanelNumber + 1 );
    }
  },

  //----------------------------------------------------------------------------
  /**
   * Adds the listeners to the "prev" and "next" images.
   *
   * @private
   */
  _addListeners: function() {

    this._prevControl.observe( "click", function( e ) {
      this.switchPanel( false );
    }.bind( this ) );

    this._nextControl.observe( "click", function( e ) {
      this.switchPanel( true );
    }.bind( this ) );
    
  }
  
  //----------------------------------------------------------------------------

} );
