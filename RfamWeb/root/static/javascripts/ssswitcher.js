
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, $w, Class, console,  Event, Element, document, window */

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A simple class for controlling the secondary structure image comparison tool.
//
// jt6 20100323 WTSI
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

var SSSwitcher = Class.create( {
  /**
   * @lends SSSwitcher#
   * @author John Tate
   */

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * @class 
   * A simple class for controlling the secondary structure image comparison 
   * tool.
   * </p>
   * 
   * <h2>Synopsis</h2>
   *
   * <code><pre>
   * var members = [
   *   { acc: "RF01050", id:  "Sacc_telomerase" },
   *   { acc: "RF00025", id:  "Telomerase-cil"  },
   *   { acc: "RF00024", id:  "Telomerase-vert" }
   * ]; 
   *
   * var ssSwitcher;
   * document.observe( "dom:loaded", function() { 
   *   ssSwitcher = new SSSwitcher( members,
   *                                "switcherContainer",
   *                                "http://rfam.sanger.ac.uk/family/image" );
   * } );
   * </pre><code>
   *
   * <h2>Usage</h2>
   *
   * <p>
   *   No user-serviceable parts. Just instantiate the object and it will 
   *   build the required markup and drop it into the specified container 
   *   element.
   * </p>
   *
   * <p>
   *
   * @description Builds a new SSSwitcher object. The contents of each 
   *   drop-down is given in the data structure specified by the first argument:
   *   <code><pre>
   *     var members = [
   *       { acc: "RF01050", id:  "Sacc_telomerase" },
   *       { acc: "RF00025", id:  "Telomerase-cil"  },
   *       { acc: "RF00024", id:  "Telomerase-vert" }
   *     ];</pre>
   *   </code>
   * @constructs
   * @param {Object}         [members]    the contents of the drop-down lists
   * @param {String|Element} [container]  the element that will hold the markup
   * @param {String}         [imgUrlRoot] root of the URL for the SS images
   */
  initialize: function( members, container, imgUrlRoot ) {
    this._members     = members;
    this._containerEl = $(container);
    this._imgUrlRoot  = imgUrlRoot;

    this._imgCache = {};

    this._buildMarkup();

    this._lSelectEl.observe( "change", this._selectorChange.bindAsEventListener( this ) );
    this._rSelectEl.observe( "change", this._selectorChange.bindAsEventListener( this ) );
  },

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------

  /**
   * Handler for <code>onChange</code> events from the select elements.
   * @param {Event} [e] <code>onChange</code> event object
   */
  _selectorChange: function( e ) {
    var selectorEl          = e.findElement("select"),
        selectorElValue     = selectorEl.value,
        selectorContainerEl = selectorEl.up("div"),

        otherSelectorEl = selectorEl.up("div")
                                    .siblings()
                                    .first()
                                    .down("select");
                                    
    otherSelectorEl.select("option").each( function( optEl ) {
      optEl.disabled = optEl.value == selectorElValue ? "disabled" : "";
    } );

    this._addImg( selectorContainerEl, selectorElValue );
  },

  //----------------------------------------------------------------------------
  /**
   * Adds an image to the specified parent element. The image is specified by
   * an Rfam accession.
   * @param {Element} [parentEl] element to which the image should be added
   * @param {String}  [acc]      Rfam accession of the family whose image 
   *  should be added
   */
  _addImg: function( parentEl, acc ) {
    if ( this._imgCache[acc] ) {
      this._addOldImg( parentEl, acc );
    } else {
      this._addNewImg( parentEl, acc );
    }
  },

  //----------------------------------------------------------------------------
  /**
   * Adds a cached image to the specified parent element.
   * @param {Element} [parentEl] element to which the image should be added
   * @param {String}  [acc]      Rfam accession of the family whose image 
   *  should be added
   */
  _addOldImg: function( parentEl, acc ) {
    var img = this._imgCache[acc];

    this._removeImgs( parentEl );
    
    parentEl.appendChild( img );
    this._imgCache[acc] = img;
  },

  //----------------------------------------------------------------------------
  /**
   * Adds a new, uncached image to the specified parent element. Once added, the
   * image will be cached.
   * @param {Element} [parentEl] element to which the image should be added
   * @param {String}  [acc]      Rfam accession of the family whose image 
   *  should be added
   */
  _addNewImg: function( parentEl, acc ) {

    this._removeImgs( parentEl );
    
    var imgUrl = this._imgUrlRoot + "?type=cons&amp;acc=" + acc,
        img = new Element( "img", { id:  "img_" + acc,
                                    src: imgUrl } );

    this._imgCache[acc] = img;
    parentEl.appendChild( img );
  },

  //----------------------------------------------------------------------------
  /**
   * Removes all image elements from the specified parent element.
   * @param {Element} [parentEl] parent element to be emptied of images
   */
  _removeImgs: function( parentEl ) {
    parentEl.select("img").each( function( child ) {
      parentEl.removeChild( child );
    } );
  },
  
  //----------------------------------------------------------------------------
  /**
   * Builds the markup required for the control. This looks something like:
   * <code><pre>
   * <div id="lSwitcher"
   *      class="SSSwitcher">
   *   <select>
   *     <option value="RF01050">Sacc_telomerase</option>
   *     <option value="RF00025">Telomerase-cil</option>
   *     <option value="RF00024">Telomerase-vert</option>
   *   </select>
   *   <img id="img_RF01050" 
   *        src="http://rfam.sanger.ac.uk/family/image?type=cons&amp;amp;acc=RF01050" />
   * </div>
   * <div id="rSwitcher" 
   *      class="SSSwitcher">
   *   <select>
   *     <option value="RF01050">Sacc_telomerase</option>
   *     <option value="RF00025">Telomerase-cil</option>
   *     <option value="RF00024">Telomerase-vert</option>
   *   </select>
   *   <img id="img_RF00024" 
   *        src="http://rfam.sanger.ac.uk/family/image?type=cons&amp;amp;acc=RF00024" />
   * </div>
   * <div class="empty cleaner"></div></pre>
   * </code>
   */
  _buildMarkup: function() {
    this._lDivEl = new Element( "div", { "id": "lSwitcher", "class": "SSSwitcher" } );
    this._rDivEl = new Element( "div", { "id": "rSwitcher", "class": "SSSwitcher" } );
    
    this._lSelectEl = new Element( "select" );
    this._rSelectEl = new Element( "select" );

    this._lDivEl.appendChild( this._lSelectEl );
    this._rDivEl.appendChild( this._rSelectEl );

    this._members.each( function( member, index ) {
      var lOpt = new Element( "option", { "value": member.acc } ).update( member.id ),
          rOpt = new Element( "option", { "value": member.acc } ).update( member.id );

      if ( index === 0 ) {
        lOpt.selected = "selected";
        rOpt.disabled = "disabled";
        this._addImg( this._lDivEl, member.acc );
      }

      if ( index == 1 ) {
        rOpt.selected = "selected";
        lOpt.disabled = "disabled";
        this._addImg( this._rDivEl, member.acc );
      }

      this._lSelectEl.appendChild( lOpt );
      this._rSelectEl.appendChild( rOpt );
    }, this );

    this._containerEl.appendChild( this._lDivEl );
    this._containerEl.appendChild( this._rDivEl );
    this._containerEl.appendChild( new Element( "div", { "class": "empty cleaner" } ) );
  }

  //----------------------------------------------------------------------------
  
} );

