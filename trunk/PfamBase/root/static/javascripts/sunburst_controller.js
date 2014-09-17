
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, Class, console, Hash, document, window, Element, Sunburst, Draggable,
         Ajax, Updater, Control, $R */
/*jslint nomen: true, sloppy: true, vars: true, white: true */

// spoof a console, if necessary, so that we can run in IE (<8) without having
// to entirely disable debug messages
if ( ! window.console ) {
  window.console     = {};
  window.console.log = function() {};
}  

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A javascript library to handle the inclusion of a sunburst tree diagram into
// the Rfam family page.
//
// jt6 20120625 WTSI
//
// $Id$
//
// Copyright (c) 2012: Genome Research Ltd.
// 
// Authors: John Tate (jt6@sanger.ac.uk)
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

var SunburstController = Class.create( {
  /**
   * @lends SunburstController#
   * @author John Tate
   */
  //----------------------------------------------------------------------------
  //- class variables ----------------------------------------------------------
  //----------------------------------------------------------------------------

  /**
   * The Sunburst object itself
   *
   * @private
   */
  _sunburst: null,

  /**
   * The Scriptaculous Slider object
   *
   * @private
   */
  _slider: null,

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * @class
   * A javascript class for setting up and wiring in a sunburst tree diagram
   * into an Rfam family page.
   * </p>
   *
   * <h2>Synopsis</h2>
   *
   * <code><pre>
   * var sc = new SunburstController( baseURL, treeData );</pre>
   * </code>
   */

  /**
   * Constructor. <b>Note</b> that the JSON-format tree definition must be
   * passed in as a variable rather than as a literal. The tree is loaded
   * using a factory class that expects to read it from a variable...
   *
   * @param baseURL  the URL for the family to which this sunburst belongs
   * @param treeData the JSON string defining the species tree
   * @param db       used to determine which method of returning the
   *                 alignment should be used, as well as as the name of 
   *                 the tip style that will be used by Prototip when 
   *                 building tool tips
   */
  initialize: function( baseURL, treeData, db ) {

    // "constants" for identifying which method to use in response to mouse
    // clicks
    this._ALIGN = 1;
    this._FASTA = 2;

    this._baseURL = baseURL;
    this._db = db || "pfam";

    // the tree itself
    this._buildTree( treeData );

    // build the nodes and render the tree as soon as possible, before wiring
    // in the controls
    this._sunburst.build()
                  .draw();

    this._wireUIControls();
  },

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * Builds the Sunburst object itself and the Scriptaculous Slider object that
   * is used to set the scale.
   *
   * @private
   */
  _buildTree: function( treeData ) {

    // build the sunburst
    this._sunburst = new Sunburst( { parent:        "sunburst", 
                                     subTreeParent: "sunburstControlsContent", 
                                     tree:          treeData,
                                     tipStyle:      this._db } );
    // build scale slider
    this._slider = new Control.Slider( 
      $("sliderScale").down('.handle'),  // handle element
      $("sliderScale"),                  // track element
      {
        range: $R(20, 50),
        values: [ 20, 25, 30, 35, 40, 45, 50 ],
        sliderValue: 35,
        onSlide: function(value) {
          this._sunburst.setScale( value )
                        .draw();
        }.bind(this),
        onChange: function(value) {
          this._sunburst.setScale( value )
                        .draw();
        }.bind(this)
      }
    );
 
  },

  //----------------------------------------------------------------------------
  /**
   * Wires in the various UI controls.
   *
   * @private
   */
  _wireUIControls: function() {

    // wire up the "weight by..." buttons
    $("weightSeq").observe( "change", function() {
      this._sunburst.weightByNumSeq()
                    .draw();
    }.bind(this) );
    $("weightSpecies").observe( "change", function() {
      this._sunburst.weightByNumSpecies()
                    .draw();
    }.bind(this) );

    // make the sunburst controls draggable and translucent
    var d = new Draggable( "sunburstControls" );
    $("sunburstControls").setOpacity(0.85);

    // show/hide the sunburst controls when the toggle button is clicked
    $("sunburstControlsToggle").observe("click", function() {
      this._toggleTools( "sunburstControlsToggle", "sunburstControlsContent" );
    }.bind(this) );
  
    // a switch to store and then align sequences
    $("sunburstAlignSelectionSwitch").observe( "click", this._storeSequences.bind( this, this._ALIGN ) );
    $("sunburstDLSelectionSwitch").observe(    "click", this._storeSequences.bind( this, this._FASTA ) );

    // a switch to clear the current selection
    $("sunburstClearSelection").observe( "click", function() {
      this._sunburst.clearSelection()
                    .draw();
    }.bind(this) );

    // register a listener for changes to the sunburst selections
    document.observe( "sunburst:selectionChange", function( e ) {
      var numSeq     = e.memo.numSequences,
          numSpecies = e.memo.numSpecies;
      $("sunburstSequencesCount").update( numSeq );
      $("sunburstSpeciesCount").update( numSpecies );
      if ( numSeq == 0 && numSpecies == 0 ) {
        $("sunburstSelectionCount").hide();
      } else {
        $("sunburstSelectionCount").show();
      }
      if ( numSeq == 0 || numSeq > 1000 ) {
        $("sunburstAlignSelectionSwitch").addClassName( "disabled" );
        this._alignmentDisabled = true;
      } else {
        $("sunburstAlignSelectionSwitch").removeClassName( "disabled" );
        this._alignmentDisabled = false;
      }
    } );

    this._alignmentDisabled = false;
  },

  //----------------------------------------------------------------------------

  clearSelection: function() {
    this._sunburst.clearSelection()
                  .draw();
  },

  //----------------------------------------------------------------------------

  _toggleTools: function( sToggle, sContent ) {
    var toggle  = $(sToggle),
        content = $(sContent);

    if ( content.visible() ) {
      content.hide();
      toggle.update( "Show" );
    } else {
      content.show();
      toggle.update( "Hide" );
    }
  },

  //----------------------------------------------------------------------------
  //- methods for storing sequence accessions ----------------------------------
  //----------------------------------------------------------------------------
  
  _storeSequences: function( endpoint ) {

    var accessions, accessionsString, r, fasta;

    if ( this._alignmentDisabled ) {
      // console.debug( "SunburstController._storeSequences: too many sequences; done" );
      return;
    }

    $("sunburstSelectionTools").hide();
    $("sunburstSpinner").show();

    // unregister the TabPage methods that keep track of active AJAX calls, 
    // otherwise the "Loading 1 component" message keeps popping up during 
    // polling, which makes the whole page contents bounce up and down
    Ajax.Responders.unregister( tabPage.responders );

    accessions       = this._sunburst.getSelectedSeqAccs();
    accessionsString = JSON.stringify( accessions );

    // bail unless there are selected species
    if ( accessions.size() < 1 ) {
      // console.warn( 'SunburstController._storeSequences: no accessions to store' );
      return;
    }

    // get rid of any error messages that we might have
    $("sunburstErrors").hide();

    // store the list of accessions. We pass a flag to the callback to tell it 
    // whether to generate the alignment as Stockholm or FASTA.
    fasta = ( endpoint == this._FASTA ) ? 1 : 0;

    r = new Ajax.Request( 
      this._baseURL + "/sunburst/accessions",
      {
        method: "post",
        postBody: accessionsString,
        contentType: "application/json",
        onSuccess: this._alignSequences.bind(this, fasta),
        onFailure: this._storeFailure.bind(this)
      }
    );

    // console.debug( "SunburstController._storeAccessions: store request submitted" );
  },

  //----------------------------------------------------------------------------

  _storeFailure: function() {
    $("sunburstErrors").update( "There was a problem storing your sequences" )
                       .show();
  },

  //----------------------------------------------------------------------------

  _resetSpinner:function() {
    $("sunburstSpinner").update( "Storing selection&hellip;" )
                        .hide();
    $("sunburstSelectionTools").show();

    // re-register the TabPage methods that keep track of active AJAX calls
    Ajax.Responders.register( tabPage.responders );
  },

  //----------------------------------------------------------------------------
  //- methods for aligning stored sequences ------------------------------------
  //----------------------------------------------------------------------------

  _alignSequences: function( generateFasta, response ) {

    $("sunburstSpinner").update( "Aligning sequences&hellip;" );
    setTimeout( this._resetSpinner.bind(this), 2000 );

    var jobId = response.responseJSON.jobId,
        acc   = response.responseJSON.acc,
        r;
        
    if ( this._db == "pfam" ) {
      // Pfam shows the alignment in a pfamviewer window
      popUp( '/family/' + acc + '/alignment/build/?jobId=' + jobId, 'console', 800, 800, 'selectedSeqsWin' );
    } else {
      // Rfam just hands back the alignment
      r = new Ajax.Request( 
        '/family/' + acc + '/sunburst/alignment/' + jobId + ( generateFasta ? '/fasta' : '' ),
        {
          method: "post",
          contentType: "application/json",
          onSuccess: this._pollForAlignment.bind(this),
          onFailure: this._alignmentSubmissionFailed.bind(this)
        }
      );
    }

  },

  //----------------------------------------------------------------------------

  _pollForAlignment: function( response ) {
    // console.debug( "SunburstController._pollForAlignment: job submission succeeded" );

    this._alignmentURI = response.getHeader('Location');

    this._updater = new Updater(
      this._alignmentURI,
      {
        method:         'get',
        requestHeaders: { 'Content-type': 'application/json' },
        parameters:     { poll: 1 },
        frequency:      1,
        decay:          1.2,
        on204:          this._alignmentRunning.bind(this),
        onSuccess:      this._alignmentSucceeded.bind(this),
        onFailure:      this._alignmentFailed.bind(this)
      }
    );
  
    // console.debug( "SunburstController._pollForAlignment: started updater; polling" );
  },

  //----------------------------------------------------------------------------

  _alignmentSubmissionFailed: function( response ) {
    $("sunburstErrors").update( "There was a problem submitting your sequences for alignment" )
                       .show();
  },

  //----------------------------------------------------------------------------
  
  _alignmentRunning: function( response ) {
    // console.debug( 'SunburstController._align204: job %s, status 204; no results; polling further',
    //   this._jobId );
  },

  //----------------------------------------------------------------------------

  _alignmentSucceeded: function( response ) {
    // console.debug( 'SunburstController._alignmentSucceeded: alignment generated: ', response );
    this._updater.stop();

    $("sunburstSpinner").update( "Saving alignment&hellip;" );
    setTimeout( this._resetSpinner.bind(this), 2000 );

    // console.debug( 'SunburstController._alignmentSucceeded: setting window.location = "%s"',
    //                this._alignmentURI );

    var w = window.open( this._alignmentURI, "_blank", "width=300,height=300" );
  },

  //----------------------------------------------------------------------------

  _alignmentFailed: function( response ) {
    // console.debug( 'SunburstController._alignmentFailed: alignment failed' );
    this._updater.stop();
    setTimeout( this._resetSpinner.bind(this), 1000 );
    $("sunburstErrors").update( "There was a problem aligning your sequences" )
                       .show();
  },

  //----------------------------------------------------------------------------

} );

