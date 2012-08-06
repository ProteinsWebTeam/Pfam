
//------------------------------------------------------------------------------
//- Preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, $$, Class, console, Element, Hash, document, Updater */

//------------------------------------------------------------------------------
//- Class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// a class to handle the construction and behaviour of the Rfam sequence
// search results page.
//
// jt6 20120312 WTSI
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

var Results = Class.create( {
  /**
   * @lends Results#
   * @author John Tate
   */

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * @class
   * A wrapper class to coordinate the construction and behaviour of the single
   * sequence search results page.
   *
   * @description Builds the content and behaviour of the search results page
   * @param {String} job identifier (a UUID)
   * @param {Object} simple object with the page configuration
   */
  initialize: function( jobId, config ) {
    // console.log( "Results.initialize: setting up" );

    // store the job ID and the configuration object
    this._jobId  = jobId;
    this._config = config;

    // error message templates
    this._errorTemplate = new Template( "Job #{jobId} failed" );

    // get a new template engine
    this._template = new EJS( { url: this._config.templateUrl } );

    // retrieve and check the handles to the various page elements
    this._checkElements(); 

    // set up the updater
    this._updater = new Updater(
      this._config.resultsUrl,
      {
        method:      'get',
        requestHeaders: { 'Content-type': 'application/json' },
        frequency:   1,
        decay:       1.2,
        on202:       this.on202.bind( this ),
        onSuccess:   this.onSuccess.bind( this ),
        onFailure:   this.onFailure.bind( this )
      }
    );

    // console.log( "Results.initialize: done setting up" );
  },  

  //----------------------------------------------------------------------------
  //- public methods -----------------------------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //- methods ------------------------------------------------------------------
  //----------------------------------------------------------------------------

  // these are all callbacks for the AJAX requests...

  /**
   * We need to have a method for this status, even though we don't get any
   * information from the server in the response, otherwise the Updater sees
   * status "2XX" and considers it to be "success"
   */
  on202: function( response ) {
    // console.log( "Results.on202: job %s, status 202; no results; polling further", 
    //   this._jobId );
  },

  //-----------------------------------
  /**
   * All parts of the job succeeded; insert the job results into the page
   *
   * @param {Object} response AJAX response object
   */
  onSuccess: function( response ) {
    // console.log( "Results.onSuccess: job %s, status 200; job done", this._jobId );

    this._updater.stop();      // stop the updater polling for results
    this._response = response; // store the response for later
    this._updateSequence();    // update the sequence in the key

    if ( response.responseJSON.numHits === undefined ) {
      this._noResults();
    } else {
      this._showResults();
    }
  },

  //-----------------------------------
  /**
   * Handles a job failure response.
   *
   * @param {Object} response AJAX response object
   */
  onFailure: function( response ) {
    // console.log( "Results.onFailure: failure response for job %s; stopping polling", this._jobId );

    this._updater.stop();
    this._response = response;

    this._failed();
  },
  
  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------

  /**
   * Handles result page when there are no results to show.
   */
  _noResults: function() {
    // console.log( "Results._noResults: no results to show" );

    // update the summary with the hit count
    this._hitCountEl.update( "There were <strong>no</strong> hits to your search sequence." ); 

    // show and hide stuff, as dictated by their classes in the HTML
    $$(".hideOC").invoke("hide"); // "hide on completion of AJAX call"
    $$(".showOC").invoke("show"); // "show on completion of AJAX call"
    $$(".no" ).invoke("show");    // "no, we have no results"
    $$(".yes").invoke("hide");    // "yes, we have some results"
  },

  //-----------------------------------
  /**
   * Handles result page when there are results to show.
   */
  _showResults: function() {
    // console.log( "Results._showResults: showing results" );

    // use the EJS template to build the results table
    this._template.update( this._resultsEl, this._response.responseJSON );

    // update the summary with the hit count
    var hitCountTemplate = ' \
      <% if ( numHits == 1 ) { %> \
        We found <strong>1</strong> hit to your sequence. \
      <% } else { %> \
        We found <strong><%= numHits %></strong> hits to your sequence. \
      <% } %>';

    var ejs = new EJS( { text: hitCountTemplate } )
      .update( this._hitCountEl, this._response.responseJSON );

    //--------------------

    // add listeners to the show/hide buttons
    $$("td.showSwitch").each( function( toggleSwitch ) {
      toggleSwitch.observe( "click", this._toggleAlignment.bindAsEventListener(this) );
    }.bind( this ) );

    // and something similar for the "show all" links
    $w("showAll hideAll").each( function( link ) {
      $(link).observe( "click", this._showHideAll.bindAsEventListener( this ) );
    }.bind( this ) );

    //--------------------

    // show and hide stuff, as dictated by their classes in the HTML
    $$(".hideOC").invoke("hide"); // "hide on completion of AJAX call"
    $$(".showOC").invoke("show"); // "show on completion of AJAX call"
    $$(".no" ).invoke("hide");    // "no, we have no results"
    $$(".yes").invoke("show");    // "yes, we have some results"
  },

  //-----------------------------------
  /**
   * Handles an error message from the server
   *
   * @private
   */
  _failed: function() {
    // console.log( "Results._failure: showing error message: %s",
    //   this._response.responseJSON.error );

    this._errorsEl.update(this._response.responseJSON.error);

    // show and hide stuff, as dictated by their classes in the HTML
    $$(".hideOC").invoke("hide"); // "hide on completion of AJAX call"
    $$(".showOC").invoke("show"); // "show on completion of AJAX call"
    $$(".no" ).invoke("hide");    // "no, we have no results"
    $$(".yes").invoke("hide");    // "yes, we have some results"
    $$(".both").invoke("hide");   // show both when there are and are not results
    $$(".fail").invoke("show");   // "there was an error"

    // // add error messages
    // if ( ! this._errorsList ) {
    //   var errorHeader = '<h1>Job failures</h1>' +
    //                     '<p>One or more of the searches that you submitted had errors during execution:</p>' +
    //                     '<ul></ul>';
    //   
    //   this._errorsEl.insert( { top: errorHeader } );
    //   
    //   this._errorsList = this._errorsEl.down("ul");
    // } 
    // this._summaryEl.show();
  },

  //-----------------------------------
  /**
   * Updates the "this is your sequence" field in the key.
   */
  _updateSequence: function() {
    // console.log( "Results._updateSequence: updating 'this is your sequence' element" );

    // trim off the FASTA header line from the search sequence
    var plainSequence = this._response.responseJSON.searchSequence;

    // split the sequence into multiple lines
    var ll = this._config.sequence_line_length || 40;
    var formattedSeq  = "";
    
    for ( var i = 0; i < Math.ceil( plainSequence.length / ll ); i++ ) {
      // console.log( '%d: %s', i, plainSequence.substr( i * ll, ll ) );
      formattedSeq += plainSequence.substr( i * ll, ll ) + "<br />";
    }

    this._seqEl.update( formattedSeq );
  },

  //-----------------------------------
  /**
   * Checks the configuration for elements that are used by the class. Tries
   * to convert every item into an element (using "$()") and throws an error
   * if any elements are missing
   *
   * @private
   */
  _checkElements: function() {

    // hit count
    this._hitCountEl = $( this._config.elements.hitCount );
    if ( this._hitCountEl === undefined ) {
      throw( "Error: couldn't find the element to update with the hit count" );
    }

    // the results element
    this._resultsEl = $( this._config.elements.results );
    if ( this._resultsEl === undefined ) {
      throw( "Error: couldn't find the element to update with results" );
    }

    // the plain sequence
    this._seqEl = $( this._config.elements.sequence );
    if ( this._seqEl === undefined ) {
      throw( "Error: couldn't find the element containing the input sequence" );
    }

    // somewhere for error messages
    this._errorsEl = $( this._config.elements.errors );
    if ( this._errorsEl === undefined ) {
      throw( "Error: couldn't find the element to hold error messages" );
    }

    // console.log( "Results._checkElements: all passed checks" );
  },

  //----------------------------------------------------------------------------
  /**
   * Shows or hides a single alignment row.
   *
   * @private
   * @param {Event} e mouse click event on the show/hide button
   */
  _toggleAlignment: function( e ) {
    var cell    = e.findElement("td"),
        btn     = cell.down("span"),
        nextRow = cell.up("tr").next("tr");
    nextRow.toggle();
    btn.innerHTML = nextRow.visible() ? "Hide" : "Show";
  },
  
  //----------------------------------------------------------------------------
  /**
   * Shows or hides all of the alignments in one fell swoop...
   *
   * @private
   * @param {Event} e mouse click event on the "show/hide all" link
   */
  _showHideAll: function( e ) {
    var link = e.findElement(),
        show = link.innerHTML.match( "Show" ),
        tbody = link.up("table").down("tbody");

    tbody.select("tr.alignment").invoke( show ? "show" : "hide" );

    tbody.select("span.btn-inner").each( function(toggle) {
      toggle.innerHTML = show ? "Hide" : "Show";
    } );
  }

  //----------------------------------------------------------------------------

} );
