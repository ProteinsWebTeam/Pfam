
//------------------------------------------------------------------------------
//- Preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, $$, Class, console, Element, Hash, document, PfamGraphic, 
  Underliner, Updater */

//------------------------------------------------------------------------------
//- Class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// a class to handle the construction and behaviour of the Pfam sequence
// search results page.
//
// jt6 20090803 WTSI
//
// $Id: results.js,v 1.4 2009-10-27 14:23:54 jt6 Exp $
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

    // retrieve and check the handles to the various page elements
    this._checkElements(); 

    // set up the updater
    this._updater = new Updater(
      this._config.pollUrl,
      {
        method:     'get',
        frequency:  1,
        decay:      1.2,
        on202:      this.on202.bind( this ),
        onSuccess:  this.onSuccess.bind( this ),
        onFailure:  this.onFailure.bind( this )
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

    // stop the updater for this job
    this._updater.stop();

    // update the page with the results
    $("results").update( response.responseText );
  },

  //-----------------------------------
  /**
   * Handles a job failure response.
   *
   * @param {Object} response AJAX response object
   */
  onFailure: function( jobId, response ) {
    // console.log( "Results.onFailure: failure response for job %s; stopping polling", jobId );

    // stop the updater for this job and remove it from the list of
    // running jobs
    this._updater.stop();

    // hide the "waiting for the results of..." message
    this._loadingEl.hide();

    // add error messages
    if ( ! this._errorsList ) {
      var errorHeader = '<h1>Job failures</h1>' +
                        '<p>One or more of the searches that you submitted had errors during execution:</p>' +
                        '<ul></ul>';
      
      this._errorsEl.insert( { top: errorHeader } );
      
      this._errorsList = this._errorsEl.down("ul");
    } 
    this._summaryEl.show();

    // var msg = this._errorTemplate.evaluate( { jobId: jobId } );
    this._errorsList.insert( { bottom: new Element( "li" ).update( response.responseText ) } );
    this._errorsEl.show();
  },
  
  //----------------------------------------------------------------------------
  /**
   * This is a callback method that's called from the javascript snippet that
   * is loaded under the results tables (from results_table.tt). It's job is
   * to load the results tables into the page and to set up the behaviours 
   * surrounding them (e.g. "show/hide" buttons)
   *
   * @param {Object} resultDetails object with the details of the results and
   *   the layout object for the graphic
   */
  jobDone: function( resultDetails ) {
    // console.log( "Results.jobDone: using result details" );
  
    // hide the spinner
    this._loadingEl.hide();

    // hide all of the alignment rows once the table is built
    this._updateEl.select(".alignment").invoke( "hide" );
    
    // if we're using the gathering threshold, hide the insignificant hits and
    // the front of the table title too
    if ( this._config.options.ga ) {
      $("pfamASummaryI").hide();
      this._updateEl.select(".titlePrefix").invoke( "hide");
    }

    // build the summary text and show it
    // TODO write the summary...
    this._writeSummary( resultDetails.details );
    this._summaryEl.show();

    // add listeners to the show/hide buttons
    $$("td.showSwitch").each( function( toggleSwitch ) {
      toggleSwitch.observe( "click", this._toggleAlignment.bindAsEventListener( this ) );
    }.bind( this ) );

    // and something similar for the "show all" links
    $$("caption span.showHideLink").each( function( link ) {
      link.observe( "click", this._showHideAll.bindAsEventListener( this ) );
    }.bind( this ) );

    // set the widths for the alignment blocks, so that the scroll bars are
    // correctly added
    // $$("#results table.resultTable div.hmmWindow").each( function(a) {
    //   console.log( "setting width to %d for ", alignmentWidth, a );
    //   a.setStyle( { width: alignmentWidth+"px" } );
    // } );
   
    // console.log( "Results.jobDone: results loaded and switched wired in" );
    
    //-----------------------------------
 
    // draw the domain graphics. Hand in the base URL for the links on the image
    this._pg = new PfamGraphic( "dg", resultDetails.layout.first() );
    this._pg.setBaseUrl( this._config.baseUrl );
    this._pg.render();
  
    // set up the underlining
    this._underliner = new Underliner( this._pg );

    // add tooltips to the alignment rows
    $$("div.hmmAlignment").each( function(alignmentDiv) {
      var t = new Tip( alignmentDiv, 
                        $("alignmentKey").cloneNode(true),
                        { title: "Alignment key",
                          style: "pfam" } );
    } );
    
    // console.log( "Results.jobDone: finished adding behaviour to page" );
  },

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------
  /**
   * Checks the configuration for elements that are used by the class. Tries
   * to convert every item into an element (using "$()") and throws an error
   * if any elements are missing
   *
   * @private
   */
  _checkElements: function() {

    // the results element
    this._updateEl = $( this._config.elements.update );
    if ( this._updateEl === undefined ) {
      throw( "Error: couldn't find the element to update with results" );
    }

    // error messages
    this._errorsEl = $( this._config.elements.errors );
    if ( this._errorsEl === undefined ) {
      throw( "Error: couldn't find the element to update with error messages" );
    }

    // summary
    this._summaryEl = $( this._config.elements.summary );
    if ( this._summaryEl === undefined ) {
      throw( "Error: couldn't find the element to update with summary" );
    }

    // message saying how many jobs are outstanding
    this._numJobsEl = $( this._config.elements.numJobs );
    if ( this._numJobsEl === undefined ) {
      throw( "Error: couldn't find the element to update with the number of running jobs" );
    }
    this._jobsLabelEl = $( this._config.elements.jobLabel );
    if ( this._jobsLabelEl === undefined ) {
      throw( "Error: couldn't find the element to update with the label for the number of running jobs" );
    }

    // loading messages
    this._loadingEl = $( this._config.elements.loading );
    if ( this._loadingEl === undefined ) {
      throw( "Error: couldn't find the 'loading' element" );
    }

    // summary text
    this._summaryTextEl = $( this._config.elements.summaryText );
    if ( this._summaryTextEl === undefined ) {
      throw( "Error: couldn't find the element for summary text" );
    }

    // console.log( "Results._checkElements: all passed checks" );
  },

  //----------------------------------------------------------------------------
  /**
   * Shows or hides the alignment rows
   *
   * @private
   * @param {Event} e mouse click even on the domain graphic
   */
  _toggleAlignment: function( e ) {

    // get the clicked table cell. If the click was actually on the img, we 
    // need to walk up a little
    var cell = e.findElement( "td" );

    // the image in that cell
    var img = cell.down("img");

    // walk up the DOM and onto the next row in the table
    var nextRow = cell.up("tr").next("tr");
    
    // toggle the state of the row and reset the image source to point to the
    // new image
    nextRow.toggle();
    img.src = nextRow.visible() ? this._config.hideButton : this._config.showButton;
  },
  
  //----------------------------------------------------------------------------
  /**
   * Shows or hides all of the alignments in one fell swoop...
   *
   * @private
   * @param {Event} e mouse click even on the domain graphic
   */
  _showHideAll: function( e ) {

    // the clicked link
    var link = e.findElement();

    // decide whether it was "Show" or "hide"
    var show = link.innerHTML.match( "Show" );

    // get the table that corresponds to the clicked link, and then show/hide
    // the alignment rows from that table
    var tbody = link.up("table").down("tbody");
    tbody.select("tr.alignment").invoke( show ? "show" : "hide" );

    // set the show/hide images for the toggled rows
    var imgSrc = show ? this._config.hideButton : this._config.showButton;
    tbody.select("td.showSwitch img").each( function( img ) {
      img.src = imgSrc;
    } );    
    
  },

  //----------------------------------------------------------------------------
  /**
   * Writes the verbose description of the results at the top of the page
   *
   * @private
   * @param {Object} details statistics for the job results
   */
  _writeSummary: function( details ) {
    // console.log( "Results._writeSummary: writing results for %s", this._jobId );
    
    var aHits = details.A;
    var bHits = details.B || {};
    var s = "";
    
    if ( this._config.options.ga ) {
    
      s += "We found <strong>" + aHits.numSignificantMatches +
           "</strong> Pfam-A match" + ( aHits.numSignificantMatches > 1 ? "es" : "" ) +
           " to your search sequence"; 

    } else {
    
      if ( aHits && aHits.total > 0 ) {
  
        s += "We found <strong>" + aHits.total + 
             "</strong> Pfam-A match" + ( aHits.total > 1 ? "es" : "" ) + 
             " to your search sequence";
  
        if ( aHits.numSignificantMatches > 0 && aHits.numInsignificantMatches > 0 ) {
          s += " (<strong>" + aHits.numSignificantMatches + "</strong> significant and " +
               "<strong>" + aHits.numInsignificantMatches + "</strong> insignificant)";
        } else if ( aHits.numInsignificantMatches > 0 ) {
          s += " (there were <strong>no</strong> significant matches)";
        } else {
          s += " (<strong>all</strong> significant)";
        }
        
      } else {
        s += "We did not find any Pfam-A matches to your search sequence";
      }
      
    }
    
    if ( bHits && bHits.total > 0 ) {

      if ( aHits && aHits.total > 0 ) {
        s += " and ";
      } else {
        s += " but we did find ";
      }      
      s += "<strong>" + bHits.total + "</strong> Pfam-B match" +
           ( bHits.total > 1 ? "es" : "" ) + ".";
      
    } else if ( bHits && ! ( bHits.total > 0 ) ) {
      
      if ( aHits && aHits.total > 0 ) {
        s += " but we did not find any Pfam-B matches.";
      } else if ( aHits && ! ( aHits.total > 0 ) ) {
        s += " nor any Pfam-B matches.";
      }
      
    } else {
      s += ". You did not choose to search for Pfam-B matches.";
    }
    
    if ( this._config.options.ga ) {
      s += " Because you chose to show only hits that score above the " +
           "gathering threshold, there are no <em>in</em>significant " +
           "Pfam-A hits.";
    }

    this._summaryTextEl.update( s );
  }

  //----------------------------------------------------------------------------

} );
