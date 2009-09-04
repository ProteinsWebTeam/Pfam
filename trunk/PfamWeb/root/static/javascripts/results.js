
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
// $Id: results.js,v 1.1 2009-09-04 13:01:00 jt6 Exp $

var Results = Class.create( {

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------

  initialize: function( config ) {
    console.log( "Results.initialize: setting up" );

    // store the whole configuration object
    this._config = config;

    // stuff for the domain graphics data
    this._sequence = config.sequence;
    this._sequence.regions = [];
    this._sequence.motifs  = [];

    // error message templates
    this._errorTemplate = new Template( "Job #{jobId} failed" );

    // somewhere to store the details of completed jobs, keyed on job ID
    this._details = new Hash(); 

    // retrieve and check the handles to the various page elements
    this._checkElements(); 

    // set up the updaters
    this._updaters = new Hash();
    console.log( "Results.initialize: adding updaters" );
    document.observe( "dom:loaded", this._addUpdaters.bind( this ) );

    console.log( "Results.initialize: done setting up" );
  },  

  //----------------------------------------------------------------------------
  //- methods ------------------------------------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //- public methods -----------------------------------------------------------
  //----------------------------------------------------------------------------

  jobDone: function( jobId, details ) {
    console.log( "Results.jobDone: got results for %s: ", jobId, details );

    // stash the details of this set of results
    this._details.set( jobId, details );
    console.log( "Results.jobDone: stashing result details for type %s",
      details.jobType );
    this._details.set( details.jobType, details ); // store the data data, but 
                                                    // keyed on the job type too

    // add the newly determined regions/motifs to the sequence object. This
    // will be passed to the domain graphics library when we draw the domain
    // graphic
    this._sequence.regions = this._sequence.regions.concat( details.regions );
    this._sequence.motifs  = this._sequence.motifs.concat( details.motifs );

    // remove this job from the list of running jobs
    this._updaters.unset( jobId );

    // how many jobs are still running ?
    var numJobs = this._updaters.keys().size() || 0;
    console.log( "Results.jobDone: %d job(s) still running", numJobs );

    // update the status display
    this._updateJobCount( numJobs );

    if ( numJobs < 1 ) {
      console.log( "Results.jobDone: no more running jobs; finishing page" );
      
      // hide the spinner
      this._loadingEl.hide();
  
      // build the summary text and show it
      this._writeSummary( jobId );
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
//      $$("#results table.resultTable div.hmmWindow").each( function(a) {
//        console.log( "setting width to %d for ", alignmentWidth, a );
//        a.setStyle( { width: alignmentWidth+"px" } );
//      } );
  
      // draw the domain graphics. Hand in the base URL for the links on the image
      this._pg = new PfamGraphic( "dg", this._sequence );
      this._pg.setImageParams( { regionUrl: this._config.familyUrl } );
      this._pg.render();
    
      // set up the underlining
      this._underliner = new Underliner( this._pg );
    }

  },

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------

  // updates the status display showing the number of currently running jobs
  
  _updateJobCount: function( numJobs ) {
    this._numJobsEl.update( numJobs );
    this._jobsLabelEl.update( numJobs > 1 ? "jobs" : "job" );
  },

  //----------------------------------------------------------------------------

  // checks the configuration for elements that are used by the class. Tries
  // to convert every item into an element (using "$()") and throws an error
  // if any elements are missing

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

  },

  //----------------------------------------------------------------------------

  // starts a periodical updater for each job in the input

  _addUpdaters: function() {
    console.log( "Results._addUpdaters: adding updaters for each job ID" );

    var pollUrl = this._config.pollUrl;
    console.log( "Results._addUpdaters: pollUrl: |%s|", pollUrl );

    var jobs    = this._config.jobs;
    console.log( "Results._addUpdaters: jobs: ", jobs );

    // generate a new PeriodicalUpdater for each job ID
    jobs.keys().each( function( jobId ) {
      console.log( "Results._addUpdaters: setting up polling for job %s", jobId );

      var insertionPosition = jobs.get( jobId ).job_type === "A" ? "top" : "bottom";
      console.log( "Results._addUpdaters: inserting results at %s", insertionPosition );

      var r = new Updater(
        pollUrl,
        {
          method:     'get',
          parameters: { jobId: jobId },
          frequency:  1,
          decay:      1.2,
          on202:      this._on202.bind( this, jobId ),
          onSuccess:  this._onSuccess.bind( this, jobId ),
          onFailure:  this._onFailure.bind( this, jobId )
        }
      );
      
      // keep hold of all of the updaters that we create, so that we can keep 
      // track of how many job remain outstanding
      this._updaters.set( jobId, r );
      
    }.bind( this ) ); // end of "foreach job"

    console.log( "Results._addUpdaters: done adding updaters" );
  },
  
  //-----------------------------------

  _on202: function( jobId, response ) {
    console.log( "Results._on202: job %s, status 202; no results; polling further", jobId );

    // we need to have a method for this status, even though we don't get any
    // information from the server in the response, otherwise the Updater sees
    // "2XX" and considers it to be "success"
  },

  //-----------------------------------

  // inserts the job results into the page

  _onSuccess: function( jobId, response ) {
    console.log( "Results._onSuccess: job %s, status 200; job done", jobId );

    // stop the updater for this job
    this._updaters.get( jobId ).stop();
    console.log( "Results._onSuccess: stopped updater for %s", jobId );
  
    // decide whether the results should go at the top or bottom of the page
    if ( this._config.jobs.get( jobId ).job_type === "A" ) {
      this._updateEl.insert( { "top": response.responseText } );
    } else {
      this._updateEl.insert( { "bottom": response.responseText } );
    }      

    // hide all of the alignment rows once the table is built
    this._updateEl.select(".alignment").invoke( "hide" );
    
    // if we're using the gathering threshold, hide the insignificant hits and
    // the front of the table title too
    if ( this._config.options.ga ) {
      $("pfamASummaryI").hide();
      this._updateEl.select(".titlePrefix").invoke( "hide");
    }

    console.log( "Results._onSuccess: job %s; results loaded", jobId );
  },

  //-----------------------------------

  // handles a job failure response

  _onFailure: function( jobId, response ) {
    console.log( "Results._onFailure: failure response for job %s; stopping polling", jobId );

    // how many jobs are still running ?
    var numJobs = this._updaters.keys().size() || 0;
    console.log( "Results.jobDone: %d job(s) still running", numJobs );

    // stop the updater for this job and remove it from the list of
    // running jobs
    this._updaters.unset( jobId ).stop();

    // and update the display to show many are still running
    var numJobs = this._updaters.keys().size() || 0;
    console.log( "Results._onFailure: %d job(s) still running", numJobs );
    this._updateJobCount( numJobs );

    if ( numJobs < 1 ) {
      console.log( "Results._onFailure: no more jobs running" );

      // hide the "waiting for the results of..." message
      this._loadingEl.hide();

      this._summaryEl.show();
    }

    // add error messages
    if ( ! this._errorsList ) {
      console.log( "Results._onFailure: building results list" );
      var errorHeader = '<h1>Job failures</h1>' +
                        '<p>One or more of the searches that you submitted had errors during execution:</p>' +
                        '<ul></ul>';
      
      this._errorsEl.insert( { top: errorHeader } );
      
      this._errorsList = this._errorsEl.down("ul");
    } 

    // var msg = this._errorTemplate.evaluate( { jobId: jobId } );
    console.log( "Results._onFailure: adding error to results list" );
    this._errorsList.insert( { bottom: new Element( "li" ).update( response.responseText ) } );
    this._errorsEl.show();

    //this._updateEl.insert( { 'top': response.responseText } );
  },
  
  //----------------------------------------------------------------------------
  
  // shows or hides the alignment rows

  _toggleAlignment: function( e ) {

    // get the clicked table cell. If the click was actually on the img, we 
    // need to walk up a little
    var cell = e.element();
    if ( cell.up("td") ) {
      cell = cell.up("td");
    }

    // the image in that cell
    var img = cell.down("img");

    // walk up the DOM and onto the next row in the table
    var row = cell.up("tr").next("tr");
    
    // toggle the state of the row and reset the image source to point to the
    // new image
    row.toggle();
    img.src = row.visible() ? this._config.hideButton : this._config.showButton;
  },
  
  //----------------------------------------------------------------------------

  // shows or hides all of the alignments in one fell swoop...

  _showHideAll: function( e ) {

    // the clicked link
    var link = e.element();

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
  
  // writes the verbose description of the results at the top of the page

  _writeSummary: function( jobId) {
    console.log( "Results._writeSummary: writing results for %s", jobId );
    
    //var r = this._details.get( jobId );
    var aHits = this._details.get( "A" );
    var bHits = this._details.get( "B" ) || {};
    var s = "";
    
    if ( this._config.options.ga ) {
    
      s += "We found <strong>" + aHits.significant +
           "</strong> Pfam-A match" + ( aHits.significant > 1 ? "es" : "" ) +
           " to your search sequence"; 

    } else {
    
      if ( aHits && aHits.total > 0 ) {
  
        s += "We found <strong>" + aHits.total + 
             "</strong> Pfam-A match" + ( aHits.total > 1 ? "es" : "" ) + 
             " to your search sequence";
  
        if ( aHits.significant > 0 && aHits.insignificant > 0 ) {
          s += " (<strong>" + aHits.significant + "</strong> significant and " +
               "<strong>" + aHits.insignificant + "</strong> insignificant)";
        } else if ( aHits.insignificant > 0 ) {
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
