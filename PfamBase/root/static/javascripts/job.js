
// job.js
// jt6 20070411 WTSI
//
// javascript class implementing a "job tracker", with progress bar. The
// use of the timer is copied from prototype.js.
//
// $Id: job.js,v 1.8 2009-11-25 10:49:15 jt6 Exp $

// Copyright (c) 2007: Genome Research Ltd.
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

// class variables are now declared at the end

// class definition
var Job = Class.create({

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  
  initialize: function( jobConfig ) {

    // check the inputs
    try {
      this.checkInput( jobConfig );
    } catch( e ) {
      // console.error( e );
      throw e;
    }

    //----------------------------------------

    // job parameters, encoded as a JSON string something like this
    /*
     * [
     *   {
     *     checkURI      => "/pfam/seqsearch/checkStatus",
     *     doneURI       => "/pfam/seqsearch/jobDone",
     *     estimatedTime => 24,
     *     interval      => 3,
     *     jobClass      => "pfamASearch",
     *     jobId         => "7925AD28-12AF-11DC-AED1-AB9C94253275",
     *     name          => "Pfam A search",
     *     opened        => "2007-06-04 16:22:57",
     *   },
     *   {
     *     checkURI      => "/pfam/seqsearch/checkStatus",
     *     doneURI       => "/pfam/seqsearch/jobDone",
     *     estimatedTime => 12,
     *     interval      => 3,
     *     jobClass      => "pfamBSearch",
     *     jobId         => "792DF4C4-12AF-11DC-AED1-AB9C94253275",
     *     name          => "Pfam B search",
     *     opened        => "2007-06-04 16:22:57",
     *   },
     * ]
     */

    this.estimatedTime = jobConfig.estimatedTime; // estimated job run time
    this.interval      = jobConfig.interval;      // polling interval
    this.jobId         = jobConfig.jobId;         // the job ID, from the server
    this.elId          = jobConfig.jobId;         // use the job ID as element ID
    this.jobName       = jobConfig.name;          // a pretty name for the job
    this.jobClass      = jobConfig.jobClass;      // job class - used for styling
    this.opened        = jobConfig.opened;        // submission time
    this.checkURI      = jobConfig.checkURI;      // URI to poll for status
    this.doneURI       = jobConfig.doneURI;       // URI for retrieving results

    this._noredirect   = jobConfig._noredirect;   // debug flag; don't redirect to
                                                   // results page if true

    //----------------------------------------

    // build the markup for the Job
    this.buildMarkup(); 

    //----------------------------------------

    // set up the interval timer and timeout

    // create a timer. We'll trigger at one interval but only poll the server at
    // the interval specified by the caller
    this.timer = setInterval( this.onTimerEvent.bind( this ), Job.TICK_INTERVAL );
    // console.debug( "Job.initialize: started interval timer" );
    
    // set a timeout
    this.timeout = setTimeout( this.onTimeout.bind( this), Job.TIMEOUT );
    // console.debug( "Job.initialize: started timeout timer" );

    //----------------------------------------

    // keep track of how many jobs are running concurrently
    Job.LIST.set( this.jobId, 1 );
    Job.RUNNING.set( this.jobId, 1 );
    
    //----------------------------------------

    this.log( this.jobName + ": initialize", 
              "submitted job " + this.jobId );

  },

  //----------------------------------------------------------------------------
  //- methods ------------------------------------------------------------------
  //----------------------------------------------------------------------------

  // check the input to the constructor

  checkInput: function( jobConfig ) {

    // estimated run time
    if( jobConfig.estimatedTime === undefined ) {
      throw "No estimated time specified";
    }

    // check for a polling interval
    if( jobConfig.interval === undefined ) {
      throw "No polling interval specified";
    }

    // must have a job ID
    if( jobConfig.jobId === undefined ) {
      throw "No job ID specified";
    }
    
    // the presentable name for the job
    if( jobConfig.name === undefined ) {
      throw "No job name specified";
    }

    // the "class name" for the job
    if( jobConfig.jobClass === undefined ) {
      throw "No class name specified";
    }

    // when was it started ?
    if( jobConfig.opened === undefined ) {
      throw "No start time given";
    }

    // the URI that we poll
    if( jobConfig.checkURI === undefined ) {
      throw "Server URIs not configured: 'checkURI' not defined";
    }

    // and the one we call when we're told that the job is done 
    if( jobConfig.doneURI === undefined ) {
      throw "Server URIs not configured: 'doneURI' not defined";
    }
  },

  //----------------------------------------------------------------------------
  // build the markup

  buildMarkup: function() {

    // start with a template and edit in the specific values, like IDs
    var jobDivString = '\
      <div class="job ' + this.jobClass + '" id="job' + this.jobId + '">\
        <div class="jobTitle">' + this.jobName + '</div>\
        <dl>\
          <dt>Status:</dt>\
          <dd class="status">unknown</dd>\
          <dt>Submitted:</dt>\
          <dd class="submitted">' + this.opened + '</dd>\
          <dt>Started:</dt>\
          <dd class="started">-</dd>\
          <dt>Estimated run time:</dt>\
          <dd class="runtime">' + this.estimatedTime + 's</dd>\
          <dt>Progress:</dt>\
          <dd>\
            <div class="progressWrapper">\
              <div class="progressBar"></div>\
            </div>\
          </dd>\
        </dl>\
      </div>';

    // add the new div to the bottom of the "jobs" div
    var jobDiv = $("jobs").insert( jobDivString );

    // store pointers to specific elements within this div
    this.statusMsg    = jobDiv.select( ".status"      ).first();
    this.startedValue = jobDiv.select( ".started"     ).first();
    this.bar          = jobDiv.select( ".progressBar" ).first();

  },
  
  //----------------------------------------------------------------------------
  // stop the timer
  
  stop: function() {
    if( ! this.timer || ! this.timeout ) {
      return;
    }
    clearInterval( this.timer );
    this.timer   = null;
    clearTimeout( this.timeout );
    this.timeout = null;

    // console.debug( "Job.stop: timer stopped" );
  },

  //----------------------------------------------------------------------------
  //- timer callbacks ----------------------------------------------------------
  //----------------------------------------------------------------------------

  // the callback for the timer. Performs a couple of checks on each tick: 
  //  o on each tick we'll see if the job is running and, if so, update the 
  //    progress bar.
  //  o at the specified polling intervals we'll send a request to the server to
  //    retrieve the job status
  
  onTimerEvent: function() {

    // only poll the server for the job status at the required interval    
    if( 0 != this.tick++ % this.interval ) {
      var r = new Ajax.Request( this.checkURI, 
                                {
                                  onSuccess:  this.checkStatus.bind( this ),
                                  onFailure:  this.pollingFailed.bind( this ),
                                  parameters: { jobId: this.jobId }
                                }
                              );
    }
  },

  //----------------------------------------------------------------------------
  // the callback for the timeout. 
  
  onTimeout: function() {
    // console.debug( "Job.onTimeout: job timed out ! Stopping updates..." );

    this.log( this.jobName + ": timed out", "please try your search again later" );

    // tidy up here
    this.stop();
  },

  //----------------------------------------------------------------------------
  //- job status callbacks -----------------------------------------------------
  //----------------------------------------------------------------------------

  // called when the ajax call to poll the job status fails. An opportunity
  // to tidy up and tell the user that there were problems

  pollingFailed: function() {
    // console.debug( "Job.statusCheckFailed: couldn't poll server for status" );

    // tidy up here
  },
  
  //----------------------------------------------------------------------------
  // reacts to the status of the job, as retrieved from the server

  checkStatus: function( result ) {
    var statusString = result.responseText;
    var statusObj;
    try {
      statusObj = statusString.evalJSON();
    } catch( e ) {
      // problem retrieving status. Say so
      // console.debug( "Job.checkStatus: couldn't parse the JSON from the server: " + statusString );
      this.log( this.jobName + ": error",
                "there was a problem retrieving the status of job " + this.jobName );

      return;
    }

    // got a status object from the server response
    // console.debug( "Job.checkStatus: status object: " +  statusString );
    if( statusObj.status == "PEND" ) {
      this.jobPending( statusObj );
    } else if( statusObj.status == "RUN" ) {
      this.jobRunning( statusObj );
    } else if( statusObj.status == "DONE" ) {
      this.jobDone( statusObj );
    } else if( statusObj.status == "FAIL" ) {
      this.jobFailed( statusObj );
    } else {
      // something unexpected happened...
    }
  },

  //----------------------------------------------------------------------------
  //- handlers for the various job status values -------------------------------
  //----------------------------------------------------------------------------

  // the job is pending. Don't update the progress bar but do

  jobPending: function( statusObj ) {
    // console.debug( "Job.jobPending: still waiting for the job to run..." );

    // set the status message
    this.statusMsg.update( '<span class="spinner">pending</span>' );
    
    // log the number of pending jobs
    var n = statusObj.numPending;
    var w = statusObj.waitTime;
    if( n != this.lastPending && n > 0 ) {
      
      this.log( this.jobName + ": pending",
                "there " + ( (n>1) ? "are " : "is " ) + n + 
                " job" + ( (n>1) ? "s" : "" ) + 
                " in the queue ahead of yours. Estimated wait " + w + 
                " second" + ( (w>1) ? "s" :"" ) );
    }
    
    // keep track of how many jobs were pending when we last checked
    this.lastPending = n;
  },

  //----------------------------------------------------------------------------
  // the job is running. Update the progress bar etc.

  jobRunning: function( statusObj ) {
    // console.debug( "Job.jobRunning: updating the client" );

    if( ! this.running ) {
      
      // don't do all of this again
      this.running = true;

      // start the "ticks", so that the progress bar gets updated
      this.tick = 0;
       
      // set the status message
      this.statusMsg.update( "running" );

      // log it
      this.log( this.jobName + ": submitted",
                "your job is now running (started " + statusObj.started + ")" );

      // add the start time to the status line
      this.startedValue.update( statusObj.started );
    }

    // update the status bar
    this.updateProgressBar();

  },

  //----------------------------------------------------------------------------
  // the job has completed. Tidy up

  jobDone: function( statusObj ) {
    // console.debug( "Job.jobDone: the job completed successfully" );

    // set the status message
    this.statusMsg.update( "done" );
  
    // log it
    this.log( this.jobName + ": completed", "finished at " + statusObj.closed );
  
    // max out the progress bar
    this.bar.setStyle( { width: '100%' } );
    
    this.jobEnded( statusObj );

    // tidy up here

  },
  
  //----------------------------------------------------------------------------
  // the job has completed. Tidy up

  jobFailed: function( statusObj ) {
    // console.debug( "Job.jobFailed: the job failed" );

    // set the status message
    this.statusMsg.update( "failed" );
  
    // log it
    this.log( this.jobName + ": failed", "finished at " + statusObj.closed );
  
    // max out the progress bar
    this.bar.setStyle( { width: '100%' } );
  
    this.jobEnded( statusObj );

    // tidy up here

  },

  //----------------------------------------------------------------------------
  // tidy up at the completion of a job, whether successful or unsuccessful

  jobEnded: function( statusObj ) {
    // console.debug( "Job.jobEnded: job completed; tidying up" );
    
    // just for good form... shouldn't matter but still
    this.running = false;
    
    // stop the timer and kill the timeout
    this.stop();

    // update the status message
    // if( statusObj.status == "FAIL" ) {
      // console.debug( "Job.jobEnded: job failed..." );  
    // } else {
      // console.debug( "Job.jobEnded: job successful" );  
    // }

    // update the list of currently running jobs and then see if we should
    // finish off
    Job.RUNNING.unset( this.jobId );

    var left = Job.RUNNING.keys().size();
    // console.debug( "Job.jobEnded: " + left + " jobs left" );
    if( left < 1 ) {
      // console.debug( "Job.jobEnded: all jobs completed; finishing up" );  
      this.log( this.jobName + ": finished",
                "all jobs complete; redirecting" );
      this.finish();
    } else {
      // console.debug( "Job.jobEnded: still waiting for " + left + " jobs" );  
      this.log( this.jobName + ": waiting",
                "still waiting for " + left + 
                " job" + ( (left > 1 ) ? "s" : "" ) + 
                " to complete..." );
    }
  },

  //----------------------------------------------------------------------------
  //- methods to update client display -----------------------------------------
  //----------------------------------------------------------------------------

  // updates the progress bar

  updateProgressBar: function() {
    var barWidth = this.bar.getWidth();
    var maxWidth = this.bar.ancestors().first().getWidth();
  
    if( barWidth < maxWidth ) {
      var newWidth = Math.floor( maxWidth * this.tick / this.estimatedTime );
      this.bar.setStyle( { width: newWidth+'px' } );
    } else {
      this.bar.setStyle( { width: '1px' } );
      this.tick = 0;
    }
  },

  //----------------------------------------------------------------------------
  // all jobs are done; redirect to the "done" page

  finish: function( result ) {
    // console.debug( "Job.finish: finishing up" );

    // need to build the URI with the job IDs given as parameters
    var doneLoc;
    if( this.doneURI.indexOf( "?" ) > 0 ) {
      doneLoc = this.doneURI + "&";
    } else {
      doneLoc = this.doneURI + "?";
    }
    var buildURI = $A( Job.LIST.keys() ).inject( doneLoc,
                       function( uri, jobId ) {
                         return uri + "jobId=" + jobId + "&";
                       }
                     );
    // chop off the last ampersand
    var uri = buildURI.substr( 0, buildURI.length - 1 )

    // unless the configuration tells us not to, redirect the browser to the
    // results page
    if ( this._noredirect !== undefined && this._noredirect ) {
      alert( "All jobs complete" );
    } else {
      // console.debug( "Job.finish: redirecting to: |" + uri + "|" )
      document.location = uri;
    }
  },
  
  //----------------------------------------------------------------------------  

  log: function( title, msg ) {
    
    // where are we putting the new message ?
    var messages = $A( $("logScroller").childNodes );
    var numMsgs = messages.size();
    var last = $A( $("logScroller").childNodes ).last();

    // a span to hold the whole log entry
    var entryEl = new Element( "div", { id: "msg" + numMsgs } );

    // the title
    var titleEl = new Element( "span", { "class": "title" } ).update( title + ":&nbsp;" );
    
    // the message itself
    var msgEl = new Element( "span", {"class": "msg" } ).update( msg );

    // the entry
    entryEl.appendChild( titleEl );
    entryEl.appendChild( msgEl );
    $("logScroller").appendChild( entryEl );

    // scroll the logger to make the most recent message appear
    $("logScroller").scrollTop = $("logScroller").scrollHeight;
  
  //----------------------------------------------------------------------------  
  }

} );

// class variables

// a tally of submitted jobs and a list of those that are actually running
if( Job.LIST    === undefined ) { Job.LIST    = new Hash(); }
if( Job.RUNNING === undefined ) { Job.RUNNING = new Hash(); }

// constants

// tick interval
Job.TICK_INTERVAL = 1000;

// the timeout period
Job.TIMEOUT = 3600000;
