
// job.js
// jt6 20070411 WTSI
//
// javascript class implementing a "job tracker", with progress bar. The
// use of the timer is copied from prototype.js.
//
// $Id: job.js,v 1.1 2007-04-16 15:49:19 jt6 Exp $

// Copyright (c) 2007: Genome Research Ltd.
// 
// Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
// 
// This is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//  
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//  
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
// or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

var Job = Class.create();

// a tally of submitted jobs and a list of those that are actually running
Job.LIST    = new Hash();
Job.RUNNING = new Hash();

Job.prototype = {

  //----------------------------------------------------------------------------
  //- constructor --------------------------------------------------------------
  //----------------------------------------------------------------------------
  
  initialize: function( jobConfig ) {

    // check the inputs
    try {
      this.checkInput( jobConfig );
    } catch( e ) {
      console.error( e );
      throw e;
    }

    //----------------------------------------

    // job parameters
    this.jobName       = jobConfig.name;                 // a pretty name for the job
    this.elId          = jobConfig.elId;                 // the ID for the parent div
    this.jobId         = jobConfig.status.jobId;         // the job ID, from the server
    this.estimatedTime = jobConfig.status.estimatedTime; // estimated job run time
    this.checkURI      = jobConfig.checkURI;             // URI to poll for status
    this.doneURI       = jobConfig.doneURI;              // URI for retrieving results

    //----------------------------------------

    // build the markup for the progress bar
    this.buildProgressBar();

    // keep shortcut to the status message and error message elements
    this.statusMsg = $( jobConfig.statusMsg );
    this.errorMsg  = $( jobConfig.errorMsg );

    //----------------------------------------

    // set periodical execution parameters
    this.interval = ( jobStatus.interval === undefined ) ? 3 : jobStatus.interval;    
    this.callback = this.update;
    this.currentlyExecuting = false;

    // create a timer. We'll trigger every second but only poll the server at
    // the interval specified by the caller
    this.tick = 0;
    this.timer = setInterval( this.onTimerEvent.bind( this ), 1000 );

    //----------------------------------------

    // keep track of how many jobs are running concurrently
    Job.LIST[ this.jobId ] = 1;
    Job.RUNNING[ this.jobId ] = 1;
    console.debug( "Job.initialize: currently running jobs: " + Job.RUNNING );

    //----------------------------------------

    // get a logger (or pretend to) and log this job
    this.l = this.getLogger();
    // this.l.log( "submitted job \"" + this.jobName + "\" (" + this.jobId + ")",
    //             "initialize" );
    this.l.log( "submitted job " + this.jobId, "initialize" );

  },

  //----------------------------------------------------------------------------
  //- methods ------------------------------------------------------------------
  //----------------------------------------------------------------------------

  // check the input to the constructor

  checkInput: function( jobConfig ) {

    // a name for the job, user readable
    if( jobConfig.name === undefined ) {
      throw "Job name not given";
    }

    // make sure the two URIs are defined
    
    // the URI that we poll
    if( jobConfig.checkURI === undefined ) {
      throw "Server URIs not configured: 'checkURI' not defined";
    }

    // and the one we call when we're told that the job is done 
    if( jobConfig.doneURI === undefined ) {
      throw "Server URIs not configured: 'doneURI' not defined";
    }

    // must have a job ID too - specified in the status object, which comes
    // ultimately from the server right now
    if( jobConfig.status.jobId === undefined ) {
      throw "No job ID specified";
    }

    // and finally, we need to know the ID for the progress bar holder, so
    // that we can update its components
    if( jobConfig.elId === undefined ) {
      throw "No element ID specified";
    }
  },

  //----------------------------------------------------------------------------
  // build the markup for the progress bar

  buildProgressBar: function() {
    this.wrapper = document.createElement( "div" );
    this.wrapper.setAttribute( "class", "progressWrapper" );

    this.bar = document.createElement( "div" );
    this.bar.setAttribute( "class", "progressBar" );

    this.wrapper.appendChild( this.bar );
    $(this.elId).appendChild( this.wrapper );
  },
   
  //----------------------------------------------------------------------------
  // stop the timer
  
  stop: function() {
    if( ! this.timer ) {
      return;
    }
    clearInterval( this.timer );
    this.timer = null;
  },

  //----------------------------------------------------------------------------

  // try to create a logger or generate a stub if we fail to create a real one
  getLogger: function() {
    console.debug( "Job.getLogger: generating a logger" );
    var l = new Logger();
    if( l === undefined ) {
      l = {};
      l.log = function() {};      
    }
    console.debug( "Job.getLogger: l = " + l );
    return l;
  },

  //----------------------------------------------------------------------------
  //- callbacks ----------------------------------------------------------------
  //----------------------------------------------------------------------------

  // the callback for the timer itself. Triggers the call to poll the server 
  // and update the page and progress bar

  onTimerEvent: function() {
    if( ! this.currentlyExecuting ) {
      try {
        this.currentlyExecuting = true;
        this.update();
      } finally {
        this.currentlyExecuting = false;
      }
    }
  },
  
  //----------------------------------------------------------------------------
  // update the progress bar and see if we should poll the server on this tick

  update: function() {
    this.updateProgressBar();    
    
    if( 0 != this.tick++ % this.interval ) {
      new Ajax.Request( this.checkURI, 
                        {
                          method:     'get',
                          onSuccess:  this.checkStatus.bind( this ),
                          parameters: { jobId: this.jobId }
                        }
                      );
    }
  },

  //----------------------------------------

  // see if the job is complete. Updates the status message appropriately

  checkStatus: function( result ) {
    var statusString = result.responseText;
    var statusObj;
    try {
      statusObj = statusString.evalJSON();
    } catch( e ) {
      // problem retrieving status. Say so
      this.errorMsg.update( "There was a problem retrieving the status of your job." );
    }

    if( statusObj.status == "DONE" || statusObj.status == "FAIL" ) {
      this.jobDone( statusObj );
    } else {
      this.statusMsg.update( "pending" );
    }
  },

  //----------------------------------------------------------------------------
  // update the progress bar

  updateProgressBar: function() {

    var barWidth = this.bar.getWidth();
    var maxWidth = this.bar.parentNode.getWidth();
  
    if( barWidth < maxWidth ) {
      var newWidth = maxWidth * this.tick / this.estimatedTime;
      this.bar.setStyle( { width: newWidth+'px' } );
    } else {
      this.bar.setStyle( { width: '1px' } );
      this.tick = 0;
    }
  },

  //----------------------------------------------------------------------------
  // the job has completed so tidy up

  jobDone: function( statusObj ) {

    // stop the timer
    this.stop();

    // as we're done with it, hide the progress bar entirely
    $(this.elId).getElementsByClassName( "progressWrapper" ).each(
      function( el ) {
        el.hide();
      }
    );

    // update the status message
    if( statusObj.status == "FAIL" ) {
      console.debug( "Job.jobDone: job failed..." );  
      this.statusMsg.update( "failed" ).addClassName( "jobFailed" );
    } else {
      console.debug( "Job.jobDone: job successful" );  
      this.statusMsg.update( "done" ).addClassName( "jobSuccessful" );
    }

    // update the list of currently running jobs and then see if we should
    // finish off
    Job.RUNNING.remove( this.jobId );

    var left = Job.RUNNING.keys().size();
    console.debug( "Job.jobDone: " + left + " jobs left" );
    if( left < 1 ) {
      console.debug( "Job.jobDone: all jobs completed; finishing up" );  
      this.l.log( "all jobs complete; redirecting" );
      this.finish();
    } else {
      console.debug( "Job.jobDone: still waiting for " + left + " jobs" );  
      this.l.log( "still waiting for " + left + 
                  " job" + (left > 1 ) ? "s" : "" + " to complete..." );
    }

    // poll the server one last time to get the result of the job  
    /* new Ajax.Request( this.doneURI,
                      {
                        method:     'get',
                        onSuccess:  this.finish.bind(this),
                        parameters: { id: this.jobId }
                      }
                    );
       */
  },
  
  //----------------------------------------


  finish: function( result ) {
    console.debug( "Job.finish: finishing up" );
    
    // all jobs are done; redirect to the "done" page
    var doneLoc = this.doneURI + "?";
    var buildURI = $A( Job.LIST.keys() ).inject( doneLoc,
                 function( uri, jobId ) {
                   return uri + "job=" + jobId + "&amp;";
                 }
               );
    // chop of the last ampersand
    var uri = buildURI.substr( 0, buildURI.length-5 )
    console.debug( "Job.finish: redirecting to: |" + uri + "|" )

    document.location = uri;
  },
  
  //----------------------------------------------------------------------------  

};