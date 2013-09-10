
//------------------------------------------------------------------------------
//- Preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, $$, Class, console, Element, Hash, document, PfamGraphic, 
  Underliner, Updater */

//------------------------------------------------------------------------------
//- Class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// a class to handle the construction and behaviour of the Pfam DNA sequence
// search results page.
//
// jt6 20130218 WTSI
//
// $Id$
// 
// Copyright (c) 2013: Genome Research Ltd.
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

var DnaResults = Class.create( {
  /**
   * @lends DnaResults#
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

    // for convenience...
    this._dl = this._config.dnaSeq.length;

    // error message templates
    this._errorTemplate = new Template( "Job #{jobId} failed" );

    // retrieve and check the handles to the various page elements
    // this._checkElements(); 

    // keep track of the number of hits
    this._significantHitCount = 0;
    this._insignificantHitCount = 0;
    this._framesWithHits = [ false, false, false, false, false, false ];
    this._failedJobs = [];
    this._jobComplete = [ false, false, false, false, false, false ];
    this._completedCount = 0;

    // set up the updater
    this._updater = new Updater(
      this._config.pollUrl,
      {
        method:     'get',
        frequency:  1,
        decay:      1.2,
        onSuccess:  this._onSuccess.bind( this ),
        onFailure:  this._onFailure.bind( this )
      }
    );

    // add listeners to...

    // ... the "toggle coordinates system" button
    $$("caption span.toggleCoordType").each( function( link ) {
      link.observe( "click", this._toggleCoordinateSystem.bind( this ) );
    }.bind( this ) );

    // ... the "show/hide all" links
    $$("caption span.showHideLink").each( function( link ) {
      link.observe( "click", this._showHideAll.bind( this ) );
    }.bind( this ) );

    // console.log( "Results.initialize: done setting up" );
  },  

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------

  // these are all callbacks for the AJAX requests...

  /**
   * Called when the polling call succeeded. 
   *
   * @param {Object} response AJAX response object
   */
  _onSuccess: function( response ) {
    // console.log( "Results.onSuccess: job %s, status 200; job done", this._jobId );

    var results = response.responseJSON.results;
    
    // a flag to show whether we've hit the limit on the number of times we
    // should poll any single job
    var pollLimit = false;

    results.each( function( job, frameNumber ) {
      var status = job.status;
      // console.log( "DnaResults.onSuccess: frame %d is %s", frameNumber, status );

      if ( status == 'PEND' || status == 'RUN' ) {

        // keep track of how many times this particular job has returned "PEND"
        // or "RUN". Once it's been polled more than a given limited number of
        // times, we'll stop polling for it.
        if ( job.pollCount === undefined ) {
          job.pollCount = 0;
        }
        job.pollCount++;
        if ( job.pollCount++ > 10 ) {
          // console.log( "DnaResults.onSuccess: hit polling limit for frame %d", frameNumber );
          pollLimit = true;
        }
      } else {
        this._done( job, frameNumber );
      }
    }, this );

    // should we stop the updater yet ?
    if ( this._completedCount == 6 || pollLimit ) {
      // console.log( "DnaResults.onSuccess: either all jobs are done (%d done) or we hit the poll limit (%b)", 
      //              this._completedCount, pollLimit );
      this._updater.stop();

      // if there were results, re-configure the key. If not, show a message saying that
      if ( this._significantHitCount > 0 || this._insignificantHitCount > 0 ) {
        // we got some results; turn on or off the appropriate bits of the key
        $w("searchingMessage loading").each(Element.hide);
        $w("resultsNotesSwitch optionsListSwitch searchFormLink").each(Element.show);
      } else {
        // no results; show an appropriate message and turn off other bits of the key
        $w("searchingMessage loading").each(Element.hide);
        $("optionsList").update( "There were <strong>no matches</strong> to your DNA sequence." )
                        .show();
        $("searchFormLink").show();
      }
    }
  },

  //-----------------------------------
  /**
   * Handles a job failure response.
   *
   * @param {Object} response AJAX response object
   */
  _onFailure: function( response ) {
    this._updater.stop();
    $("errors").update( "There was a problem with one of more of the searches." )
               .show();
  },
  
  //----------------------------------------------------------------------------
  /**
   * Called when a job is flagged as "DONE". Starts a new AJAX call to retrieve
   * result rows then takes care of pushing them into the staging div and
   * drawing domain graphics.
   *
   * @param {Object} job Object with details of the job
   * @param {Int}    frameNumber the index of the frame for this job
   */
  _done: function( job, frameNumber ) {
    if ( this._jobComplete[frameNumber] ) {
      return;
    }

    var failedList, r, pg, starSearch,
        graphicDiv = $("seq"+frameNumber),
        starRegex = /\*/g;  // used for finding "*" characters in protein sequences

    if ( job.status != "DONE" ) {
      this._jobComplete[frameNumber] = true;
      this._completedCount++;
      this._failedJobs.push(frameNumber);

      failedList = this._failedJobs.size() == 1 
                 ? "The search for frame 1 failed."
                 : "The searches for frames " + this._failedJobs.join(", ") + " failed.";
      $("errors").update( "There was a problem with one of more of the searches. " + failedList )
                 .show();

      return;
    }
    
    r = new Ajax.Updater(
      "stagingDiv",
      this._config.loadTableUrl + "/" + frameNumber,
      {
        method:     "get",
        onComplete: this._loadTable.bind(this, frameNumber)
      }
    );

    this._jobComplete[frameNumber] = true;
    this._completedCount++;

    // find the occurrences of "*" in the protein sequence and flag them
    // with lollipops
    while ( ( starSearch = starRegex.exec( this._config.proteinSeqs[frameNumber] ) ) ) {
      job.graphic.markups.push( {
        "lineColour": "#666",
        "colour":     "#F36",
        "display":    true,
        "v_align":    "top",
        "headStyle":  "square",
        "type":       "Stop codon",
        "start":      starSearch.index,
        "metadata": { 
          "database": "translate", 
          "description": "Stop codon", 
          "start": starSearch.index
        }
      } );
    }

    // draw the domain graphics. Hand in the base URL for the links on the image
    pg = new PfamGraphic( graphicDiv, job.graphic );
    pg.setBaseUrl( this._config.baseUrl );
    pg.render();

    // make the graphic visible
    graphicDiv.up("div").show();

    // update the count of the number of jobs remaining. Hide that if they're 
    // all done
    if ( this._completedCount == 6 ) {
      $("numJobs").up("p").hide();
    }
    $("numJobs").update( 6 - this._completedCount );
    $("jobLabel").update( this._completedCount == 5 ? "job" : "jobs" );
  },

  //----------------------------------------------------------------------------
  /**
   * Loads result rows into the staging div.
   */
  _loadTable: function( frameNumber ) {
    
    // move each of the result rows from the hidden staging table into the
    // visible results table. Keep count of the number of rows at the same
    // time.
    $("stagingDiv").select("table.significantRows tr").each( function( row ) {

      // move the row itself
      $("pfamASummary")
        .show()
        .down("tbody")
        .insert( { bottom: row.remove() } );

      // hide the sequence-to-HMM alignment row. Use these rows to keep track of
      // the number of hits too
      if ( row.hasClassName("alignment") ) {
        row.hide();
        this._significantHitCount++;
        this._framesWithHits[frameNumber] = true;
        $("numSig").update( this._significantHitCount );
        $("numSigPlural").update( this._significantHitCount == 1 ? "" : "s" );
      }

      // add listeners to the "show" buttons
      row.select("td.showSwitch").each( function( toggleSwitch ) {
        toggleSwitch.observe( "click", this._toggleAlignment.bind(this) );
      }.bind(this) );

      // and to the "..." links that appear when a description is too long
      row.select("span.toggleFullDescLink").each( function( toggleSwitch ) {
        toggleSwitch.observe( "click", function(e) {
          var link = e.findElement();
          link.up("td").select("span.familyDesc").invoke("toggle");
          }.bind(this) );
      }.bind(this) );

    }.bind(this) );

    $("stagingDiv").select("table.insignificantRows tr").each( function( row ) {

      $("pfamASummaryI")
        .show()
        .down("tbody")
        .insert( { bottom: row.remove() } );

      if ( row.hasClassName("alignment") ) {
        row.hide();
        this._insignificantHitCount++;
        this._framesWithHits[frameNumber] = true;
        $("numInsig").update( this._insignificantHitCount );
        $("numInsigPlural").update( this._insignificantHitCount == 1 ? "" : "s" );
      }

      row.select("td.showSwitch").each( function( toggleSwitch ) {
        toggleSwitch.observe( "click", this._toggleAlignment.bind(this) );
      }.bind(this) );

      row.select("span.toggleFullDescLink").each( function( toggleSwitch ) {
        toggleSwitch.observe( "click", function(e) {
          var link = e.findElement();
          link.up("td").select("span.familyDesc").invoke("toggle");
          }.bind(this) );
      }.bind(this) );

    }.bind(this) );

    var numFramesHit = this._framesWithHits
                         .findAll( function(hasHits) { return hasHits } )
                         .size();
    $("numFramesHit").update( numFramesHit );
    $("numFramesHitPlural").update( numFramesHit == 1 ? "" : "s" );
  },

  //----------------------------------------------------------------------------
  //
  //  coordinate system maths...
  //
  //
  //  frames
  //
  //      1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
  //      a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z  
  //   
  //       1    2    3    4    5    6    7    8
  //   1  abc  def  ghi  jkl  mno  pqr  stu  vwx      
  //   2   bcd  efg  hij  klm  nop  qrs  tuv  wxy
  //   3    cde  fgh  ijk  lmn  opq  rst  uvw  xyz
  //      
  //       1    2    3    4    5    6    7    8
  //   4  ZYX  WVU  TSR  QPO  NML  KJI  HGF  EDC  
  //   5   YXW  VUT  SRQ  PON  MLK  JIH  GFE  DCB
  //   6    XWV  UTS  RQP  ONM  LKJ  IHG  FED  CBA  
  // 
  //    Z  Y  X  W  V  U  T  S  R  Q  P  O  N  M  L  K  J  I  H  G  F  E  D  C  B  A
  //   26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1
  //
  // 
  //   end points
  //
  //       1  2  3    4  5  6    7  8  9   10 11 12   13 14 15   16 17 18   19 20 21   22 23 24
  //       a  a  a    b  b  b    c  c  c    d  d  d    e  e  e    f  f  f    g  g  g    h  h  h
  //       |          |          |          |          |          |          |          |
  //    d1 |          s=================================================e    |          |
  //    p1 |          s-------------------------------------------e          |          |
  //       |          |          |          |          |          |          |          |
  //       a          b          c          d          e          f          g          h
  //       1          2          3          4          5          6          7          8
  //
  //   +ve strand:
  //   protein --> DNA
  //   d start = 3ps - 3 + f
  //   d end   = 3pe - 3 + f + 2
  //
  //   DNA --> protein
  //   p start = ( ds + 3 - f ) / 3
  //   p end   = ( de + 1 - f ) / 3
  //
  //      24 23 22   21 20 19   18 17 16   15 14 13   12 11 10    9  8  7    6  5  4    3  2  1
  //       a  a  a    b  b  b    c  c  c    d  d  d    e  e  e    f  f  f    g  g  g    h  h  h
  //       |          |          |          |          |          |          |          |
  //   d4  |          s=================================================e    |          |
  //   p4  |          s-------------------------------------------e          |          |
  //       |          |          |          |          |          |          |          |
  //       a          b          c          d          e          f          g          h
  //       1          2          3          4          5          6          7          8
  //
  //    -ve strand
  //    protein --> DNA
  //    d start = dl - 3ps + 7 - f
  //    d end   = dl - 3pe + 5 - f
  //
  //    DNA --> protein
  //    p start = ( dl - ds + 7 - f ) / 3
  //    p end   = ( dl - de + 5 - f ) / 3
  //
  //    examples
  //    p1 = 2p to 6p --> d1 = 4d to 18p
  //    d1 = 4d to 18p --> p1 = 2p to 6p
  //    p4 = 2p to 6p --> d4 = 21d to 7d
  //
  //----------------------------------------------------------------------------
  /**
   * Toggles the coordinates of a match between amino-acid and nucleotide
   * coordinate systems.
   *
   * @params {Event} w click event object
   */
  _toggleCoordinateSystem: function( e ) {

    // the "toggle" element. We use the classes on this element to decide which
    // way we're converting the coords
    var switchEl = e.findElement("span");

    if ( switchEl.hasClassName("aa") ) {
      // convert protein- to DNA-coordinates
      $$("td.aa").each( function( cell ) {

        var frameNum = Number( cell.up("tr").down("span.frameNum").innerHTML ),
            c        = Number( cell.innerHTML );

        if ( frameNum < 4 ) { // positive strand
          if      ( cell.hasClassName("start") ) { cell.innerHTML = 3 * c - 3 + frameNum; }
          else if ( cell.hasClassName("end")   ) { cell.innerHTML = 3 * c - 3 + frameNum + 2; }
        } else {              // negative strand
          if      ( cell.hasClassName("start") ) { cell.innerHTML = this._dl - 3 * c + 7 - frameNum; }
          else if ( cell.hasClassName("end")   ) { cell.innerHTML = this._dl - 3 * c + 5 - frameNum; }
        }

        cell.removeClassName("aa")
            .addClassName("dna");

      }.bind(this) );

      // flip the class names on the switch to record which type of coordinates
      // we're currently showing
      switchEl.removeClassName("aa")
              .addClassName("dna");

    } else if ( switchEl.hasClassName("dna") ) {
      // convert DNA- to protein-coordinates
      $$("td.dna").each( function( cell ) {

        var frameNum = Number( cell.up("tr").down("span.frameNum").innerHTML ),
            c        = Number( cell.innerHTML );

        if ( frameNum < 4 ) { // positive strand
          if      ( cell.hasClassName("start") ) { cell.innerHTML = ( c + 3 - frameNum ) / 3; }
          else if ( cell.hasClassName("end")   ) { cell.innerHTML = ( c + 1 - frameNum ) / 3; }
        } else {                // negative strand
          if      ( cell.hasClassName("start") ) { cell.innerHTML = ( this._dl - c + 7 - frameNum ) / 3; }
          else if ( cell.hasClassName("end")   ) { cell.innerHTML = ( this._dl - c + 5 - frameNum ) / 3; }
        }

        cell.removeClassName("dna")
            .addClassName("aa");

      }.bind(this) );

      switchEl.removeClassName("dna")
              .addClassName("aa");
    }
  },

  //----------------------------------------------------------------------------
  /**
   * Shows or hides the alignment rows
   *
   * @private
   * @param {Event} e mouse click event on the show/hide button
   */
  _toggleAlignment: function( e ) {

    // get the clicked table cell
    var cell = e.findElement( "td" );

    // the button inner in that cell
    var btn = cell.down("span");

    // walk up the DOM and onto the next row in the table
    var nextRow = cell.up("tr").next("tr");
    
    // toggle the state of the row and reset the image source to point to the
    // new image
    nextRow.toggle();
    btn.innerHTML = nextRow.visible()
                  ? btn.innerHTML.replace("Show","Hide")
                  : btn.innerHTML.replace("Hide","Show");
  },
  
  //----------------------------------------------------------------------------
  /**
   * Shows or hides all of the alignments in one fell swoop...
   *
   * @private
   * @param {Event} e mouse click event on the "show/hide all" link
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

    // set the text in the show/hide buttons for the toggled rows
    tbody.select("td.showSwitch span").each( function( btn ) {
      btn.innerHTML = ( show == null )
                    ? btn.innerHTML.replace("Hide","Show")
                    : btn.innerHTML.replace("Show","Hide");
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
    var bHits = details.B;// || {};
    var s = "";
    
    if ( this._config.options.ga !== undefined ) {
    
      s += "We found <strong>" + aHits.numSignificantMatches +
           "</strong> Pfam-A match" + ( aHits.numSignificantMatches > 1 ? "es" : "" ) +
           " to your search sequence"; 

    } else {
    
      if ( aHits !== undefined && aHits.total > 0 ) {
  
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
    
    if ( bHits !== undefined && bHits.total > 0 ) {

      if ( aHits !== undefined && aHits.total > 0 ) {
        s += " and ";
      } else {
        s += " but we did find ";
      }      
      s += "<strong>" + bHits.total + "</strong> Pfam-B match" +
           ( bHits.total > 1 ? "es" : "" ) + ".";
      
    } else if ( bHits !== undefined && ! ( bHits.total > 0 ) ) {
      
      if ( aHits !== undefined && aHits.total > 0 ) {
        s += " but we did not find any Pfam-B matches.";
      } else if ( aHits !== undefined && ! ( aHits.total > 0 ) ) {
        s += " nor any Pfam-B matches.";
      }
      
    } else {
      s += ". You did not choose to search for Pfam-B matches.";
    }
    
    if ( this._config.options.ga !== undefined ) {
      s += " Because you chose to show only hits that score above the " +
           "gathering threshold, there are no <em>in</em>significant " +
           "Pfam-A hits.";
    }

    this._summaryTextEl.update( s );
  }

  //----------------------------------------------------------------------------

} );
