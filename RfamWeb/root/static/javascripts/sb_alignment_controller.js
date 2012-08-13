
//------------------------------------------------------------------------------
//- preamble -------------------------------------------------------------------
//------------------------------------------------------------------------------

// for the benefit of jslint, declare global variables from outside this script
/*global $, Class, console, Hash, document, window */

// spoof a console, if necessary, so that we can run in IE (<8) without having
// to entirely disable debug messages
if ( ! window.console ) {
  window.console     = {};
  window.console.log = function() {};
}  

//------------------------------------------------------------------------------
//- class ----------------------------------------------------------------------
//------------------------------------------------------------------------------

// A javascript library to handle the submission and retrieval of an alignment
// of sequences selected in the sunburst.
//
// jt6 20120711 WTSI
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

var AlignmentController = Class.create( {
  /**
   * @lends AlignmentController#
   * @author John Tate
   */
  //----------------------------------------------------------------------------
  //- class variables ----------------------------------------------------------
  //----------------------------------------------------------------------------

  // none

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
   * var ac = new AlignmentController( baseURL, jobId );</pre>
   * </code>
   *
   */
  initialize: function( baseURL, jobId ) {

    // the URL for the page that's handling this job
    this._baseURL = baseURL;

    // the ID for the alignment job
    this._jobId = jobId;

    this._submitAlignment();
  },

  //----------------------------------------------------------------------------
  //- private methods ----------------------------------------------------------
  //----------------------------------------------------------------------------

  _submitAlignment: function() {
    var r = new Ajax.Request( 
      this._baseURL,
      {
        method: "post",
        postBody: JSON.stringify( { jobId: this._jobId } ),
        contentType: "application/json",
        onSuccess: this._pollAlignment.bind(this),
        onFailure: this._submissionFailed.bind(this)
      }
    );
  },

  //----------------------------------------------------------------------------

  _pollAlignment: function( response ) {
    console.debug( "job submission succeeded with 201 status" );

    this._alignmentURI = response.getHeader('Location');

    this._updater = new Updater(
      this._alignmentURI,
      {
        method:         'head',
        requestHeaders: { 'Content-type': 'application/json' },
        frequency:      1,
        decay:          1.2,
        on204: this._align204.bind(this),
        onSuccess: this._alignSuccess.bind(this),
        onFailure: this._alignFailure.bind(this)
      }
    );
  
    console.debug( "started updater; polling" );
  },

  //----------------------------------------------------------------------------

  _submissionFailed: function( response ) {
    console.debug( "submission failed: ", response );
  },

  //----------------------------------------------------------------------------
  
  _align204: function( response ) {
    console.debug( 'AlignmentController._align204: job %s, status 204; no results; polling further',
      this._jobId );
  },

  _alignSuccess: function( response ) {
    console.debug( 'AlignmentController._alignSuccess: alignment generated: ', response );
    this._updater.stop();

    console.debug( 'AlignmentController._alignSuccess: setting window.location = "%s"',
                   this._alignmentURI );

    window.location = this._alignmentURI;
  },

  _alignFailure: function( response ) {
    console.debug( 'AlignmentController._alignFailure: alignment failed' );
    this._updater.stop();
  }

  //----------------------------------------------------------------------------

} );

