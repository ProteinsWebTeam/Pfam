
// a hacked version of the Ajax.PeriodicalUpdater. This one uses an
// Ajax.Request, rather than an Ajax.Updater, so that we get control of
// what gets updated in the page, and when. 
//
// jt6 20090803 WTSI
//
// $Id: updater.js,v 1.1 2009-09-04 13:02:00 jt6 Exp $
// 
// Copyright (c) 2009: Genome Research Ltd.
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

// for the benefit of jslint, declare global variables from outside this script
/*global $, Class, console */

var Updater = Class.create( {
  initialize: function( url, options ) {

    // console.debug( "Updater.initialize: initializing..." );

    this.options = {
      method:       'post',
      asynchronous: true,
      contentType:  'application/x-www-form-urlencoded',
      encoding:     'UTF-8',
      parameters:   { },
      evalJSON:     true,
      evalJS:       true
    };
    Object.extend( this.options, options || { } );

    this.onComplete = this.options.onComplete;

    this.frequency  = ( this.options.frequency || 2 );
    this.decay      = ( this.options.decay || 1 );
    this.updater    = { };
    this.url        = url;

    // console.debug( "Updater.initialize: starting..." );
    this.start();
  },

  start: function() {
    this.options.onComplete = this.updateComplete.bind(this);
    this.onTimerEvent();
    // console.debug( "Updater.initialize: started" );
  },

  stop: function() {
    this.updater.options.onComplete = undefined;
    clearTimeout(this.timer);
    ( this.onComplete || Prototype.emptyFunction ).apply( this, arguments );
  },

  updateComplete: function( response ) {
    // console.debug( "Updater.updateComplete: response status: %d", response.status );
    if ( this.options.decay ) {
      this.decay = (response.responseText == this.lastText ?
        this.decay * this.options.decay : 1);

      this.lastText = response.responseText;
    }
    this.timer = this.onTimerEvent.bind(this).delay( this.decay * this.frequency );
  },

  onTimerEvent: function() {
    this.updater = new Ajax.Request( this.url, this.options );
  }
});
