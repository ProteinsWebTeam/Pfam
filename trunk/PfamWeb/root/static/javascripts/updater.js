
// a hacked version of the Ajax.PeriodicalUpdater. This one uses an
// Ajax.Request, rather than an Ajax.Updater, so that we get control of
// what gets updated in the page, and when. 
//
// jt6 20090803 WTSI
//
// $Id: updater.js,v 1.1 2009-09-04 13:02:00 jt6 Exp $

// for the benefit of jslint, declare global variables from outside this script
/*global $, Class, console */

var Updater = Class.create( {
  initialize: function( url, options ) {

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

    this.start();
  },

  start: function() {
    this.options.onComplete = this.updateComplete.bind(this);
    this.onTimerEvent();
  },

  stop: function() {
    this.updater.options.onComplete = undefined;
    clearTimeout(this.timer);
    ( this.onComplete || Prototype.emptyFunction ).apply( this, arguments );
  },

  updateComplete: function( response ) {
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
