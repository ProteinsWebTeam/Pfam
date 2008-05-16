
// history.jt
// jt6 20080404 WTSI
//
// $Id: history.js,v 1.1 2008-05-16 14:58:22 jt6 Exp $

var Switcher = Class.create( {

  //----------------------------------------------------------------------------

  // the maximum number of breadcrumbs that are allowed in a trail
  maxNumBreadcrumbs: 5,

  initialize: function( sSection, sAcc ) {
    console.debug( "Switcher::initialize: ----------------------------------" );
    console.debug( "Switcher::initialize: initialising" );
    this.section = sSection;
    this.acc     = sAcc || "";

    var trail = this.getBreadcrumbs();
    if ( trail.size() ) {
      console.debug( "Switcher.initialize: initialising history items from cookie" );
      trail.reverse().each( function ( crumb ) {
        console.debug( "Switcher::initialize: adding history item for |" + crumb + "|" );
        dsHistory.addFunction( this.switchTab, this, crumb );
      } );
    } else {
      console.debug( "Switcher.initialize: no history; start from first tab" );
      dsHistory.addFunction( this.firstTab, this );
    }
  },

  //----------------------------------------------------------------------------

  chooseTab: function() {
    console.debug( "Switcher::chooseTab: choosing the best tab..." );

    if( $$('div.error').size() ) {
      console.debug( "window: switching to first error tab" );
      $$('.error')
        .first()
        .ancestors()
        .each( function( el ) {
                 if( el.hasClassName( "block" ) ) { 
                   s.forceTab( el.id );
                 }
               }
             );
    }

    // the server wants us to select a particular tab  
    else if ( showTab ) {
      console.debug( "window: switching to server-nominated tab" );
      s.forceTab( showTab );
    }

/*
    // there is a cookie; use that to initialise the history
    else if ( s.getBreadcrumbs().size() ) {
      console.debug( "window: switching to history tab" );
      s.forceTab( s.getBreadcrumbs().last() );
    }
 */
        
    //no tab specified; show the first one
    else {
      console.debug( "window: switching to first tab" );
      s.firstTab();
    }

  },

  //----------------------------------------------------------------------------

  firstTab: function() {
    console.debug( "Switcher::firstTab: switching to first tab" );
    this._switchTab( $$(".block").first().id );
  },

  //----------------------------------------------------------------------------

  forceTab: function( sId ) {
    if ( sId && $(sId) ) {
      console.debug( "Switcher::forceTab: switching to specified tab" );
      this._switchTab( sId );
    } else {
      console.error( "Switcher::forceTab: specified tab doesn't exist ("
                     + sId + "); switching to first" );
      this.firstTab();
    }
  },

  //----------------------------------------------------------------------------

  switchTab: function( sId, oHistory ) {

    if ( ! oHistory || ! oHistory.calledFromHistory ) {
      console.debug( "Switcher::switchTab: not called from history" );
      dsHistory.addFunction( this.switchTab, this, sId );
      this.addBreadcrumb( sId );
    } else {
      console.debug( "Switcher::switchTab: called from history" );
    }
  
    this.forceTab( sId );  
  },
  
  //----------------------------------------------------------------------------

  _switchTab: function( sId ) {
  
    if ( ! ( sId && $(sId) ) ) {
      console.error( "Switcher::_switchTab: invalid tab ID: |" + sId + "|" );
      return;
    }
    console.debug( "Switcher::_switchTab: switching to tab |" + sId + "|" );

    // show/hide the blocks themselves
    $$("#content div.block").each( function( block ) {
                                     if( sId == block.id ) {
                                       block.setStyle( { display: "block" } );
                                     } else {
                                       block.hide();
                                     }
                                   } );
  
    // set the appropriate selector in the sidebar
    $$("#sidebar li").each( function( item ) {
                              if( sId+"Selector" == item.id ) {
                                item.addClassName( "selected" );
                              } else {
                                item.removeClassName( "selected" );
                              }
                            } );
  },

  //----------------------------------------------------------------------------
  // adds the specified breadcrumb to the breadcrumbs for this section/accession

  addBreadcrumb: function( sId ) {
    console.debug( "Switcher::addBreadcrumb: adding |" + sId + "|" );

    var trail = this.getBreadcrumbs();
    console.debug( "Switcher::addBreadcrumb: trail has |" + trail.size() + "| crumbs" );

    if ( trail.last() != sId ) {
      console.debug( "Switcher::addBreadcrumb: added new crumb to trail" );
      trail.push( sId );
    } else {
      console.debug( "Switcher::addBreadcrumb: not adding repeated crumb" );
    }

    this.setBreadcrumbs( trail );
    console.debug( "Switcher::addBreadcrumb: set new breadcrumbs" );
  },

  //----------------------------------------------------------------------------
  // retrieves the breadcrumbs for this section and accession

  getBreadcrumbs: function() {
    console.debug( "Switcher::getBreadcrumbs: looking for trail in cookie" );

    var history = this.getHistory();
    if ( ! history ) {
      console.debug( "Switcher::getBreadcrumbs: found empty history" );
      return [];
    }

    var key = this.section + this.acc;
    console.debug( "Switcher::setBreadcrumbs: looking for trail with key |" + key + "|" );

    var trail = history[key] || [];
    trail.each( function( crumb ) {
      console.debug("Switcher::getBreadcumbs: crumb: |" + crumb + "|" );
    } );

    return trail;
  },
  
  //----------------------------------------------------------------------------
  // sets the breadcrumbs for this section and accession

  setBreadcrumbs: function( trail ) {
    console.debug( "Switcher::setBreadcrumbs: setting trail in cookie" );

    if ( ! trail ) {
      console.warn( "Switcher::setBreadcrumbs: empty trail" );
      return;
    }
    console.debug( "Switcher::setBreadcrumbs: trail has |" + trail.size() + "| crumbs" );

    if ( trail.size() > this.maxNumBreadcrumbs ) {
      console.debug( "Switcher::setBreadcrumbs: trail is too long; truncating" );
      while( trail.size() > this.maxNumBreadcrumbs ) {
        trail.shift();
      }
    }

    var key = this.section + this.acc;
    console.debug( "Switcher::setBreadcrumbs: setting with key |" + key + "|" );

    var history = this.getHistory() || {};
    history[key] = trail;
    var sHistory = Object.toJSON( history );

    createCookie( "tabHistory", escape( sHistory ), "1d", window.location.pathname );
    console.debug( "Switcher::setBreadcrumbs: created new cookie" );
  },
  
  //----------------------------------------------------------------------------
  // retrieves all history information from the cookie

  getHistory: function() {
    console.debug( "Switcher::getHistory: looking for history in cookie" );

    var sHistory = unescape( readCookie( "tabHistory" ) );
    console.debug( "Switcher::getHistory: sHistory: |" + sHistory + "|" );

    if ( ! sHistory ) {
      console.debug( "Switcher::getHistory: didn't find history in a cookie" );
      return;
    }

    console.debug( "Switcher::getHistory: found history in a cookie" );
    var history;
    try {
      history = sHistory.evalJSON(true);
    } catch(e) {
      console.error( "Switcher::getHistory: failed to get JSON from cookie" );
      return;
    }

    console.debug( "Switcher::getHistory: parsed history string" );
    return history;
  }
  
} );
