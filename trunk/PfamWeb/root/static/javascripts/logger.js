
// logger.js
// jt6 20070416 WTSI
//
// A simple status message scroller.
//
// $Id: logger.js,v 1.1 2007-04-16 15:49:18 jt6 Exp $

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

var Logger = Class.create();

Logger.prototype = {
  
  initialize: function() {
    console.debug( "Logger.initialize: added logger" );
  },

  log: function( msg, caller ) {
    
    console.debug( "Logger.log: adding \"" + msg + "\"" );

    // where are we putting the new message    
    var messages = $A( $("logScroller").childNodes );
    var numMsgs = messages.size();
    var last = $A( $("logScroller").childNodes ).last();

    // build the markup
    var msgEl = document.createElement( "span" );
    msgEl.setAttribute( "id", "msg" + numMsgs );
    msgEl.setAttribute( "class", "msg" );
    msgEl.update( msg );
    $("logScroller").appendChild( msgEl );

    // was there a caller specified ? If so, add it to the message
    if( ! ( caller === undefined ) ) {
      console.debug( "Logger.log: msg from caller \"" + caller + "\"" );
      new Insertion.Top( "msg" + numMsgs, 
                         '<span class="caller">' + caller + ':&nbsp;</span>' );
    }

    // scroll the logger to make the most recent message appear
    $("logScroller").scrollTop = $("logScroller").scrollHeight;
  },
  
};
